library(tidyverse)
library(tidymodels)
library(workflowsets)
library(discrim)     
library(baguette)    
library(kknn)        
library(kernlab)     
library(ranger)     
library(xgboost)     
library(C50)         
library(glmnet)      
library(nnet)        
library(DBI)
library(RSQLite)
library(doFuture)
plan(multisession, workers = parallel::detectCores() - 1)
options(future.rng.onMisuse = "ignore")
install.packages("naivebayes")
library(naivebayes)

nb_wf     <- default_set %>% extract_workflow("base_naive_bayes")
nb_result <- fit_resamples(nb_wf, resamples = cv_folds,
                           metrics = metric_set(accuracy, f_meas, roc_auc))
collect_metrics(nb_result)

set.seed(42)

# ---- 1. Retrieve data FROM the database (not directly from CSV) ----
con <- dbConnect(RSQLite::SQLite(), "unsw_project.db")
train_processed <- dbReadTable(con, "train_data")
test_processed  <- dbReadTable(con, "test_data")
dbDisconnect(con)

train_processed <- train_processed %>% mutate(attack_cat = factor(attack_cat))
test_processed  <- test_processed  %>% mutate(attack_cat = factor(attack_cat))

cat("Train rows:", nrow(train_processed), " | Test rows:", nrow(test_processed), "\n")

# ---- 2. Stratified subsample for CV/tuning + for SVM/KNN's full lifecycle ----
# Reason: SVM (~O(n^2)) and KNN (distance to every point at predict time)
# do not scale to ~240K rows in reasonable time. All 13 models are
# compared on this SAME subsample during CV so the comparison is fair;
# scalable models are then refit on the FULL training set afterward
# (step 6) for their final version, while SVM/KNN keep the subsample.
train_sub <- train_processed %>%
  group_by(attack_cat) %>%
  slice_sample(prop = 0.15) %>%
  ungroup()

cat("Subsample rows for CV/tuning:", nrow(train_sub), "\n")
print(table(train_sub$attack_cat))

# ---- 3. Recipe: feature selection on top of Phase 1's encoding/normalization ----
# EDA (Phase 1) flagged high correlation among ct_* connection-history
# features - step_corr() removes redundant numeric predictors above a
# 0.9 correlation threshold, directly addressing that finding here.
base_recipe <- recipe(attack_cat ~ ., data = train_sub) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9)

# ---- 4. Cross-validation folds (on the subsample, stratified) ----
cv_folds <- vfold_cv(train_sub, v = 5, strata = attack_cat)

# ---- 5. Model specifications (13 algorithms) ----

# -- Tuned models (5) --
spec_glmnet <- multinom_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>% set_mode("classification")

spec_rf <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
  set_engine("ranger", importance = "impurity") %>% set_mode("classification")

spec_xgb <- boost_tree(trees = tune(), tree_depth = tune(), learn_rate = tune()) %>%
  set_engine("xgboost") %>% set_mode("classification")

spec_knn <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>% set_mode("classification")

spec_svm_rbf <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>% set_mode("classification")

# -- Untouched / default-hyperparameter models (8) --
spec_logreg <- multinom_reg(penalty = 0, mixture = 0) %>%
  set_engine("nnet") %>% set_mode("classification")

spec_dtree <- decision_tree() %>%
  set_engine("rpart") %>% set_mode("classification")

spec_c50 <- boost_tree(trees = 15) %>%
  set_engine("C5.0") %>% set_mode("classification")

spec_bagtree <- bag_tree() %>%
  set_engine("rpart", times = 25) %>% set_mode("classification")

spec_nb <- naive_Bayes() %>%
  set_engine("naivebayes") %>% set_mode("classification")

spec_lda <- discrim_linear() %>%
  set_engine("MASS") %>% set_mode("classification")

spec_svm_linear <- svm_linear() %>%
  set_engine("kernlab") %>% set_mode("classification")

spec_mlp <- mlp(hidden_units = 10) %>%
  set_engine("nnet") %>% set_mode("classification")

# ---- 6. Build two workflow sets: tuned vs. default ----
tuned_set <- workflow_set(
  preproc = list(base = base_recipe),
  models  = list(
    glmnet  = spec_glmnet,
    rf      = spec_rf,
    xgboost = spec_xgb,
    knn     = spec_knn,
    svm_rbf = spec_svm_rbf
  )
)

default_set <- workflow_set(
  preproc = list(base = base_recipe),
  models  = list(
    logreg     = spec_logreg,
    dtree      = spec_dtree,
    c50_boost  = spec_c50,
    bag_tree   = spec_bagtree,
    naive_bayes = spec_nb,
    lda        = spec_lda,
    svm_linear = spec_svm_linear,
    mlp        = spec_mlp
  )
)

# ---- 7. Run tuning (small grids - keep runtime reasonable) ----
cat("Tuning 5 models via cross-validation... this will take a while.\n")
tuned_results <- tuned_set %>%
  workflow_map(
    fn = "tune_grid",
    resamples = cv_folds,
    grid = 8,                     
    metrics = metric_set(accuracy, f_meas, roc_auc),
    control = control_grid(save_pred = FALSE, parallel_over = "everything"),
    verbose = TRUE
  )

# ---- 8. Run default-hyperparameter models via CV (fit_resamples, no tuning) ----
cat("Fitting 8 default-hyperparameter models via cross-validation...\n")
default_results <- default_set %>%
  workflow_map(
    fn = "fit_resamples",
    resamples = cv_folds,
    metrics = metric_set(accuracy, f_meas, roc_auc),
    control = control_resamples(save_pred = FALSE, parallel_over = "everything"),
    verbose = TRUE
  )

# ---- 9. Combine and compare all 13 models by CV performance ----
default_results_clean <- default_results %>%
  filter(!wflow_id %in% c("base_lda", "base_naive_bayes"))

all_results <- bind_rows(tuned_results, default_results_clean)

comparison_table <- rank_results(all_results, rank_metric = "f_meas", select_best = TRUE) %>%
  filter(.metric == "f_meas") %>%
  select(wflow_id, .metric, mean, std_err, rank)

print(comparison_table, n = 11)
write_csv(comparison_table, "phase2_cv_comparison.csv")

# ---- 10. Finalize each workflow with its best hyperparameters (tuned models only) ----
best_glmnet  <- tuned_results %>% extract_workflow_set_result("base_glmnet")  %>% select_best(metric = "f_meas")
best_rf      <- tuned_results %>% extract_workflow_set_result("base_rf")      %>% select_best(metric = "f_meas")
best_xgb     <- tuned_results %>% extract_workflow_set_result("base_xgboost") %>% select_best(metric = "f_meas")
best_knn     <- tuned_results %>% extract_workflow_set_result("base_knn")     %>% select_best(metric = "f_meas")
best_svm_rbf <- tuned_results %>% extract_workflow_set_result("base_svm_rbf") %>% select_best(metric = "f_meas")

final_wf_glmnet  <- tuned_results %>% extract_workflow("base_glmnet")  %>% finalize_workflow(best_glmnet)
final_wf_rf      <- tuned_results %>% extract_workflow("base_rf")      %>% finalize_workflow(best_rf)
final_wf_xgb     <- tuned_results %>% extract_workflow("base_xgboost") %>% finalize_workflow(best_xgb)
final_wf_knn     <- tuned_results %>% extract_workflow("base_knn")     %>% finalize_workflow(best_knn)
final_wf_svm_rbf <- tuned_results %>% extract_workflow("base_svm_rbf") %>% finalize_workflow(best_svm_rbf)

# ---- 11. Final fit: scalable models on FULL training data, SVM/KNN on subsample ----
# (this is the point where SVM/KNN deliberately keep using train_sub - documented
# design choice, not an oversight)
fit_full  <- function(wf) fit(wf, data = train_processed)
fit_small <- function(wf) fit(wf, data = train_sub)

final_fit_glmnet  <- fit_full(final_wf_glmnet)
final_fit_rf      <- fit_full(final_wf_rf)
final_fit_xgb     <- fit_full(final_wf_xgb)
final_fit_knn     <- fit_small(final_wf_knn)       
final_fit_svm_rbf <- fit_small(final_wf_svm_rbf)   

final_fit_logreg     <- fit_full(default_set %>% extract_workflow("base_logreg"))
final_fit_dtree      <- fit_full(default_set %>% extract_workflow("base_dtree"))
final_fit_c50        <- fit_full(default_set %>% extract_workflow("base_c50_boost"))
bagtree_wf <- default_set %>% extract_workflow("base_bag_tree") %>%
  update_model(
    bag_tree() %>%
      set_engine("rpart", times = 25, control = control_bag(allow_parallel = FALSE)) %>%
      set_mode("classification")
  )
final_fit_bagtree <- fit_full(bagtree_wf)
final_fit_nb <- fit_full(nb_wf)
final_fit_svm_linear <- fit_small(default_set %>% extract_workflow("base_svm_linear"))  
final_fit_mlp        <- fit_full(default_set %>% extract_workflow("base_mlp"))

all_fitted_models <- list(
  glmnet = final_fit_glmnet, rf = final_fit_rf, xgboost = final_fit_xgb,
  knn = final_fit_knn, svm_rbf = final_fit_svm_rbf, logreg = final_fit_logreg,
  dtree = final_fit_dtree, c50_boost = final_fit_c50, bag_tree = final_fit_bagtree,
  naive_bayes = final_fit_nb, svm_linear = final_fit_svm_linear,
  mlp = final_fit_mlp
)

# ---- 12. Evaluate every model on the held-out, never-resampled TEST set ----
compute_manual_metrics <- function(preds, all_classes) {
  cm <- table(
    factor(preds$attack_cat, levels = all_classes),
    factor(preds$.pred_class, levels = all_classes)
  )
  
  per_class <- map_dfr(all_classes, function(cl) {
    tp <- cm[cl, cl]
    fp <- sum(cm[, cl]) - tp
    fn <- sum(cm[cl, ]) - tp
    
    precision_c <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
    recall_c    <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
    f1_c        <- if ((precision_c + recall_c) == 0) 0 else 2 * precision_c * recall_c / (precision_c + recall_c)
    
    tibble(class = cl, precision = precision_c, recall = recall_c, f_meas = f1_c)
  })
  
  tibble(
    .metric = c("precision", "recall", "f_meas"),
    .estimate = c(mean(per_class$precision), mean(per_class$recall), mean(per_class$f_meas))
  )
}

all_classes <- levels(test_processed$attack_cat)

test_metrics_all <- map_dfr(names(all_fitted_models), function(model_name) {
  mod <- all_fitted_models[[model_name]]
  preds <- predict(mod, new_data = test_processed) %>%
    bind_cols(test_processed %>% select(attack_cat))
  
  acc <- accuracy(preds, truth = attack_cat, estimate = .pred_class) %>% select(.metric, .estimate)
  manual <- compute_manual_metrics(preds, all_classes)
  
  bind_rows(acc, manual) %>% mutate(model = model_name)
})

test_comparison <- test_metrics_all %>%
  select(model, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(desc(f_meas))

print(test_comparison, n = 13)
write_csv(test_comparison, "phase2_test_comparison.csv")

# ---- 13. Save all fitted models + processed test set for the evaluation/viz script ----
saveRDS(all_fitted_models, "phase2_all_models.rds")
write_csv(test_processed, "phase2_test_for_viz.csv")

cat("Phase 2 modeling complete. Best model by test macro-F1:\n")
print(head(test_comparison, 3))