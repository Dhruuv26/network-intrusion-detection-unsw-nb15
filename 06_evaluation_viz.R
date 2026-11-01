library(tidyverse)
library(tidymodels)
library(vip)

theme_set(theme_minimal(base_size = 12))

all_fitted_models <- readRDS("phase2_all_models.rds")
test_processed    <- read_csv("phase2_test_for_viz.csv", show_col_types = FALSE) %>%
  mutate(attack_cat = factor(attack_cat))
test_comparison   <- read_csv("phase2_test_comparison.csv", show_col_types = FALSE)

best_model_name <- test_comparison %>% arrange(desc(f_meas)) %>% slice(1) %>% pull(model)
cat("Best model by test macro-F1:", best_model_name, "\n")
best_model <- all_fitted_models[[best_model_name]]

# ---- Viz 1: Comparative bar chart - all 13 models by test macro-F1 ----
test_comparison %>%
  mutate(model = fct_reorder(model, f_meas)) %>%
  ggplot(aes(x = model, y = f_meas, fill = model == best_model_name)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#00A87E", "FALSE" = "#8FA3BF"), guide = "none") +
  labs(title = "Model Comparison: Macro F1-Score on Held-Out Test Set",
       x = NULL, y = "Macro F1-Score")

# ---- Viz 2: Confusion matrix heatmap (best model) ----
preds_best <- predict(best_model, new_data = test_processed) %>%
  bind_cols(test_processed %>% select(attack_cat))

conf_mat_obj <- conf_mat(preds_best, truth = attack_cat, estimate = .pred_class)

autoplot(conf_mat_obj, type = "heatmap") +
  scale_fill_gradient(low = "white", high = "#00A87E") +
  labs(title = paste("Confusion Matrix -", best_model_name)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Viz 3: Per-class precision/recall/F1 (best model) - table + bar chart ----
per_class_metrics <- preds_best %>%
  conf_mat(truth = attack_cat, estimate = .pred_class) %>%
  summary(estimator = NULL) %>%
  filter(.metric %in% c("precision", "recall", "f_meas"))
print(per_class_metrics)

# ---- Viz 4: ROC curves, one-vs-rest per class (needs predicted probabilities) ----
probs_best <- predict(best_model, new_data = test_processed, type = "prob") %>%
  bind_cols(test_processed %>% select(attack_cat))

roc_data <- roc_curve(probs_best, truth = attack_cat,
                       matches("^\\.pred_"), event_level = "first")

autoplot(roc_data) +
  labs(title = paste("ROC Curves (One-vs-Rest) -", best_model_name)) +
  theme(legend.position = "right", legend.text = element_text(size = 8))

# ---- Viz 5: Precision-Recall curves, one-vs-rest per class ----
pr_data <- pr_curve(probs_best, truth = attack_cat, matches("^\\.pred_"))

autoplot(pr_data) +
  labs(title = paste("Precision-Recall Curves (One-vs-Rest) -", best_model_name)) +
  theme(legend.position = "right", legend.text = element_text(size = 8))

# ---- Viz 6: Feature importance (from Random Forest, if available) ----
if ("rf" %in% names(all_fitted_models)) {
  rf_fitted <- all_fitted_models[["rf"]] %>% extract_fit_parsnip()
  vip(rf_fitted, num_features = 15) +
    labs(title = "Top 15 Features by Importance - Random Forest")
}

cat("Evaluation & visualization complete.\n")