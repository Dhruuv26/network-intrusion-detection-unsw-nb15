library(tidyverse)
library(tidymodels)
library(themis)
library(janitor)

train_raw <- read_csv("C:/Users/Dhruuv/Downloads/UNSW_NB15_training-set.csv", show_col_types = FALSE)
test_raw  <- read_csv("C:/Users/Dhruuv/Downloads/UNSW_NB15_testing-set.csv", show_col_types = FALSE)

cat("training-set.csv rows:", nrow(train_raw), "\n")
cat("testing-set.csv rows:", nrow(test_raw), "\n")

combined <- bind_rows(
  train_raw %>% mutate(source_file = "training-set"),
  test_raw  %>% mutate(source_file = "testing-set")
) %>%
  select(-id) %>%
  mutate(row_id = row_number()) %>%
  clean_names()

cat("Combined rows:", nrow(combined), "\n")
cat("Duplicate row_ids (should be 0):", sum(duplicated(combined$row_id)), "\n")

missing_report <- combined %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "column", values_to = "pct_missing") %>%
  arrange(desc(pct_missing))
print(missing_report, n = 10)

combined <- combined %>%
  mutate(
    attack_cat = str_trim(attack_cat),          
    attack_cat = factor(attack_cat),
    label      = factor(label, levels = c(0, 1), labels = c("Normal", "Attack"))
  )

cat("Class distribution (combined):\n")
print(table(combined$attack_cat))

write_csv(combined, "unsw_combined_raw.csv")

set.seed(42)
split      <- initial_split(combined, prop = 0.8, strata = attack_cat)
train_data <- training(split)
test_data  <- testing(split)

cat("Train rows:", nrow(train_data), " | Test rows:", nrow(test_data), "\n")

unsw_recipe <- recipe(
    attack_cat ~ .,
    data = train_data %>% select(-row_id, -source_file, -label)
  ) %>%
  step_novel(proto, service, state) %>%
  step_other(proto, threshold = 0.01, other = "other_proto") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

prepped_recipe <- prep(unsw_recipe, training = train_data)

train_encoded <- juice(prepped_recipe)                       
test_processed <- bake(prepped_recipe, new_data = test_data)  

cat("Class distribution BEFORE any resampling (train):\n")
print(table(train_encoded$attack_cat))

majority_class <- "Normal"
majority_count <- sum(train_encoded$attack_cat == majority_class)

tier_targets <- c(
  Worms          = 1500,   
  Shellcode      = 4000,   
  Backdoor       = 6000,   
  Analysis       = 6500,   
  DoS            = round(0.3 * majority_count),   
  Reconnaissance = round(0.3 * majority_count)    
)
print(tier_targets)

oversample_one_class <- function(data, target_class, target_count, majority_class, majority_count) {
  subset_data <- data %>%
    filter(attack_cat %in% c(majority_class, target_class)) %>%
    mutate(attack_cat = droplevels(attack_cat))

  ratio_needed <- target_count / majority_count

  smoted <- themis::smote(subset_data, "attack_cat", over_ratio = ratio_needed)

  smoted %>% filter(attack_cat == target_class)
}

oversampled_classes <- map2(
  names(tier_targets), tier_targets,
  ~ oversample_one_class(train_encoded, .x, .y, majority_class, majority_count)
) %>%
  bind_rows()

cat("Rows generated per tiered class (target counts, includes originals):\n")
print(table(oversampled_classes$attack_cat))

untouched_classes <- train_encoded %>%
  filter(!attack_cat %in% names(tier_targets))

train_processed <- bind_rows(untouched_classes, oversampled_classes) %>%
  mutate(attack_cat = factor(attack_cat))  

cat("Final class distribution AFTER tiered oversampling (train):\n")
print(table(train_processed$attack_cat))
cat("Class distribution (test, untouched - reflects real-world imbalance):\n")
print(table(test_data$attack_cat))

write_csv(train_processed, "unsw_train_processed.csv")
write_csv(test_processed, "unsw_test_processed.csv")
saveRDS(prepped_recipe, "unsw_recipe.rds")   
write_csv(enframe(tier_targets, name = "attack_cat", value = "target_count"),
          "unsw_tier_targets.csv")           