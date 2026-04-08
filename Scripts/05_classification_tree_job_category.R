# ==================================================
# Week 11 Data Science Jobs Analysis Project
# Script: 05_classification_tree_job_category.R
# Purpose: Fit and evaluate a simple decision tree to classify the
#          main job categories.
# Inputs:  jobs_data from Script 01 or Data/Clean/Jobs_clean.csv
# Outputs: table_10_classification_sample_class_counts.csv
#          table_11_decision_tree_confusion_matrix.csv
#          table_12_decision_tree_performance_metrics.csv
#          figure_11_decision_tree_for_job_category_classification.png
#          Outputs/Models/classification_tree_model_job_category.rds
# ==================================================

if (!exists("jobs_data")) {
  jobs_data <- readr::read_csv(clean_data_path, show_col_types = FALSE)
}

jobs_classification_data <- add_model_fields(jobs_data) %>%
  filter(job_simp %in% selected_job_groups) %>%
  mutate(
    job_simp = factor(job_simp, levels = selected_job_groups),
    row_id = row_number()
  )

# --------------------------------------------------
# Class counts
# --------------------------------------------------

class_counts_table <- jobs_classification_data %>%
  count(job_simp, name = "count") %>%
  arrange(desc(count))

write_output_table(
  class_counts_table,
  "table_10_classification_sample_class_counts.csv"
)

# --------------------------------------------------
# Train/test split
# --------------------------------------------------

set.seed(7202)

train_ids <- jobs_classification_data %>%
  group_by(job_simp) %>%
  summarise(
    train_row_id = list(sample(row_id, size = floor(0.7 * n()))),
    .groups = "drop"
  ) %>%
  pull(train_row_id) %>%
  unlist()

train_data <- jobs_classification_data %>%
  filter(row_id %in% train_ids)

test_data <- jobs_classification_data %>%
  filter(!row_id %in% train_ids)

# --------------------------------------------------
# Fit the decision tree
# --------------------------------------------------

# avg_salary is retained because this tree is being used as a descriptive
# classification exercise on fully observed job postings. If the goal were
# to predict job category before salary is known, avg_salary should be
# removed and treated as a sensitivity check.
classification_salary_note <- paste(
  "avg_salary is included here because the classifier is describing observed",
  "differences between completed job postings.",
  "A cleaner deployment-style alternative would remove avg_salary if job type",
  "had to be predicted before salary information is available."
)

classification_tree_model <- rpart::rpart(
  job_simp ~ rating + company_age_imputed + same_state +
    python + excel + hadoop + spark + aws + tableau + big_data +
    avg_salary,
  data = train_data,
  method = "class",
  control = rpart.control(cp = 0.001, maxdepth = 6, minsplit = 8)
)

saveRDS(
  classification_tree_model,
  file.path(
    project_paths$outputs_models,
    "classification_tree_model_job_category.rds"
  )
)

# --------------------------------------------------
# Evaluation
# --------------------------------------------------

test_predictions <- predict(
  classification_tree_model,
  newdata = test_data,
  type = "class"
)

confusion_matrix <- table(
  Actual = test_data$job_simp,
  Predicted = test_predictions
)

classification_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
majority_class_accuracy <- max(prop.table(table(test_data$job_simp)))

calculate_classification_metrics <- function(confusion_matrix_object) {
  class_names <- rownames(confusion_matrix_object)

  metric_rows <- lapply(class_names, function(class_name) {
    true_positive <- confusion_matrix_object[class_name, class_name]
    predicted_total <- sum(confusion_matrix_object[, class_name])
    actual_total <- sum(confusion_matrix_object[class_name, ])

    tibble::tibble(
      class = class_name,
      precision = ifelse(predicted_total == 0, NA, true_positive / predicted_total),
      recall = ifelse(actual_total == 0, NA, true_positive / actual_total),
      support = actual_total
    )
  })

  bind_rows(metric_rows)
}

class_level_metrics <- calculate_classification_metrics(confusion_matrix)

classification_metrics_table <- bind_rows(
  tibble::tibble(
    class = "overall_accuracy",
    precision = classification_accuracy,
    recall = classification_accuracy,
    support = nrow(test_data)
  ),
  tibble::tibble(
    class = "majority_class_baseline",
    precision = majority_class_accuracy,
    recall = majority_class_accuracy,
    support = nrow(test_data)
  ),
  class_level_metrics
)

confusion_matrix_table <- as.data.frame.matrix(confusion_matrix) %>%
  tibble::rownames_to_column("actual_class")

write_output_table(
  confusion_matrix_table,
  "table_11_decision_tree_confusion_matrix.csv"
)
write_output_table(
  classification_metrics_table,
  "table_12_decision_tree_performance_metrics.csv"
)

tree_variable_importance <- tibble::tibble(
  variable = names(classification_tree_model$variable.importance),
  importance = unname(classification_tree_model$variable.importance)
) %>%
  arrange(desc(importance))

png(
  filename = file.path(
    project_paths$outputs_figures,
    "figure_11_decision_tree_for_job_category_classification.png"
  ),
  width = 11,
  height = 7,
  units = "in",
  res = 300,
  bg = "white"
)
rpart.plot::rpart.plot(
  classification_tree_model,
  type = 4,
  extra = 104,
  under = TRUE,
  fallen.leaves = TRUE,
  faclen = 0,
  varlen = 0,
  box.palette = "Blues",
  branch.lty = 1,
  branch.col = analysis_palette["charcoal"],
  shadow.col = "grey85",
  tweak = 1.12,
  main = paste(
    "Decision Tree for the Main Job Groups",
    paste0(
      "Test accuracy = ", format_number(classification_accuracy, 3),
      "; majority baseline = ", format_number(majority_class_accuracy, 3)
    ),
    sep = "\n"
  )
)
dev.off()

cat("\nClassification class counts:\n")
print(class_counts_table)
cat("\nClassification accuracy:", format_number(classification_accuracy, 3), "\n")
cat("\nClassification note:", classification_salary_note, "\n")
cat("\nClass-level metrics:\n")
print(classification_metrics_table)
cat("\nMost important tree variables:\n")
print(tree_variable_importance)
