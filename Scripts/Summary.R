# ==================================================
# Week 11 Data Science Jobs Analysis Project
# Script: Summary.R
# Purpose: Bring together the main findings in a simple final summary
#          so the project is easy to review before presentation.
# Inputs:  Results objects created in earlier scripts
# Outputs: final_project_summary_table
#          project_cautions_table
#          slide_order_table
# ==================================================

if (!exists("jobs_data")) {
  jobs_data <- readr::read_csv(clean_data_path, show_col_types = FALSE)
}

if (!exists("project_title")) {
  project_title <- "Week 11 Data Science Jobs Analysis Project"
}

top_job_count <- jobs_data %>%
  count(job_simp, sort = TRUE) %>%
  slice(1)

top_job_label <- clean_label_text(top_job_count$job_simp)

t_test_p_value <- t_test_results_table$p_value[1]
t_test_difference <- t_test_results_table$mean_difference[1]
t_test_effect_size <- t_test_results_table$cohens_d[1]

hypothesis_result_text <- if (t_test_p_value < 0.05) {
  paste0(
    "Same-state postings were ",
    format_number(abs(t_test_difference), 2),
    " thousand USD ",
    ifelse(t_test_difference > 0, "higher", "lower"),
    " on average, with a small effect size (Cohen's d = ",
    format_number(t_test_effect_size, 2),
    ")."
  )
} else {
  paste0(
    "The Welch t-test did not show a statistically reliable salary difference ",
    "between same-state groups (p = ",
    format_p_value(t_test_p_value),
    ")."
  )
}

regression_result_text <- paste0(
  "The linear regression remained weak overall (R-squared = ",
  format_number(regression_fit_table$r_squared[1], 3),
  ", adjusted R-squared = ",
  format_number(regression_fit_table$adjusted_r_squared[1], 3),
  "), so it should be treated as descriptive."
)

classification_accuracy <- classification_metrics_table %>%
  filter(class == "overall_accuracy") %>%
  pull(precision)

majority_class_baseline <- classification_metrics_table %>%
  filter(class == "majority_class_baseline") %>%
  pull(precision)

lowest_recall_class <- classification_metrics_table %>%
  filter(!class %in% c("overall_accuracy", "majority_class_baseline")) %>%
  filter(recall == min(recall, na.rm = TRUE)) %>%
  slice(1) %>%
  pull(class)

classification_result_text <- paste0(
  "The decision tree reached ",
  format_number(classification_accuracy, 3),
  " test accuracy, only slightly above the majority-class baseline of ",
  format_number(majority_class_baseline, 3),
  ". The weakest recall was for ",
  lowest_recall_class,
  "."
)

cluster_salary_order <- cluster_profiles_table %>%
  arrange(desc(mean_avg_salary))

highest_salary_cluster <- cluster_salary_order %>%
  slice(1)

cluster_skill_differences <- cluster_profiles_table %>%
  select(cluster, hadoop_rate, spark_rate, aws_rate, big_data_rate, python_rate) %>%
  pivot_longer(
    cols = -cluster,
    names_to = "skill",
    values_to = "rate"
  ) %>%
  group_by(skill) %>%
  summarise(rate_difference = max(rate) - min(rate), .groups = "drop") %>%
  arrange(desc(rate_difference)) %>%
  slice_head(n = 2)

key_cluster_skills <- gsub("_rate", "", cluster_skill_differences$skill)
key_cluster_skills <- gsub("_", " ", key_cluster_skills)

clustering_result_text <- paste0(
  "The final k-means solution used k = ",
  final_k,
  ". Cluster ",
  highest_salary_cluster$cluster,
  " had the higher mean salary at ",
  format_number(highest_salary_cluster$mean_avg_salary, 1),
  " thousand USD, but the clearer separation came from differences in ",
  paste(key_cluster_skills, collapse = " and "),
  "."
)

final_project_summary_table <- tibble::tibble(
  section = c(
    "Project title",
    "Dataset overview",
    "Hypothesis test",
    "Linear regression",
    "Classification tree",
    "K-means clustering",
    "Final takeaway"
  ),
  summary = c(
    project_title,
    paste0(
      nrow(jobs_data),
      " cleaned job postings were analysed. The largest main job group was ",
      top_job_label,
      " with ",
      top_job_count$n,
      " postings."
    ),
    hypothesis_result_text,
    regression_result_text,
    classification_result_text,
    clustering_result_text,
    "The project gives a clear overview of salary, role, and skill patterns, but the weak regression fit, class imbalance, and exploratory clustering mean the results should be interpreted cautiously."
  )
)

project_cautions_table <- tibble::tibble(
  caution = c(
    "The salary difference between same-state groups is statistically detectable but small in practical size.",
    "The regression model explains only a small share of salary variation.",
    "The classification tree is affected by class imbalance, especially for smaller classes.",
    "The clusters are exploratory groupings rather than fixed job types."
  )
)

slide_order_table <- tibble::tibble(
  slide_number = 1:7,
  slide_focus = c(
    "Project aim and dataset overview",
    "Descriptive figures for salary and job groups",
    "Hypothesis test result",
    "Linear regression result and diagnostics",
    "Decision tree result and limitations",
    "K-means cluster result and interpretation",
    "Conclusion, cautions, and recommendations"
  )
)

print("")
print("Final project summary:")
print(final_project_summary_table)

print("")
print("Main cautions:")
print(project_cautions_table)

print("")
print("Suggested slide order:")
print(slide_order_table)

print("")
print("Summary review completed.")
