# ==================================================
# Jobs Analyse
# Script: 01 Data Quality Review.R
# Purpose: Load the cleaned jobs dataset and confirm that it is
#          ready for analysis.
# Inputs:  Data/Clean/Data Science Jobs Dataset - Clean.csv
# Outputs: Table 01 Dataset Structure and Quality Summary.csv
#          Table 02 Variable Missingness Summary.csv
#          Table 03 Key Category Frequency Summary.csv
# ==================================================

# --------------------------------------------------
# Load the cleaned dataset
# --------------------------------------------------

jobs_data <- readr::read_csv(clean_data_path, show_col_types = FALSE)

cat("\nDataset structure check:\n")
dplyr::glimpse(jobs_data)

cat("\nDataset dimensions:", nrow(jobs_data), "rows and", ncol(jobs_data), "columns.\n")

# Notes for interpretation:
# - job_simp is clearly imbalanced, with data scientist roles dominating.
# - seniority is sparse and is mostly recorded as "na", so it is not suitable
#   as a main modelling target.
# - rating includes 0 values, which are kept because they represent unrated firms.
# - outliers were flagged in the earlier cleaning stage rather than removed.

# --------------------------------------------------
# Overview checks
# --------------------------------------------------

exact_duplicate_rows <- sum(duplicated(jobs_data))

dataset_overview_table <- tibble::tibble(
  metric = c(
    "row_count",
    "column_count",
    "exact_duplicate_rows",
    "avg_salary_mean",
    "avg_salary_sd",
    "rating_mean",
    "rating_zero_count",
    "same_state_one_count",
    "same_state_zero_count",
    "flagged_salary_outliers"
  ),
  value = c(
    nrow(jobs_data),
    ncol(jobs_data),
    exact_duplicate_rows,
    mean(jobs_data$avg_salary, na.rm = TRUE),
    sd(jobs_data$avg_salary, na.rm = TRUE),
    mean(jobs_data$rating, na.rm = TRUE),
    sum(jobs_data$rating == 0, na.rm = TRUE),
    sum(jobs_data$same_state == 1, na.rm = TRUE),
    sum(jobs_data$same_state == 0, na.rm = TRUE),
    sum(jobs_data$avg_salary_outlier_flag, na.rm = TRUE)
  )
)

missing_values_table <- tibble::tibble(
  variable = names(jobs_data),
  missing_count = vapply(jobs_data, function(x) sum(is.na(x)), numeric(1))
) %>%
  mutate(
    missing_percent = 100 * missing_count / nrow(jobs_data)
  ) %>%
  arrange(desc(missing_count), variable)

key_category_counts_table <- bind_rows(
  jobs_data %>%
    count(job_simp, name = "count") %>%
    mutate(variable = "job_simp", category = job_simp) %>%
    select(variable, category, count),
  jobs_data %>%
    count(sector, name = "count") %>%
    mutate(variable = "sector", category = sector) %>%
    select(variable, category, count),
  jobs_data %>%
    count(seniority, name = "count") %>%
    mutate(variable = "seniority", category = seniority) %>%
    select(variable, category, count),
  jobs_data %>%
    mutate(same_state_label = if_else(same_state == 1, "same_state", "different_state")) %>%
    count(same_state_label, name = "count") %>%
    mutate(variable = "same_state", category = same_state_label) %>%
    select(variable, category, count)
) %>%
  arrange(variable, desc(count))

write_output_table(
  dataset_overview_table,
  "Table 01 Dataset Structure and Quality Summary.csv"
)
write_output_table(
  missing_values_table,
  "Table 02 Variable Missingness Summary.csv"
)
write_output_table(
  key_category_counts_table,
  "Table 03 Key Category Frequency Summary.csv"
)

cat("\nExact duplicate rows:", exact_duplicate_rows, "\n")
cat("Missing company_age values:", sum(is.na(jobs_data$company_age)), "\n")
cat("Top job_simp counts:\n")
print(jobs_data %>% count(job_simp, sort = TRUE))

jobs_analysis_data <- add_model_fields(jobs_data)

cat("\nThe cleaned analysis dataset is ready for the Week 11 tasks.\n")
