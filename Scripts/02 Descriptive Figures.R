# ==================================================
# Jobs Analyse
# Script: 02 Descriptive Figures.R
# Purpose: Create descriptive summaries and clean visualisations
#          that support the later modelling choices.
# Inputs:  jobs_data from Script 01 or Data/Clean/Data Science Jobs Dataset - Clean.csv
# Outputs: Table 04 Descriptive Statistics for Main Variables.csv
#          Figure 01 Average Salary Distribution.png
#          Figure 02 Average Salary by Main Job Group.png
#          Figure 03 Company Rating and Average Salary.png
#          Figure 04 Frequency of Main Job Groups.png
#          Figure 05 Company Age Distribution.png
#          Figure 06 Average Salary by Same State Indicator.png
# ==================================================

if (!exists("jobs_data")) {
  jobs_data <- readr::read_csv(clean_data_path, show_col_types = FALSE)
}

if (!exists("selected_job_groups")) {
  selected_job_groups <- c("data scientist", "analyst", "data engineer", "mle")
}

# --------------------------------------------------
# Descriptive summary
# --------------------------------------------------

descriptive_summary_table <- tibble::tibble(
  variable = c("avg_salary", "rating", "company_age"),
  non_missing_n = c(
    sum(!is.na(jobs_data$avg_salary)),
    sum(!is.na(jobs_data$rating)),
    sum(!is.na(jobs_data$company_age))
  ),
  mean = c(
    mean(jobs_data$avg_salary, na.rm = TRUE),
    mean(jobs_data$rating, na.rm = TRUE),
    mean(jobs_data$company_age, na.rm = TRUE)
  ),
  median = c(
    median(jobs_data$avg_salary, na.rm = TRUE),
    median(jobs_data$rating, na.rm = TRUE),
    median(jobs_data$company_age, na.rm = TRUE)
  ),
  sd = c(
    sd(jobs_data$avg_salary, na.rm = TRUE),
    sd(jobs_data$rating, na.rm = TRUE),
    sd(jobs_data$company_age, na.rm = TRUE)
  ),
  minimum = c(
    min(jobs_data$avg_salary, na.rm = TRUE),
    min(jobs_data$rating, na.rm = TRUE),
    min(jobs_data$company_age, na.rm = TRUE)
  ),
  maximum = c(
    max(jobs_data$avg_salary, na.rm = TRUE),
    max(jobs_data$rating, na.rm = TRUE),
    max(jobs_data$company_age, na.rm = TRUE)
  )
)

write_output_table(
  descriptive_summary_table,
  "Table 04 Descriptive Statistics for Main Variables.csv"
)

# --------------------------------------------------
# Plot data preparation
# --------------------------------------------------

job_type_plot_data <- jobs_data %>%
  filter(job_simp %in% selected_job_groups) %>%
  mutate(job_simp = factor(job_simp, levels = selected_job_groups))

# Only the four most common meaningful job groups are kept in the
# job-type plots so that the comparisons remain readable and useful.

job_type_count_data <- jobs_data %>%
  mutate(
    job_simp_display = if_else(job_simp == "na", "unspecified", job_simp)
  ) %>%
  count(job_simp_display, sort = TRUE) %>%
  slice_head(n = 5) %>%
  mutate(job_simp_display = forcats::fct_reorder(job_simp_display, n))

same_state_plot_data <- jobs_data %>%
  mutate(
    same_state_label = factor(
      same_state,
      levels = c(0, 1),
      labels = c("Different state", "Same state")
    )
  )

# --------------------------------------------------
# Plots
# --------------------------------------------------

salary_histogram_plot <- ggplot(jobs_data, aes(x = avg_salary)) +
  geom_histogram(
    bins = 25,
    fill = analysis_palette["primary"],
    colour = "white",
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Average Salary",
    x = "Average salary (thousand USD)",
    y = "Number of job postings"
  ) +
  analysis_theme()

salary_by_job_type_plot <- ggplot(
  job_type_plot_data,
  aes(x = job_simp, y = avg_salary, fill = job_simp)
) +
  geom_boxplot(alpha = 0.9, width = 0.7, outlier.alpha = 0.35) +
  scale_fill_manual(values = unname(c(
    analysis_palette["primary"],
    analysis_palette["secondary"],
    analysis_palette["accent"],
    analysis_palette["neutral"]
  ))) +
  labs(
    title = "Average Salary by Main Job Type",
    subtitle = "Restricted to the four most common meaningful job groups",
    x = "Job type",
    y = "Average salary (thousand USD)"
  ) +
  analysis_theme()

rating_vs_salary_plot <- ggplot(jobs_data, aes(x = rating, y = avg_salary)) +
  geom_point(
    colour = analysis_palette["primary"],
    alpha = 0.55,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = analysis_palette["accent"],
    linewidth = 0.9
  ) +
  labs(
    title = "Company Rating and Average Salary",
    x = "Company rating",
    y = "Average salary (thousand USD)"
  ) +
  analysis_theme()

job_type_counts_plot <- ggplot(job_type_count_data, aes(x = job_simp_display, y = n)) +
  geom_col(fill = analysis_palette["secondary"], width = 0.7) +
  coord_flip() +
  labs(
    title = "Most Common Job Type Categories",
    x = "Job type",
    y = "Number of job postings"
  ) +
  analysis_theme()

company_age_histogram_plot <- ggplot(
  jobs_data %>% filter(!is.na(company_age)),
  aes(x = company_age)
) +
  geom_histogram(
    bins = 25,
    fill = analysis_palette["secondary"],
    colour = "white",
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Company Age",
    x = "Company age (years)",
    y = "Number of job postings"
  ) +
  analysis_theme()

salary_by_same_state_plot <- ggplot(
  same_state_plot_data,
  aes(x = same_state_label, y = avg_salary, fill = same_state_label)
) +
  geom_boxplot(alpha = 0.9, width = 0.65, outlier.alpha = 0.35) +
  scale_fill_manual(values = unname(c(
    analysis_palette["neutral"],
    analysis_palette["primary"]
  ))) +
  labs(
    title = "Average Salary by Same-State Indicator",
    x = "Posting location relative to headquarters",
    y = "Average salary (thousand USD)"
  ) +
  analysis_theme()

save_analysis_figure(
  salary_histogram_plot,
  "Figure 01 Average Salary Distribution.png"
)
save_analysis_figure(
  salary_by_job_type_plot,
  "Figure 02 Average Salary by Main Job Group.png"
)
save_analysis_figure(
  rating_vs_salary_plot,
  "Figure 03 Company Rating and Average Salary.png"
)
save_analysis_figure(
  job_type_counts_plot,
  "Figure 04 Frequency of Main Job Groups.png"
)
save_analysis_figure(
  company_age_histogram_plot,
  "Figure 05 Company Age Distribution.png"
)
save_analysis_figure(
  salary_by_same_state_plot,
  "Figure 06 Average Salary by Same State Indicator.png"
)

cat("\nVisual exploration outputs saved.\n")
