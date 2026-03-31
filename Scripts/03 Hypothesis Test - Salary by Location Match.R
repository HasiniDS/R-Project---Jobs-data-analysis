# ==================================================
# Week 11 Jobs Project
# Script: 03 Hypothesis Test - Salary by Location Match.R
# Purpose: Test whether average salary differs between jobs where
#          the role is in the same state as company headquarters
#          and jobs where it is not.
# Inputs:  jobs_data from Script 01 or Data/Clean/Data Science Jobs Dataset - Clean.csv
# Outputs: Table 05 Salary Summary by Same State Group.csv
#          Table 06 Welch T Test Results for Salary Difference.csv
#          Figure 07 Salary by Same State Group.png
# ==================================================

if (!exists("jobs_data")) {
  jobs_data <- readr::read_csv(clean_data_path, show_col_types = FALSE)
}

# H0: Mean average salary is the same for same_state = 1 and same_state = 0.
# H1: Mean average salary is different between same_state = 1 and same_state = 0.

# --------------------------------------------------
# Prepare the test data
# --------------------------------------------------

hypothesis_test_data <- jobs_data %>%
  mutate(
    same_state_group = factor(
      same_state,
      levels = c(1, 0),
      labels = c("Same state", "Different state")
    )
  ) %>%
  select(avg_salary, same_state_group)

hypothesis_group_summary <- hypothesis_test_data %>%
  group_by(same_state_group) %>%
  summarise(
    group_n = n(),
    mean_avg_salary = mean(avg_salary, na.rm = TRUE),
    sd_avg_salary = sd(avg_salary, na.rm = TRUE),
    .groups = "drop"
  )

write_output_table(
  hypothesis_group_summary,
  "Table 05 Salary Summary by Same State Group.csv"
)

# --------------------------------------------------
# Welch t-test and effect size
# --------------------------------------------------

t_test_object <- t.test(
  avg_salary ~ same_state_group,
  data = hypothesis_test_data,
  var.equal = FALSE
)

same_state_stats <- hypothesis_group_summary %>%
  filter(same_state_group == "Same state")

different_state_stats <- hypothesis_group_summary %>%
  filter(same_state_group == "Different state")

pooled_sd <- sqrt(
  (
    (same_state_stats$group_n - 1) * same_state_stats$sd_avg_salary^2 +
      (different_state_stats$group_n - 1) * different_state_stats$sd_avg_salary^2
  ) /
    (same_state_stats$group_n + different_state_stats$group_n - 2)
)

cohens_d <- (
  same_state_stats$mean_avg_salary - different_state_stats$mean_avg_salary
) / pooled_sd

t_test_results_table <- tibble::tibble(
  test_name = "Welch independent-samples t-test",
  comparison = "Same state minus different state",
  mean_same_state = same_state_stats$mean_avg_salary,
  mean_different_state = different_state_stats$mean_avg_salary,
  mean_difference = same_state_stats$mean_avg_salary - different_state_stats$mean_avg_salary,
  t_statistic = unname(t_test_object$statistic),
  degrees_freedom = unname(t_test_object$parameter),
  p_value = t_test_object$p.value,
  confidence_interval_low = t_test_object$conf.int[1],
  confidence_interval_high = t_test_object$conf.int[2],
  cohens_d = cohens_d
)

write_output_table(
  t_test_results_table,
  "Table 06 Welch T Test Results for Salary Difference.csv"
)

# --------------------------------------------------
# Supporting plot
# --------------------------------------------------

salary_same_state_boxplot <- ggplot(
  hypothesis_test_data,
  aes(x = same_state_group, y = avg_salary, fill = same_state_group)
) +
  geom_boxplot(alpha = 0.9, width = 0.65, outlier.alpha = 0.35) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    size = 3,
    fill = "white",
    colour = "black"
  ) +
  scale_fill_manual(values = unname(c(
    analysis_palette["primary"],
    analysis_palette["neutral"]
  ))) +
  labs(
    title = "Average Salary by Same-State Indicator",
    subtitle = "White points show the group means",
    x = "Posting location relative to headquarters",
    y = "Average salary (thousand USD)"
  ) +
  analysis_theme()

save_analysis_figure(
  salary_same_state_boxplot,
  "Figure 07 Salary by Same State Group.png"
)

cat("\nHypothesis test summary:\n")
print(hypothesis_group_summary)
print(t_test_results_table)

if (t_test_object$p.value < 0.05) {
  cat(
    "\nThere is evidence of a salary difference between the two same_state groups.\n"
  )
} else {
  cat(
    "\nThere is not enough evidence to say that average salary differs by same_state.\n"
  )
}
