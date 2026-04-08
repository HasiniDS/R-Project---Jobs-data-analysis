# ==================================================
# Week 11 Data Science Jobs Analysis Project
# Script: 03 Hypothesis Test Salary by Location Match.R
# Purpose: Test whether average salary differs between jobs where
#          the role is in the same state as company headquarters
#          and jobs where it is not.
# Inputs:  jobs_data from Script 01 or Data/Clean/Jobs Clean.csv
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
    group_n = sum(!is.na(avg_salary)),
    mean_avg_salary = mean(avg_salary, na.rm = TRUE),
    sd_avg_salary = sd(avg_salary, na.rm = TRUE),
    standard_error = sd_avg_salary / sqrt(group_n),
    ci_low = mean_avg_salary - qt(0.975, df = group_n - 1) * standard_error,
    ci_high = mean_avg_salary + qt(0.975, df = group_n - 1) * standard_error,
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

annotation_text <- paste(
  paste0(
    "Mean difference = ",
    format_number(t_test_results_table$mean_difference, 2),
    "k"
  ),
  paste0(
    "95% CI [",
    format_number(t_test_results_table$confidence_interval_low, 2),
    ", ",
    format_number(t_test_results_table$confidence_interval_high, 2),
    "]"
  ),
  paste0(
    "Welch p = ",
    format_p_value(t_test_results_table$p_value),
    "; Cohen's d = ",
    format_number(t_test_results_table$cohens_d, 2)
  ),
  sep = "\n"
)

annotation_segment_data <- tibble::tibble(
  start_group = "Same state",
  end_group = "Different state",
  start_mean = same_state_stats$mean_avg_salary,
  end_mean = different_state_stats$mean_avg_salary
)

salary_same_state_inference_plot <- ggplot(
  hypothesis_group_summary,
  aes(x = same_state_group, y = mean_avg_salary, colour = same_state_group)
) +
  geom_segment(
    aes(
      x = start_group,
      xend = end_group,
      y = start_mean,
      yend = end_mean
    ),
    data = annotation_segment_data,
    inherit.aes = FALSE,
    colour = analysis_palette["neutral"],
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_linerange(
    aes(ymin = ci_low, ymax = ci_high),
    linewidth = 1.4,
    show.legend = FALSE
  ) +
  geom_point(
    aes(fill = same_state_group),
    shape = 21,
    size = 4.8,
    stroke = 1.1,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = paste0(format_number(mean_avg_salary, 1), "k")),
    nudge_y = 2.4,
    size = 3.6,
    colour = analysis_palette["ink"],
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      y = ci_low - 1.1,
      label = paste0("n = ", scales::comma(group_n))
    ),
    size = 3.4,
    colour = analysis_palette["charcoal"],
    show.legend = FALSE
  ) +
  annotate(
    "label",
    x = 1.5,
    y = max(hypothesis_group_summary$ci_high) + 11,
    label = annotation_text,
    fill = "white",
    colour = analysis_palette["ink"],
    linewidth = 0.25,
    size = 3.6
  ) +
  scale_colour_manual(
    values = unname(binary_palette[levels(hypothesis_group_summary$same_state_group)])
  ) +
  scale_fill_manual(
    values = unname(binary_palette[levels(hypothesis_group_summary$same_state_group)])
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(6),
    expand = expansion(mult = c(0.04, 0.22))
  ) +
  labs(
    title = "Same-State Jobs Have a Slightly Lower Mean Salary",
    subtitle = "Points show the group mean salary and lines show the 95% confidence interval for each group",
    x = "Posting location relative to headquarters",
    y = "Average salary (thousand USD)",
    caption = figure_caption(
      "The group difference is statistically detectable, but the estimated effect size is small."
    )
  ) +
  analysis_theme()

save_analysis_figure(
  salary_same_state_inference_plot,
  "Figure 07 Salary by Same State Group.png",
  width = 10.2,
  height = 6.5
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
