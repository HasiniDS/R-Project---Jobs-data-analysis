# ==================================================
# Week 11 Data Science Jobs Analysis Project
# Script: 04_linear_regression_average_salary.R
# Purpose: Fit and evaluate one linear regression model for
#          average salary.
# Inputs:  jobs_data from Script 01 or Data/Clean/Jobs_clean.csv
# Outputs: table_07_linear_regression_coefficient_estimates.csv
#          table_08_linear_regression_model_fit_statistics.csv
#          table_09_variance_inflation_factor_summary.csv
#          figure_08_regression_residuals_versus_fitted_values.png
#          figure_09_regression_normal_qq_plot.png
#          figure_10_regression_residual_distribution.png
#          Outputs/Models/linear_regression_model_average_salary.rds
# ==================================================

if (!exists("jobs_data")) {
  jobs_data <- readr::read_csv(clean_data_path, show_col_types = FALSE)
}

jobs_regression_data <- add_model_fields(jobs_data) %>%
  mutate(
    job_simp_grouped = forcats::fct_relevel(job_simp_grouped, "data scientist")
  )

# --------------------------------------------------
# Fit the model
# --------------------------------------------------

regression_model <- lm(
  avg_salary ~ rating + company_age_imputed + same_state +
    python + excel + hadoop + spark + aws + tableau + big_data +
    job_simp_grouped,
  data = jobs_regression_data
)

regression_model_summary <- summary(regression_model)
regression_coefficients_table <- broom::tidy(
  regression_model,
  conf.int = TRUE
)

regression_fit_table <- broom::glance(regression_model) %>%
  transmute(
    n_observations = nobs,
    r_squared = r.squared,
    adjusted_r_squared = adj.r.squared,
    residual_standard_error = sigma,
    model_statistic = statistic,
    model_p_value = p.value
  )

raw_vif_values <- car::vif(regression_model)

if (is.matrix(raw_vif_values)) {
  vif_values_table <- tibble::tibble(
    term = rownames(raw_vif_values),
    gvif = raw_vif_values[, 1],
    degrees_freedom = raw_vif_values[, 2],
    adjusted_vif = raw_vif_values[, 3]
  )
} else {
  vif_values_table <- tibble::tibble(
    term = names(raw_vif_values),
    gvif = as.numeric(raw_vif_values),
    degrees_freedom = 1,
    adjusted_vif = as.numeric(raw_vif_values)
  )
}

write_output_table(
  regression_coefficients_table,
  "table_07_linear_regression_coefficient_estimates.csv"
)
write_output_table(
  regression_fit_table,
  "table_08_linear_regression_model_fit_statistics.csv"
)
write_output_table(
  vif_values_table,
  "table_09_variance_inflation_factor_summary.csv"
)

saveRDS(
  regression_model,
  file.path(
    project_paths$outputs_models,
    "linear_regression_model_average_salary.rds"
  )
)

# --------------------------------------------------
# Diagnostic plots
# --------------------------------------------------

regression_diagnostics <- broom::augment(regression_model)
regression_diagnostics <- regression_diagnostics %>%
  mutate(
    residual_size = case_when(
      abs(.std.resid) >= 2 ~ "Large (>|2|)",
      abs(.std.resid) >= 1 ~ "Moderate (1 to 2)",
      TRUE ~ "Smaller (<1)"
    ),
    residual_size = factor(
      residual_size,
      levels = c("Smaller (<1)", "Moderate (1 to 2)", "Large (>|2|)")
    )
  )

qq_tail_points <- regression_diagnostics %>%
  filter(abs(.std.resid) > 2)

regression_residual_plot <- ggplot(
  regression_diagnostics,
  aes(x = .fitted, y = .resid, colour = residual_size)
) +
  geom_point(
    alpha = 0.7,
    size = 2
  ) +
  geom_hline(yintercept = 0, colour = analysis_palette["accent"], linewidth = 0.8) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    colour = analysis_palette["neutral"],
    linewidth = 0.8
  ) +
  scale_colour_manual(
    values = c(
      "Smaller (<1)" = unname(analysis_palette["secondary"]),
      "Moderate (1 to 2)" = unname(analysis_palette["gold"]),
      "Large (>|2|)" = unname(analysis_palette["accent"])
    )
  ) +
  labs(
    title = "Residual Spread Remains Broad Across the Fitted Salary Range",
    subtitle = paste(
      "R-squared =",
      format_number(regression_fit_table$r_squared, 3),
      "and adjusted R-squared =",
      format_number(regression_fit_table$adjusted_r_squared, 3),
      "so the model explains only a small share of salary variation."
    ),
    x = "Fitted average salary",
    y = "Residuals",
    colour = "Residual size",
    caption = figure_caption(
      "A random cloud around zero is preferred; the wide spread reflects the model's weak explanatory power."
    )
  ) +
  analysis_theme(legend_position = "top")

regression_qq_plot <- ggplot(regression_diagnostics, aes(sample = .std.resid)) +
  stat_qq(
    colour = analysis_palette["neutral"],
    alpha = 0.6,
    size = 1.7
  ) +
  stat_qq(
    data = qq_tail_points,
    colour = analysis_palette["accent"],
    alpha = 0.9,
    size = 2.1
  ) +
  stat_qq_line(
    colour = analysis_palette["accent"],
    linewidth = 0.8
  ) +
  labs(
    title = "Normal Q-Q Plot for Standardised Residuals",
    subtitle = "Most residuals follow the reference line reasonably well, with a few larger tail deviations",
    x = "Theoretical quantiles",
    y = "Sample quantiles",
    caption = figure_caption(
      "Red points identify standardised residuals beyond +/-2, which are the main tail departures."
    )
  ) +
  analysis_theme()

regression_residual_histogram <- ggplot(
  regression_diagnostics,
  aes(x = .resid)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 28,
    fill = analysis_palette["secondary"],
    colour = "white",
    alpha = 0.72
  ) +
  geom_density(
    fill = analysis_palette["secondary"],
    colour = analysis_palette["primary"],
    alpha = 0.14,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = 0,
    colour = analysis_palette["accent"],
    linewidth = 0.9
  ) +
  labs(
    title = "Residual Distribution is Centred Close to Zero",
    subtitle = paste(
      "Residual mean =",
      format_number(mean(regression_diagnostics$.resid), 2),
      "with a wide spread, which again points to modest model fit."
    ),
    x = "Residual",
    y = "Density",
    caption = figure_caption(
      "The distribution is centred as expected, but it is still fairly broad rather than tightly concentrated."
    )
  ) +
  analysis_theme()

save_analysis_figure(
  regression_residual_plot,
  "figure_08_regression_residuals_versus_fitted_values.png"
)
save_analysis_figure(
  regression_qq_plot,
  "figure_09_regression_normal_qq_plot.png"
)
save_analysis_figure(
  regression_residual_histogram,
  "figure_10_regression_residual_distribution.png"
)

cat("\nLinear regression model summary:\n")
print(regression_model_summary)
print(regression_fit_table)
