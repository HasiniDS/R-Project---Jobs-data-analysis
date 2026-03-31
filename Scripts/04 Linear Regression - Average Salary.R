# ==================================================
# Week 11 Jobs Project
# Script: 04 Linear Regression - Average Salary.R
# Purpose: Fit and evaluate one linear regression model for
#          average salary.
# Inputs:  jobs_data from Script 01 or Data/Clean/Data Science Jobs Dataset - Clean.csv
# Outputs: Table 07 Linear Regression Coefficient Estimates.csv
#          Table 08 Linear Regression Model Fit Statistics.csv
#          Table 09 Variance Inflation Factor Summary.csv
#          Figure 08 Regression Residuals versus Fitted Values.png
#          Figure 09 Regression Normal Q Q Plot.png
#          Figure 10 Regression Residual Distribution.png
#          Outputs/Models/Linear Regression Model - Average Salary.rds
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
  "Table 07 Linear Regression Coefficient Estimates.csv"
)
write_output_table(
  regression_fit_table,
  "Table 08 Linear Regression Model Fit Statistics.csv"
)
write_output_table(
  vif_values_table,
  "Table 09 Variance Inflation Factor Summary.csv"
)

saveRDS(
  regression_model,
  file.path(
    project_paths$outputs_models,
    "Linear Regression Model - Average Salary.rds"
  )
)

# --------------------------------------------------
# Diagnostic plots
# --------------------------------------------------

regression_diagnostics <- broom::augment(regression_model)

regression_residual_plot <- ggplot(
  regression_diagnostics,
  aes(x = .fitted, y = .resid)
) +
  geom_point(
    colour = analysis_palette["primary"],
    alpha = 0.55,
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
  labs(
    title = "Regression Residuals vs Fitted Values",
    x = "Fitted average salary",
    y = "Residuals"
  ) +
  analysis_theme()

regression_qq_plot <- ggplot(regression_diagnostics, aes(sample = .std.resid)) +
  stat_qq(
    colour = analysis_palette["primary"],
    alpha = 0.7
  ) +
  stat_qq_line(
    colour = analysis_palette["accent"],
    linewidth = 0.8
  ) +
  labs(
    title = "Regression Q-Q Plot",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  analysis_theme()

regression_residual_histogram <- ggplot(
  regression_diagnostics,
  aes(x = .resid)
) +
  geom_histogram(
    bins = 25,
    fill = analysis_palette["secondary"],
    colour = "white",
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Regression Residuals",
    x = "Residual",
    y = "Frequency"
  ) +
  analysis_theme()

save_analysis_figure(
  regression_residual_plot,
  "Figure 08 Regression Residuals versus Fitted Values.png"
)
save_analysis_figure(
  regression_qq_plot,
  "Figure 09 Regression Normal Q Q Plot.png"
)
save_analysis_figure(
  regression_residual_histogram,
  "Figure 10 Regression Residual Distribution.png"
)

cat("\nLinear regression model summary:\n")
print(regression_model_summary)
print(regression_fit_table)
