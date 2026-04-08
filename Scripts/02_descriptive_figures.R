# ==================================================
# Week 11 Data Science Jobs Analysis Project
# Script: 02_descriptive_figures.R
# Purpose: Create descriptive summaries and clean visualisations
#          that support the later modelling choices.
# Inputs:  jobs_data from Script 01 or Data/Clean/Jobs_clean.csv
# Outputs: table_04_descriptive_statistics_for_main_variables.csv
#          figure_01_average_salary_distribution.png
#          figure_02_average_salary_by_main_job_group.png
#          figure_03_company_rating_and_average_salary.png
#          figure_04_frequency_of_main_job_groups.png
#          figure_05_company_age_distribution.png
#          figure_06_average_salary_by_same_state_indicator.png
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
  "table_04_descriptive_statistics_for_main_variables.csv"
)

# --------------------------------------------------
# Plot data preparation
# --------------------------------------------------

salary_distribution_summary <- jobs_data %>%
  summarise(
    mean_salary = mean(avg_salary, na.rm = TRUE),
    median_salary = median(avg_salary, na.rm = TRUE),
    salary_q1 = quantile(avg_salary, probs = 0.25, na.rm = TRUE),
    salary_q3 = quantile(avg_salary, probs = 0.75, na.rm = TRUE)
  )

company_age_distribution_summary <- jobs_data %>%
  filter(!is.na(company_age)) %>%
  summarise(
    mean_company_age = mean(company_age, na.rm = TRUE),
    median_company_age = median(company_age, na.rm = TRUE),
    company_age_q1 = quantile(company_age, probs = 0.25, na.rm = TRUE),
    company_age_q3 = quantile(company_age, probs = 0.75, na.rm = TRUE)
  )

rating_salary_correlation <- cor(
  jobs_data$rating,
  jobs_data$avg_salary,
  use = "complete.obs"
)

job_type_plot_data <- jobs_data %>%
  filter(job_simp %in% selected_job_groups) %>%
  mutate(
    job_simp = forcats::fct_reorder(
      job_simp,
      avg_salary,
      .fun = median
    )
  )

# Only the four most common meaningful job groups are kept in the
# job-type plots so that the comparisons remain readable and useful.

job_type_label_data <- job_type_plot_data %>%
  group_by(job_simp) %>%
  summarise(
    group_n = n(),
    label_y = max(avg_salary, na.rm = TRUE) + 7,
    .groups = "drop"
  )

job_type_count_data <- jobs_data %>%
  mutate(
    job_simp_display = if_else(job_simp == "na", "unspecified", job_simp)
  ) %>%
  count(job_simp_display, sort = TRUE) %>%
  slice_head(n = 5) %>%
  arrange(n) %>%
  mutate(
    job_simp_display = factor(job_simp_display, levels = job_simp_display)
  )

same_state_plot_data <- jobs_data %>%
  mutate(
    same_state_label = factor(
      same_state,
      levels = c(0, 1),
      labels = c("Different state", "Same state")
    )
  )

same_state_summary_data <- same_state_plot_data %>%
  group_by(same_state_label) %>%
  summarise(
    group_n = n(),
    mean_avg_salary = mean(avg_salary, na.rm = TRUE),
    .groups = "drop"
  )

# --------------------------------------------------
# Plots
# --------------------------------------------------

salary_histogram_plot <- ggplot(jobs_data, aes(x = avg_salary)) +
  annotate(
    "rect",
    xmin = salary_distribution_summary$salary_q1,
    xmax = salary_distribution_summary$salary_q3,
    ymin = 0,
    ymax = Inf,
    fill = analysis_palette["light_fill"],
    alpha = 0.9
  ) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 28,
    fill = analysis_palette["secondary"],
    colour = "white",
    alpha = 0.7
  ) +
  geom_density(
    fill = analysis_palette["primary"],
    colour = analysis_palette["primary"],
    alpha = 0.12,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = salary_distribution_summary$mean_salary,
    colour = analysis_palette["accent"],
    linewidth = 0.95
  ) +
  geom_vline(
    xintercept = salary_distribution_summary$median_salary,
    colour = analysis_palette["charcoal"],
    linewidth = 0.95,
    linetype = "dashed"
  ) +
  labs(
    title = "Average Salary Distribution Across Job Postings",
    subtitle = paste(
      "Mean =", paste0(format_number(salary_distribution_summary$mean_salary, 1), "k;"),
      "median =", paste0(format_number(salary_distribution_summary$median_salary, 1), "k."),
      "The shaded band marks the middle 50% of postings."
    ),
    x = "Average salary (thousand USD)",
    y = "Density",
    caption = figure_caption(
      "The solid vertical line marks the mean and the dashed line marks the median."
    )
  ) +
  analysis_theme()

salary_by_job_type_plot <- ggplot(
  job_type_plot_data,
  aes(x = job_simp, y = avg_salary, fill = job_simp)
) +
  geom_violin(alpha = 0.5, colour = NA, trim = FALSE) +
  geom_boxplot(
    width = 0.18,
    fill = "white",
    colour = analysis_palette["charcoal"],
    alpha = 0.95,
    outlier.alpha = 0.18
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    shape = 21,
    size = 2.8,
    fill = analysis_palette["gold"],
    colour = analysis_palette["ink"]
  ) +
  geom_text(
    data = job_type_label_data,
    aes(x = job_simp, y = label_y, label = paste0("n = ", group_n)),
    inherit.aes = FALSE,
    size = 3.4,
    colour = analysis_palette["charcoal"]
  ) +
  coord_flip() +
  scale_fill_manual(
    values = unname(job_group_palette[levels(job_type_plot_data$job_simp)])
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  labs(
    title = "Salary Levels Differ Across the Main Job Groups",
    subtitle = "Only the four main job groups are retained so that each comparison remains readable",
    x = NULL,
    y = "Average salary (thousand USD)",
    caption = figure_caption(
      "Violin width shows concentration, while the boxplot shows the quartiles and median."
    )
  ) +
  analysis_theme()

rating_vs_salary_plot <- ggplot(
  same_state_plot_data,
  aes(x = rating, y = avg_salary, colour = same_state_label)
) +
  geom_point(
    alpha = 0.45,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = analysis_palette["accent"],
    fill = analysis_palette["light_fill"],
    linewidth = 0.9
  ) +
  scale_colour_manual(
    values = unname(binary_palette[levels(same_state_plot_data$same_state_label)])
  ) +
  labs(
    title = "Company Rating Shows Only a Weak Positive Salary Pattern",
    subtitle = paste(
      "Correlation between rating and average salary:",
      paste0("r = ", format_number(rating_salary_correlation, 2), "."),
      "Ratings of 0 are retained as unrated firms."
    ),
    x = "Company rating",
    y = "Average salary (thousand USD)",
    colour = "Location match",
    caption = figure_caption(
      "The fitted line summarises the overall linear trend and does not imply causation."
    )
  ) +
  analysis_theme(legend_position = "top")

job_type_counts_plot <- ggplot(job_type_count_data, aes(y = job_simp_display, x = n)) +
  geom_segment(
    aes(x = 0, xend = n, y = job_simp_display, yend = job_simp_display),
    colour = analysis_palette["light_fill"],
    linewidth = 1.6
  ) +
  geom_point(
    colour = analysis_palette["primary"],
    size = 3.8
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = -0.15,
    size = 3.5,
    colour = analysis_palette["ink"]
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Data Scientist Roles Dominate the Dataset",
    subtitle = "The job_simp distribution is clearly imbalanced before any classification work begins",
    x = "Number of job postings",
    y = NULL,
    caption = figure_caption(
      "Only the most common job categories are shown so that the ranking remains easy to follow."
    )
  ) +
  analysis_theme()

company_age_histogram_plot <- ggplot(
  jobs_data %>% filter(!is.na(company_age)),
  aes(x = company_age)
) +
  annotate(
    "rect",
    xmin = company_age_distribution_summary$company_age_q1,
    xmax = company_age_distribution_summary$company_age_q3,
    ymin = 0,
    ymax = Inf,
    fill = analysis_palette["light_fill"],
    alpha = 0.9
  ) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 28,
    fill = analysis_palette["sage"],
    colour = "white",
    alpha = 0.72
  ) +
  geom_density(
    colour = analysis_palette["sage"],
    fill = analysis_palette["sage"],
    alpha = 0.14,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = company_age_distribution_summary$mean_company_age,
    colour = analysis_palette["accent"],
    linewidth = 0.95
  ) +
  geom_vline(
    xintercept = company_age_distribution_summary$median_company_age,
    colour = analysis_palette["charcoal"],
    linewidth = 0.95,
    linetype = "dashed"
  ) +
  labs(
    title = "Company Age is Concentrated in Younger and Mid-Age Firms",
    subtitle = paste(
      "Mean company age =",
      paste0(format_number(company_age_distribution_summary$mean_company_age, 1), " years;"),
      "median =",
      paste0(format_number(company_age_distribution_summary$median_company_age, 1), " years.")
    ),
    x = "Company age (years)",
    y = "Density",
    caption = figure_caption(
      "The plot uses only records with non-missing company age values."
    )
  ) +
  analysis_theme()

salary_by_same_state_plot <- ggplot(
  same_state_plot_data,
  aes(x = same_state_label, y = avg_salary, fill = same_state_label)
) +
  geom_violin(alpha = 0.38, colour = NA, trim = FALSE) +
  geom_boxplot(
    width = 0.18,
    alpha = 0.9,
    outlier.alpha = 0.2,
    colour = analysis_palette["charcoal"]
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    size = 3.2,
    fill = "white",
    colour = analysis_palette["ink"]
  ) +
  geom_text(
    data = same_state_summary_data,
    aes(
      x = same_state_label,
      y = max(same_state_plot_data$avg_salary, na.rm = TRUE) + 8,
      label = paste0(
        "Mean = ", format_number(mean_avg_salary, 1), "k",
        "\n",
        "n = ", group_n
      )
    ),
    inherit.aes = FALSE,
    size = 3.4,
    colour = analysis_palette["charcoal"]
  ) +
  scale_fill_manual(
    values = unname(binary_palette[levels(same_state_plot_data$same_state_label)])
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.16))) +
  labs(
    title = "Same-State Roles Sit Slightly Lower in the Salary Distribution",
    subtitle = "This descriptive figure previews the two-group comparison used in the hypothesis test",
    x = "Posting location relative to headquarters",
    y = "Average salary (thousand USD)",
    caption = figure_caption(
      "White points mark group means, while the violin and boxplot show the shape and spread of each group."
    )
  ) +
  analysis_theme()

save_analysis_figure(
  salary_histogram_plot,
  "figure_01_average_salary_distribution.png"
)
save_analysis_figure(
  salary_by_job_type_plot,
  "figure_02_average_salary_by_main_job_group.png"
)
save_analysis_figure(
  rating_vs_salary_plot,
  "figure_03_company_rating_and_average_salary.png"
)
save_analysis_figure(
  job_type_counts_plot,
  "figure_04_frequency_of_main_job_groups.png"
)
save_analysis_figure(
  company_age_histogram_plot,
  "figure_05_company_age_distribution.png"
)
save_analysis_figure(
  salary_by_same_state_plot,
  "figure_06_average_salary_by_same_state_indicator.png"
)

cat("\nVisual exploration outputs saved.\n")
