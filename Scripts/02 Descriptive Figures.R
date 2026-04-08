# ==================================================
# Week 11 Data Science Jobs Analysis Project
# Script: 02 Descriptive Figures.R
# Purpose: Create descriptive summaries and clean visualisations
#          that support the later modelling choices.
# Inputs:  jobs_data from Script 01 or Data/Clean/Jobs Clean.csv
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

salary_distribution_summary <- jobs_data %>%
  summarise(
    mean_salary = mean(avg_salary, na.rm = TRUE),
    median_salary = median(avg_salary, na.rm = TRUE),
    salary_q1 = quantile(avg_salary, probs = 0.25, na.rm = TRUE),
    salary_q3 = quantile(avg_salary, probs = 0.75, na.rm = TRUE)
  )

salary_density_peak <- max(density(jobs_data$avg_salary, na.rm = TRUE)$y)

company_age_distribution_summary <- jobs_data %>%
  filter(!is.na(company_age)) %>%
  summarise(
    mean_company_age = mean(company_age, na.rm = TRUE),
    median_company_age = median(company_age, na.rm = TRUE),
    company_age_q1 = quantile(company_age, probs = 0.25, na.rm = TRUE),
    company_age_q3 = quantile(company_age, probs = 0.75, na.rm = TRUE)
  )

company_age_density_peak <- max(
  density(
    jobs_data %>% filter(!is.na(company_age)) %>% pull(company_age)
  )$y
)

rating_salary_correlation <- cor(
  jobs_data$rating,
  jobs_data$avg_salary,
  use = "complete.obs"
)

job_type_summary_data <- jobs_data %>%
  filter(job_simp %in% selected_job_groups) %>%
  group_by(job_simp) %>%
  summarise(
    group_n = n(),
    median_salary = median(avg_salary, na.rm = TRUE),
    mean_salary = mean(avg_salary, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(median_salary) %>%
  mutate(
    job_label = paste0(
      clean_label_text(job_simp),
      " (n = ",
      scales::comma(group_n),
      ")"
    )
  )

job_type_plot_data <- jobs_data %>%
  filter(job_simp %in% selected_job_groups) %>%
  left_join(job_type_summary_data, by = "job_simp") %>%
  mutate(
    job_simp = factor(job_simp, levels = job_type_summary_data$job_simp),
    job_label = factor(job_label, levels = job_type_summary_data$job_label)
  )

job_type_count_data <- jobs_data %>%
  mutate(
    job_simp_display = if_else(job_simp == "na", "unspecified", job_simp)
  ) %>%
  count(job_simp_display, sort = TRUE) %>%
  slice_head(n = 5)

largest_job_group <- job_type_count_data$job_simp_display[1]

job_type_count_data <- job_type_count_data %>%
  mutate(
    share = n / nrow(jobs_data),
    job_label = clean_label_text(job_simp_display),
    highlight_group = if_else(
      job_simp_display == largest_job_group,
      "Largest group",
      "Other groups"
    )
  ) %>%
  arrange(n) %>%
  mutate(
    job_label = factor(job_label, levels = job_label)
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
  ) %>%
  mutate(
    summary_label = paste0(
      "Mean = ",
      format_number(mean_avg_salary, 1),
      "k\nn = ",
      scales::comma(group_n)
    ),
    label_y = max(same_state_plot_data$avg_salary, na.rm = TRUE) + 12
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
    bins = 24,
    fill = analysis_palette["secondary"],
    colour = "white",
    alpha = 0.65,
    linewidth = 0.35
  ) +
  geom_density(
    colour = analysis_palette["primary"],
    fill = analysis_palette["primary"],
    alpha = 0.08,
    linewidth = 1.3
  ) +
  geom_vline(
    xintercept = salary_distribution_summary$mean_salary,
    colour = analysis_palette["accent"],
    linewidth = 1
  ) +
  geom_vline(
    xintercept = salary_distribution_summary$median_salary,
    colour = analysis_palette["charcoal"],
    linewidth = 1,
    linetype = "dashed"
  ) +
  annotate(
    "label",
    x = salary_distribution_summary$mean_salary + 11,
    y = salary_density_peak * 0.94,
    label = paste0(
      "Mean\n",
      format_number(salary_distribution_summary$mean_salary, 1),
      "k"
    ),
    fill = "white",
    colour = analysis_palette["accent"],
    linewidth = 0.2,
    size = 3.4
  ) +
  annotate(
    "label",
    x = salary_distribution_summary$median_salary - 11,
    y = salary_density_peak * 0.78,
    label = paste0(
      "Median\n",
      format_number(salary_distribution_summary$median_salary, 1),
      "k"
    ),
    fill = "white",
    colour = analysis_palette["charcoal"],
    linewidth = 0.2,
    size = 3.4
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(6),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.001),
    expand = expansion(mult = c(0, 0.06))
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
  aes(x = job_label, y = avg_salary, fill = job_simp)
) +
  geom_violin(alpha = 0.55, colour = NA, trim = FALSE) +
  geom_boxplot(
    width = 0.18,
    fill = "white",
    colour = analysis_palette["charcoal"],
    alpha = 0.95,
    outlier.alpha = 0.10
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3,
    fill = analysis_palette["gold"],
    colour = analysis_palette["ink"]
  ) +
  coord_flip() +
  scale_fill_manual(
    values = unname(job_group_palette[levels(job_type_plot_data$job_simp)])
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(6),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Salary Levels Differ Across the Main Job Groups",
    subtitle = "Only the four main job groups are retained so that the comparison stays readable and focused",
    x = NULL,
    y = "Average salary (thousand USD)",
    caption = figure_caption(
      "Violin width shows concentration, the boxplot shows the quartiles, and the gold marker shows the group mean."
    )
  ) +
  analysis_theme()

rating_vs_salary_plot <- ggplot(
  same_state_plot_data,
  aes(x = rating, y = avg_salary, colour = same_state_label)
) +
  geom_point(
    alpha = 0.32,
    size = 2.3,
    position = position_jitter(width = 0.05, height = 0)
  ) +
  geom_smooth(
    data = same_state_plot_data,
    aes(x = rating, y = avg_salary),
    inherit.aes = FALSE,
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = analysis_palette["accent"],
    fill = analysis_palette["light_fill"],
    linewidth = 1
  ) +
  scale_colour_manual(
    values = unname(binary_palette[levels(same_state_plot_data$same_state_label)])
  ) +
  scale_x_continuous(
    breaks = 0:5,
    limits = c(-0.15, 5.15)
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(6),
    expand = expansion(mult = c(0.03, 0.04))
  ) +
  labs(
    title = "Company Rating Shows Very Little Linear Salary Pattern",
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

job_type_counts_plot <- ggplot(job_type_count_data, aes(x = n, y = job_label)) +
  geom_col(
    aes(fill = highlight_group),
    width = 0.64,
    alpha = 0.95,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = paste0(
        scales::comma(n),
        " (",
        format_percent(share, 1),
        ")"
      )
    ),
    hjust = -0.08,
    size = 3.7,
    colour = analysis_palette["ink"]
  ) +
  scale_fill_manual(
    values = c(
      "Largest group" = unname(analysis_palette["primary"]),
      "Other groups" = unname(analysis_palette["secondary"])
    )
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(5),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title = "Data Scientist Roles Dominate the Dataset",
    subtitle = "The main job category distribution is clearly imbalanced before any classification work begins",
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
    bins = 24,
    fill = analysis_palette["sage"],
    colour = "white",
    alpha = 0.74,
    linewidth = 0.35
  ) +
  geom_density(
    colour = analysis_palette["sage"],
    fill = analysis_palette["sage"],
    alpha = 0.10,
    linewidth = 1.3
  ) +
  geom_vline(
    xintercept = company_age_distribution_summary$mean_company_age,
    colour = analysis_palette["accent"],
    linewidth = 1
  ) +
  geom_vline(
    xintercept = company_age_distribution_summary$median_company_age,
    colour = analysis_palette["charcoal"],
    linewidth = 1,
    linetype = "dashed"
  ) +
  annotate(
    "label",
    x = company_age_distribution_summary$mean_company_age + 8,
    y = company_age_density_peak * 0.95,
    label = paste0(
      "Mean\n",
      format_number(company_age_distribution_summary$mean_company_age, 1),
      " years"
    ),
    fill = "white",
    colour = analysis_palette["accent"],
    linewidth = 0.2,
    size = 3.3
  ) +
  annotate(
    "label",
    x = company_age_distribution_summary$median_company_age - 8,
    y = company_age_density_peak * 0.80,
    label = paste0(
      "Median\n",
      format_number(company_age_distribution_summary$median_company_age, 1),
      " years"
    ),
    fill = "white",
    colour = analysis_palette["charcoal"],
    linewidth = 0.2,
    size = 3.3
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(6),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.001),
    expand = expansion(mult = c(0, 0.06))
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
  geom_violin(alpha = 0.46, colour = NA, trim = FALSE) +
  geom_boxplot(
    width = 0.20,
    alpha = 0.9,
    outlier.alpha = 0.12,
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
  geom_label(
    data = same_state_summary_data,
    aes(
      x = same_state_label,
      y = label_y,
      label = summary_label
    ),
    inherit.aes = FALSE,
    size = 3.4,
    colour = analysis_palette["charcoal"],
    fill = "white",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = unname(binary_palette[levels(same_state_plot_data$same_state_label)])
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(6),
    expand = expansion(mult = c(0.02, 0.20))
  ) +
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
  "Figure 01 Average Salary Distribution.png",
  width = 10.8,
  height = 6.5
)
save_analysis_figure(
  salary_by_job_type_plot,
  "Figure 02 Average Salary by Main Job Group.png",
  width = 10.8,
  height = 6.7
)
save_analysis_figure(
  rating_vs_salary_plot,
  "Figure 03 Company Rating and Average Salary.png",
  width = 10.8,
  height = 6.5
)
save_analysis_figure(
  job_type_counts_plot,
  "Figure 04 Frequency of Main Job Groups.png",
  width = 10.2,
  height = 6.2
)
save_analysis_figure(
  company_age_histogram_plot,
  "Figure 05 Company Age Distribution.png",
  width = 10.8,
  height = 6.5
)
save_analysis_figure(
  salary_by_same_state_plot,
  "Figure 06 Average Salary by Same State Indicator.png",
  width = 10.2,
  height = 6.4
)

cat("\nVisual exploration outputs saved.\n")
