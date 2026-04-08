# ==================================================
# Week 11 Data Science Jobs Analysis Project
# Script: 00_run_entire_project.R
# Purpose: Run the full Week 11 jobs analysis project from setup
#          to final written outputs.
# Inputs:  Data/Raw/Tabular_DS_Jobs.csv
#          Data/Clean/Jobs_clean.csv
# Outputs: All saved tables, figures, model objects, and markdown
#          files created in the project folders.
# ==================================================

rm(list = ls())
gc()

options(scipen = 999)

project_title <- "Week 11 Data Science Jobs Analysis Project"
raw_data_filename <- "Tabular_DS_Jobs.csv"
clean_data_filename <- "Jobs_clean.csv"

# --------------------------------------------------
# Project paths
# --------------------------------------------------

current_directory <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

has_project_structure <- function(path) {
  all(dir.exists(file.path(path, c("Data", "Scripts", "Outputs", "Presentation"))))
}

if (has_project_structure(current_directory)) {
  project_root <- current_directory
} else if (basename(current_directory) == "Scripts") {
  parent_directory <- normalizePath("..", winslash = "/", mustWork = TRUE)

  if (!has_project_structure(parent_directory)) {
    stop("The parent folder of Scripts does not contain the expected project structure.")
  }

  project_root <- parent_directory
} else {
  stop(
    paste(
      "Could not locate the project root.",
      "Run this script from Project Week 11 or from the Scripts folder."
    )
  )
}

setwd(project_root)

project_paths <- list(
  root = project_root,
  data_raw = file.path(project_root, "Data", "Raw"),
  data_clean = file.path(project_root, "Data", "Clean"),
  scripts = file.path(project_root, "Scripts"),
  outputs_tables = file.path(project_root, "Outputs", "Tables"),
  outputs_figures = file.path(project_root, "Outputs", "Figures"),
  outputs_models = file.path(project_root, "Outputs", "Models"),
  presentation = file.path(project_root, "Presentation")
)

raw_data_path <- file.path(project_paths$data_raw, raw_data_filename)
clean_data_path <- file.path(project_paths$data_clean, clean_data_filename)

project_script_files <- c(
  "00_run_entire_project.R",
  "01_data_quality_review.R",
  "02_descriptive_figures.R",
  "03_hypothesis_test_salary_by_location_match.R",
  "04_linear_regression_average_salary.R",
  "05_classification_tree_job_category.R",
  "06_kmeans_clustering_job_profiles.R",
  "07_written_summary_and_presentation_notes.R"
)

analysis_script_files <- project_script_files[-1]
script_files <- project_script_files

table_output_files <- c(
  "table_01_dataset_structure_and_quality_summary.csv",
  "table_02_variable_missingness_summary.csv",
  "table_03_key_category_frequency_summary.csv",
  "table_04_descriptive_statistics_for_main_variables.csv",
  "table_05_salary_summary_by_same_state_group.csv",
  "table_06_welch_t_test_results_for_salary_difference.csv",
  "table_07_linear_regression_coefficient_estimates.csv",
  "table_08_linear_regression_model_fit_statistics.csv",
  "table_09_variance_inflation_factor_summary.csv",
  "table_10_classification_sample_class_counts.csv",
  "table_11_decision_tree_confusion_matrix.csv",
  "table_12_decision_tree_performance_metrics.csv",
  "table_13_cluster_selection_diagnostics.csv",
  "table_14_cluster_profile_summary.csv",
  "table_15_cluster_membership_counts.csv"
)

figure_output_files <- c(
  "figure_01_average_salary_distribution.png",
  "figure_02_average_salary_by_main_job_group.png",
  "figure_03_company_rating_and_average_salary.png",
  "figure_04_frequency_of_main_job_groups.png",
  "figure_05_company_age_distribution.png",
  "figure_06_average_salary_by_same_state_indicator.png",
  "figure_07_salary_by_same_state_group.png",
  "figure_08_regression_residuals_versus_fitted_values.png",
  "figure_09_regression_normal_qq_plot.png",
  "figure_10_regression_residual_distribution.png",
  "figure_11_decision_tree_for_job_category_classification.png",
  "figure_12_kmeans_elbow_curve.png",
  "figure_13_kmeans_silhouette_comparison.png",
  "figure_14_cluster_visualisation_in_principal_components.png"
)

model_output_files <- c(
  "classification_tree_model_job_category.rds",
  "kmeans_clustering_model_job_profiles.rds",
  "linear_regression_model_average_salary.rds"
)

presentation_output_files <- c(
  "week11_key_findings.md",
  "week11_presentation_notes.md",
  "week11_project_snapshot.md"
)

legacy_presentation_files <- c(
  "Week 11 Key Findings and Interpretation.md",
  "Week 11 Presentation Speaking Notes.md"
)

generated_output_directories <- unlist(project_paths[c(
  "outputs_tables",
  "outputs_figures",
  "outputs_models",
  "presentation"
)])

for (output_dir in generated_output_directories) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
}

clear_directory_files <- function(directory_path) {
  directory_files <- list.files(directory_path, full.names = TRUE, all.files = FALSE)

  if (length(directory_files) > 0) {
    unlink(directory_files, recursive = TRUE, force = TRUE)
  }
}

for (output_dir in unlist(project_paths[c(
  "outputs_tables",
  "outputs_figures",
  "outputs_models"
)])) {
  clear_directory_files(output_dir)
}

legacy_presentation_paths <- file.path(
  project_paths$presentation,
  c(presentation_output_files, legacy_presentation_files)
)
unlink(legacy_presentation_paths[file.exists(legacy_presentation_paths)], force = TRUE)

required_input_files <- c(raw_data_path, clean_data_path)
missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0) {
  stop(
    paste(
      "The following required input files were not found:",
      paste(missing_input_files, collapse = ", ")
    )
  )
}

expected_script_paths <- file.path(project_paths$scripts, project_script_files)
missing_script_paths <- expected_script_paths[!file.exists(expected_script_paths)]

if (length(missing_script_paths) > 0) {
  stop(
    paste(
      "The following required scripts were not found:",
      paste(missing_script_paths, collapse = ", ")
    )
  )
}

# --------------------------------------------------
# Packages and helper objects
# --------------------------------------------------

required_packages <- c(
  "dplyr",
  "ggplot2",
  "tidyr",
  "readr",
  "broom",
  "car",
  "rpart",
  "rpart.plot",
  "cluster",
  "forcats",
  "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(
  required_packages,
  function(package_name) {
    suppressWarnings(
      suppressPackageStartupMessages(
        library(package_name, character.only = TRUE)
      )
    )
  }
))

set.seed(7202)

selected_job_groups <- c(
  "data scientist",
  "analyst",
  "data engineer",
  "mle"
)

analysis_palette <- c(
  primary = "#16324F",
  secondary = "#6C8FB3",
  accent = "#B25D4A",
  neutral = "#7A8793",
  light_fill = "#EEF3F7",
  sage = "#5A7D6B",
  gold = "#C59A3D",
  charcoal = "#425466",
  ink = "#16324F"
)

job_group_palette <- c(
  "data scientist" = "#1D4E89",
  "analyst" = "#7A99B8",
  "data engineer" = "#B45F49",
  "mle" = "#607D68",
  "other_or_unspecified" = "#9AA6B2"
)

binary_palette <- c(
  "Same state" = "#1D4E89",
  "Different state" = "#AAB6C2"
)

cluster_palette <- c(
  "1" = "#1D4E89",
  "2" = "#B45F49",
  "3" = "#607D68",
  "4" = "#8B74A9",
  "5" = "#C59A3D",
  "6" = "#6C8FB3"
)

analysis_theme <- function(legend_position = "none") {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "#FBFCFD", colour = NA),
      plot.title = element_text(
        face = "bold",
        size = 15,
        colour = analysis_palette["ink"],
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 10.5,
        colour = "grey30",
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        size = 9,
        colour = "grey45",
        hjust = 0,
        margin = margin(t = 10)
      ),
      axis.title = element_text(face = "bold", colour = analysis_palette["ink"]),
      axis.text = element_text(colour = "grey20"),
      legend.title = element_text(face = "bold"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.position = legend_position,
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#E1E8EE", linewidth = 0.45),
      plot.margin = margin(10, 12, 10, 12),
      plot.title.position = "plot"
    )
}

figure_caption <- function(extra_note = NULL) {
  base_caption <- paste0("Source: ", clean_data_filename, ".")

  if (is.null(extra_note) || extra_note == "") {
    return(base_caption)
  }

  paste(base_caption, extra_note)
}

save_analysis_figure <- function(plot_object,
                                 filename,
                                 width = 10,
                                 height = 6,
                                 dpi = 300) {
  ggplot2::ggsave(
    filename = file.path(project_paths$outputs_figures, filename),
    plot = plot_object,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

write_output_table <- function(data_object, filename) {
  readr::write_csv(
    data_object,
    file.path(project_paths$outputs_tables, filename)
  )
}

add_model_fields <- function(data_object) {
  median_company_age <- median(data_object$company_age, na.rm = TRUE)

  data_object %>%
    mutate(
      company_age_imputed = if_else(
        is.na(company_age),
        median_company_age,
        company_age
      ),
      job_simp_grouped = if_else(
        job_simp %in% selected_job_groups,
        job_simp,
        "other_or_unspecified"
      ),
      job_simp_grouped = factor(
        job_simp_grouped,
        levels = c(selected_job_groups, "other_or_unspecified")
      )
    )
}

format_number <- function(x, digits = 2) {
  formatC(x, digits = digits, format = "f")
}

format_percent <- function(x, digits = 1) {
  paste0(format_number(100 * x, digits = digits), "%")
}

format_p_value <- function(x) {
  ifelse(x < 0.001, "< 0.001", format_number(x, digits = 3))
}

# --------------------------------------------------
# Run project scripts
# --------------------------------------------------

for (script_file in analysis_script_files) {
  cat("\nRunning", script_file, "...\n")
  source(file.path(project_paths$scripts, script_file), local = FALSE)
}

# --------------------------------------------------
# Final consistency checks
# --------------------------------------------------

required_result_objects <- c(
  "jobs_data",
  "dataset_overview_table",
  "hypothesis_group_summary",
  "t_test_results_table",
  "regression_model",
  "classification_tree_model",
  "final_kmeans_model",
  "cluster_profiles_table"
)

missing_result_objects <- required_result_objects[
  !vapply(required_result_objects, exists, logical(1), inherits = TRUE)
]

existing_output_directories <- generated_output_directories[
  dir.exists(generated_output_directories)
]

expected_output_paths <- c(
  file.path(project_paths$outputs_tables, table_output_files),
  file.path(project_paths$outputs_figures, figure_output_files),
  file.path(project_paths$outputs_models, model_output_files),
  file.path(project_paths$presentation, presentation_output_files),
  file.path(project_paths$root, "README.md")
)

missing_output_paths <- expected_output_paths[!file.exists(expected_output_paths)]

if (length(existing_output_directories) != length(generated_output_directories)) {
  stop("One or more output directories are missing after the project run.")
}

if (length(missing_result_objects) > 0) {
  stop(
    paste(
      "The following key result objects were not created:",
      paste(missing_result_objects, collapse = ", ")
    )
  )
}

if (length(missing_output_paths) > 0) {
  stop(
    paste(
      "The following expected output files were not saved:",
      paste(missing_output_paths, collapse = ", ")
    )
  )
}

cat("\nFinal consistency checks passed.\n")
cat("\n", project_title, " completed successfully.\n", sep = "")
cat("Project root:", project_root, "\n")
cat("Tables saved to:", project_paths$outputs_tables, "\n")
cat("Figures saved to:", project_paths$outputs_figures, "\n")
cat("Models saved to:", project_paths$outputs_models, "\n")
cat("Written outputs saved to:", project_paths$presentation, "\n")
