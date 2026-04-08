# ==================================================
# Week 11 Data Science Jobs Analysis Project
# Script: 00 Run Entire Project.R
# Purpose: Run the full Week 11 jobs analysis project from setup
#          to final written outputs.
# Inputs:  Data/Raw/Tabular_DS_Jobs.csv
#          Data/Clean/Jobs Clean.csv
# Outputs: All saved tables, figures, and text
#          files created in the project folders.
# ==================================================

rm(list = ls())
gc()

options(scipen = 999)

project_title <- "Week 11 Data Science Jobs Analysis Project"
raw_data_filename <- "Tabular_DS_Jobs.csv"
clean_data_filename <- "Jobs Clean.csv"

# --------------------------------------------------
# Project paths
# --------------------------------------------------

current_directory <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

has_project_structure <- function(path) {
  all(dir.exists(file.path(path, c("Data", "Scripts", "Outputs"))))
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
  written_outputs = project_root
)

raw_data_path <- file.path(project_paths$data_raw, raw_data_filename)
clean_data_path <- file.path(project_paths$data_clean, clean_data_filename)

project_script_files <- c(
  "00 Run Entire Project.R",
  "01 Data Quality Review.R",
  "02 Descriptive Figures.R",
  "03 Hypothesis Test Salary by Location Match.R",
  "04 Linear Regression Average Salary.R",
  "05 Classification Tree Job Category.R",
  "06 K Means Clustering Job Profiles.R",
  "07 Written Summary and Presentation Notes.R"
)

analysis_script_files <- project_script_files[-1]
script_files <- project_script_files

table_output_files <- c(
  "Table 01 Dataset Structure and Quality Summary.csv",
  "Table 02 Variable Missingness Summary.csv",
  "Table 03 Key Category Frequency Summary.csv",
  "Table 04 Descriptive Statistics for Main Variables.csv",
  "Table 05 Salary Summary by Same State Group.csv",
  "Table 06 Welch T Test Results for Salary Difference.csv",
  "Table 07 Linear Regression Coefficient Estimates.csv",
  "Table 08 Linear Regression Model Fit Statistics.csv",
  "Table 09 Variance Inflation Factor Summary.csv",
  "Table 10 Classification Sample Class Counts.csv",
  "Table 11 Decision Tree Confusion Matrix.csv",
  "Table 12 Decision Tree Performance Metrics.csv",
  "Table 13 Cluster Selection Diagnostics.csv",
  "Table 14 Cluster Profile Summary.csv",
  "Table 15 Cluster Membership Counts.csv"
)

figure_output_files <- c(
  "Figure 01 Average Salary Distribution.png",
  "Figure 02 Average Salary by Main Job Group.png",
  "Figure 03 Company Rating and Average Salary.png",
  "Figure 04 Frequency of Main Job Groups.png",
  "Figure 05 Company Age Distribution.png",
  "Figure 06 Average Salary by Same State Indicator.png",
  "Figure 07 Salary by Same State Group.png",
  "Figure 08 Regression Residuals Versus Fitted Values.png",
  "Figure 09 Regression Normal QQ Plot.png",
  "Figure 10 Regression Residual Distribution.png",
  "Figure 11 Decision Tree for Job Category Classification.png",
  "Figure 12 K Means Elbow Curve.png",
  "Figure 13 K Means Silhouette Comparison.png",
  "Figure 14 Cluster Visualisation in Principal Components.png"
)

written_output_files <- c(
  "Week 11 Key Findings.txt",
  "Week 11 Presentation Notes.txt",
  "Week 11 Project Snapshot.txt"
)

legacy_written_files <- c(
  "Week 11 Key Findings and Interpretation.txt",
  "Week 11 Presentation Speaking Notes.txt"
)

generated_output_directories <- unlist(project_paths[c(
  "outputs_tables",
  "outputs_figures"
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
  "outputs_figures"
)])) {
  clear_directory_files(output_dir)
}

legacy_written_paths <- file.path(
  project_paths$written_outputs,
  c(written_output_files, legacy_written_files)
)
unlink(legacy_written_paths[file.exists(legacy_written_paths)], force = TRUE)

legacy_markdown_paths <- list.files(
  project_paths$written_outputs,
  pattern = "\\.md$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(legacy_markdown_paths) > 0) {
  unlink(legacy_markdown_paths, force = TRUE)
}

legacy_presentation_directory <- file.path(project_root, "Presentation")
legacy_models_directory <- file.path(project_root, "Outputs", "Models")

if (dir.exists(legacy_presentation_directory)) {
  unlink(legacy_presentation_directory, recursive = TRUE, force = TRUE)
}

if (dir.exists(legacy_models_directory)) {
  unlink(legacy_models_directory, recursive = TRUE, force = TRUE)
}

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
  theme_minimal(base_size = 12.5) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.title = element_text(
        face = "bold",
        size = 18,
        colour = analysis_palette["ink"],
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 12,
        colour = "grey30",
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        size = 9.5,
        colour = "grey45",
        hjust = 0,
        margin = margin(t = 10)
      ),
      axis.title = element_text(
        face = "bold",
        size = 13.5,
        colour = analysis_palette["ink"]
      ),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text = element_text(size = 11.5, colour = "grey20"),
      legend.title = element_text(face = "bold", size = 12.5),
      legend.text = element_text(size = 11.5),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.position = legend_position,
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#DCE6F0", linewidth = 0.55),
      axis.line = element_blank(),
      plot.margin = margin(12, 16, 12, 16),
      plot.title.position = "plot",
      legend.spacing.x = grid::unit(4, "pt")
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
                                 width = 10.8,
                                 height = 6.4,
                                 dpi = 320) {
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

clean_label_text <- function(x) {
  cleaned_text <- gsub("_", " ", x)
  cleaned_text <- tools::toTitleCase(cleaned_text)
  cleaned_text <- gsub("\\bMle\\b", "MLE", cleaned_text)
  cleaned_text
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
  file.path(project_paths$written_outputs, written_output_files),
  file.path(project_paths$root, "Read Me.txt")
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
cat("Written outputs saved to:", project_paths$written_outputs, "\n")
