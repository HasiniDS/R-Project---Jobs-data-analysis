# ==================================================
# Jobs Analyse
# Script: 00 Run Entire Project.R
# Purpose: Run the full Week 11 analysis project from setup to
#          communication outputs.
# Inputs:  Data/Raw/Data Science Jobs Dataset - Raw.csv
#          Data/Clean/Data Science Jobs Dataset - Clean.csv
# Outputs: All tables, plots, model objects, and markdown files
#          saved in the project folders.
# ==================================================

rm(list = ls())
gc()

options(scipen = 999)

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

for (output_dir in unlist(project_paths[c(
  "outputs_tables",
  "outputs_figures",
  "outputs_models",
  "presentation"
)])) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
}

clean_data_path <- file.path(
  project_paths$data_clean,
  "Data Science Jobs Dataset - Clean.csv"
)

if (!file.exists(clean_data_path)) {
  stop("Data Science Jobs Dataset - Clean.csv was not found in Data/Clean.")
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
  base_caption <- "Source: Data Science Jobs Dataset - Clean.csv."

  if (is.null(extra_note) || extra_note == "") {
    return(base_caption)
  }

  paste(base_caption, extra_note)
}

save_analysis_figure <- function(plot_object,
                                 filename,
                                 width = 9.5,
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

script_files <- c(
  "01 Data Quality Review.R",
  "02 Descriptive Figures.R",
  "03 Hypothesis Test - Salary by Location Match.R",
  "04 Linear Regression - Average Salary.R",
  "05 Classification Tree - Job Category.R",
  "06 K Means Clustering - Job Profiles.R",
  "07 Written Summary and Presentation Notes.R"
)

for (script_file in script_files) {
  cat("\nRunning", script_file, "...\n")
  source(file.path(project_paths$scripts, script_file), local = FALSE)
}

cat("\nJobs Analyse completed successfully.\n")
cat("Project root:", project_root, "\n")
cat("Tables saved to:", project_paths$outputs_tables, "\n")
cat("Figures saved to:", project_paths$outputs_figures, "\n")
cat("Written outputs saved to:", project_paths$presentation, "\n")
