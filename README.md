# Week 11 Data Science Jobs Analysis Project

## Assignment Purpose
This project finalises the Week 11 jobs mini-project using the cleaned Week 5 dataset work. It covers one hypothesis test, one linear regression model, one classification model, one clustering method, supporting figures, written interpretation, and audience-focused presentation notes.

## Project Structure
- `Data/Raw/Tabular_DS_Jobs.csv`
- `Data/Clean/Jobs_clean.csv`
- `Scripts/00_run_entire_project.R`
- `Scripts/01_data_quality_review.R`
- `Scripts/02_descriptive_figures.R`
- `Scripts/03_hypothesis_test_salary_by_location_match.R`
- `Scripts/04_linear_regression_average_salary.R`
- `Scripts/05_classification_tree_job_category.R`
- `Scripts/06_kmeans_clustering_job_profiles.R`
- `Scripts/07_written_summary_and_presentation_notes.R`
- `Outputs/Tables/`
- `Outputs/Figures/`
- `Outputs/Models/`
- `week11_key_findings.md`
- `week11_presentation_notes.md`
- `week11_project_snapshot.md`
- `README.md`

## Exact Output File Names
### Tables
- `Outputs/Tables/table_01_dataset_structure_and_quality_summary.csv`
- `Outputs/Tables/table_02_variable_missingness_summary.csv`
- `Outputs/Tables/table_03_key_category_frequency_summary.csv`
- `Outputs/Tables/table_04_descriptive_statistics_for_main_variables.csv`
- `Outputs/Tables/table_05_salary_summary_by_same_state_group.csv`
- `Outputs/Tables/table_06_welch_t_test_results_for_salary_difference.csv`
- `Outputs/Tables/table_07_linear_regression_coefficient_estimates.csv`
- `Outputs/Tables/table_08_linear_regression_model_fit_statistics.csv`
- `Outputs/Tables/table_09_variance_inflation_factor_summary.csv`
- `Outputs/Tables/table_10_classification_sample_class_counts.csv`
- `Outputs/Tables/table_11_decision_tree_confusion_matrix.csv`
- `Outputs/Tables/table_12_decision_tree_performance_metrics.csv`
- `Outputs/Tables/table_13_cluster_selection_diagnostics.csv`
- `Outputs/Tables/table_14_cluster_profile_summary.csv`
- `Outputs/Tables/table_15_cluster_membership_counts.csv`

### Figures
- `Outputs/Figures/figure_01_average_salary_distribution.png`
- `Outputs/Figures/figure_02_average_salary_by_main_job_group.png`
- `Outputs/Figures/figure_03_company_rating_and_average_salary.png`
- `Outputs/Figures/figure_04_frequency_of_main_job_groups.png`
- `Outputs/Figures/figure_05_company_age_distribution.png`
- `Outputs/Figures/figure_06_average_salary_by_same_state_indicator.png`
- `Outputs/Figures/figure_07_salary_by_same_state_group.png`
- `Outputs/Figures/figure_08_regression_residuals_versus_fitted_values.png`
- `Outputs/Figures/figure_09_regression_normal_qq_plot.png`
- `Outputs/Figures/figure_10_regression_residual_distribution.png`
- `Outputs/Figures/figure_11_decision_tree_for_job_category_classification.png`
- `Outputs/Figures/figure_12_kmeans_elbow_curve.png`
- `Outputs/Figures/figure_13_kmeans_silhouette_comparison.png`
- `Outputs/Figures/figure_14_cluster_visualisation_in_principal_components.png`

### Models
- `Outputs/Models/classification_tree_model_job_category.rds`
- `Outputs/Models/kmeans_clustering_model_job_profiles.rds`
- `Outputs/Models/linear_regression_model_average_salary.rds`

## Packages
- `dplyr`
- `ggplot2`
- `tidyr`
- `readr`
- `broom`
- `car`
- `rpart`
- `rpart.plot`
- `cluster`
- `forcats`
- `scales`

## How to Run
1. Open the `Project Week 11` folder in R or RStudio.
2. Run `Scripts/00_run_entire_project.R` from top to bottom.
3. The runner checks that the input files, scripts, key objects, and main outputs all exist before finishing.

## Outputs to Inspect First
- `week11_project_snapshot.md` for the shortest project overview.
- `week11_key_findings.md` for the main written summary.
- `Outputs/Figures/figure_07_salary_by_same_state_group.png` for the hypothesis test result.
- `Outputs/Tables/table_08_linear_regression_model_fit_statistics.csv` for the weak regression fit summary.
- `Outputs/Figures/figure_11_decision_tree_for_job_category_classification.png` and `Outputs/Tables/table_12_decision_tree_performance_metrics.csv` for the classification result.
- `Outputs/Figures/figure_14_cluster_visualisation_in_principal_components.png` and `Outputs/Tables/table_14_cluster_profile_summary.csv` for the clustering interpretation.

## Interpretation Warning
- The regression model is descriptive and weak overall, not a strong predictive model.
- The classification tree is affected by class imbalance and should be read cautiously for smaller classes.
- The clustering solution is exploratory and should not be treated as proof of fixed job categories.
