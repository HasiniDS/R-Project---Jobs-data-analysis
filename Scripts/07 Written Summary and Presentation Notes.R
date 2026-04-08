# ==================================================
# Week 11 Data Science Jobs Analysis Project
# Script: 07 Written Summary and Presentation Notes.R
# Purpose: Create the written project outputs for submission and
#          presentation use.
# Inputs:  Results objects created in earlier scripts
# Outputs: Week 11 Key Findings.md
#          Week 11 Presentation Notes.md
#          Week 11 Project Snapshot.md
#          README.md
# ==================================================

if (!exists("jobs_data")) {
  jobs_data <- readr::read_csv(clean_data_path, show_col_types = FALSE)
}

if (!exists("project_title")) {
  project_title <- "Week 11 Data Science Jobs Analysis Project"
}

if (!exists("script_files")) {
  script_files <- c(
    "00 Run Entire Project.R",
    "01 Data Quality Review.R",
    "02 Descriptive Figures.R",
    "03 Hypothesis Test Salary by Location Match.R",
    "04 Linear Regression Average Salary.R",
    "05 Classification Tree Job Category.R",
    "06 K Means Clustering Job Profiles.R",
    "07 Written Summary and Presentation Notes.R"
  )
}

if (!exists("table_output_files")) {
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
}

if (!exists("figure_output_files")) {
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
}

if (!exists("model_output_files")) {
  model_output_files <- c(
    "Classification Tree Model Job Category.rds",
    "K Means Clustering Model Job Profiles.rds",
    "Linear Regression Model Average Salary.rds"
  )
}

if (!exists("written_output_files")) {
  written_output_files <- c(
    "Week 11 Key Findings.md",
    "Week 11 Presentation Notes.md",
    "Week 11 Project Snapshot.md"
  )
}

if (!exists("classification_salary_note")) {
  classification_salary_note <- paste(
    "avg_salary is included here because the classifier is describing observed",
    "differences between completed job postings.",
    "A cleaner deployment-style alternative would remove avg_salary if job type",
    "had to be predicted before salary information is available."
  )
}

if (!exists("cluster_k_justification")) {
  cluster_k_justification <- paste(
    "k = 2 was retained because it had the strongest silhouette width,",
    "the elbow curve started to flatten after two clusters, and the",
    "two-cluster solution remained easiest to interpret as a broad skill-mix split."
  )
}

# --------------------------------------------------
# Helper text
# --------------------------------------------------

top_job_count <- jobs_data %>%
  count(job_simp, sort = TRUE) %>%
  slice(1)

top_job_label <- gsub("_", " ", top_job_count$job_simp)

t_test_p_value <- t_test_results_table$p_value[1]
t_test_difference <- t_test_results_table$mean_difference[1]
t_test_effect_size <- t_test_results_table$cohens_d[1]
t_test_result_sentence <- if (t_test_p_value < 0.05) {
  paste0(
    "Average salary differed between the same-state groups. Postings in the same state as headquarters were ",
    format_number(abs(t_test_difference), 2),
    " thousand USD ",
    ifelse(t_test_difference > 0, "higher", "lower"),
    " on average, with a small effect size (Cohen's d = ",
    format_number(t_test_effect_size, 2),
    ")."
  )
} else {
  paste0(
    "The Welch t-test did not find a statistically reliable salary difference ",
    "between the same-state groups (p = ", format_p_value(t_test_p_value), ")."
  )
}

regression_r_squared <- regression_fit_table$r_squared[1]
regression_adjusted_r_squared <- regression_fit_table$adjusted_r_squared[1]
regression_model_p_value <- regression_fit_table$model_p_value[1]

regression_result_sentence <- paste0(
  "The linear regression model was weak overall (R-squared = ",
  format_number(regression_r_squared, 3),
  ", adjusted R-squared = ",
  format_number(regression_adjusted_r_squared, 3),
  "; model p = ",
  format_p_value(regression_model_p_value),
  "), so it should be treated as descriptive rather than strongly predictive."
)

classification_accuracy <- classification_metrics_table %>%
  filter(class == "overall_accuracy") %>%
  pull(precision)

majority_class_baseline <- classification_metrics_table %>%
  filter(class == "majority_class_baseline") %>%
  pull(precision)

class_recalls <- classification_metrics_table %>%
  filter(!class %in% c("overall_accuracy", "majority_class_baseline"))

lowest_recall_class <- class_recalls %>%
  filter(recall == min(recall, na.rm = TRUE)) %>%
  slice(1)

top_tree_variables <- head(tree_variable_importance$variable, 3)

classification_result_sentence <- paste0(
  "The decision tree reached ",
  format_number(classification_accuracy, 3),
  " test accuracy, only slightly above the majority-class baseline of ",
  format_number(majority_class_baseline, 3),
  ". The weakest recall was for ",
  lowest_recall_class$class,
  ", so class imbalance remained a clear limitation."
)

cluster_salary_order <- cluster_profiles_table %>%
  arrange(desc(mean_avg_salary))

highest_salary_cluster <- cluster_salary_order %>% slice(1)

cluster_skill_differences <- cluster_profiles_table %>%
  select(cluster, hadoop_rate, spark_rate, aws_rate, big_data_rate, python_rate) %>%
  pivot_longer(
    cols = -cluster,
    names_to = "skill",
    values_to = "rate"
  ) %>%
  group_by(skill) %>%
  summarise(rate_difference = max(rate) - min(rate), .groups = "drop") %>%
  arrange(desc(rate_difference)) %>%
  slice_head(n = 2)

key_cluster_skills <- gsub("_rate", "", cluster_skill_differences$skill)
key_cluster_skills <- gsub("_", " ", key_cluster_skills)

clustering_result_sentence <- paste0(
  "The final k-means solution used k = ",
  final_k,
  ". Cluster ",
  highest_salary_cluster$cluster,
  " had the higher mean salary at ",
  format_number(highest_salary_cluster$mean_avg_salary, 1),
  " thousand USD, but the clearer separation came from differences in ",
  paste(key_cluster_skills, collapse = " and "),
  "."
)

largest_limitation_sentence <- paste(
  "The largest limitation is the strong class imbalance in job_simp,",
  "combined with sparse seniority data and a weak salary regression fit."
)

final_takeaway_sentence <- paste(
  "The project gives a useful, well-supported summary of broad salary and role patterns",
  "in the jobs dataset, but its modelling results should be used cautiously rather than as final predictions."
)

# --------------------------------------------------
# Week 11 Key Findings.md
# --------------------------------------------------

key_findings_lines <- c(
  paste("#", project_title),
  "",
  "## Key Findings",
  "",
  "### Project Aim",
  "This project uses the cleaned jobs dataset to complete the Week 11 brief through one hypothesis test, one linear regression model, one decision tree classifier, one k-means clustering analysis, supporting figures, and clear audience-focused interpretation.",
  "",
  "### Dataset Overview",
  paste0(
    "The analysis used ", nrow(jobs_data), " cleaned job postings and ", ncol(jobs_data),
    " variables from `Jobs Clean.csv`. The largest job group was `", top_job_label,
    "` with ", top_job_count$n, " postings, so the class distribution was clearly imbalanced."
  ),
  "The `seniority` field was mostly recorded as `na`, so it was described but not used as a main modelling target. Ratings of 0 were retained because they represent unrated companies rather than missing values.",
  "",
  "### Hypothesis Test",
  t_test_result_sentence,
  "",
  "### Linear Regression",
  regression_result_sentence,
  "",
  "### Classification Tree",
  classification_result_sentence,
  classification_salary_note,
  "",
  "### K-Means Clustering",
  clustering_result_sentence,
  cluster_k_justification,
  "The clustering result should still be treated as exploratory rather than definitive.",
  "",
  "### Limitations",
  "- `job_simp` was highly imbalanced, which affected the classification results and limited the reliability of smaller classes.",
  "- `company_age` required median imputation to keep the modelling sample stable.",
  "- `seniority` was too sparse to support reliable modelling.",
  "- The regression remained weak overall, so it should not be treated as a strong predictive model.",
  "- The clustering solution is an exploratory grouping rather than proof of fixed job categories.",
  "",
  "### Conclusion",
  final_takeaway_sentence
)

writeLines(
  key_findings_lines,
  file.path(project_paths$written_outputs, "Week 11 Key Findings.md")
)

# --------------------------------------------------
# Week 11 Presentation Notes.md
# --------------------------------------------------

presentation_notes_lines <- c(
  paste("#", project_title),
  "",
  "## Technical Audience",
  "**Opening**",
  "This project applied a Welch t-test, one linear regression model, one decision tree classifier, and one k-means clustering model to the cleaned data science jobs dataset.",
  "",
  "**Key findings**",
  paste0("- ", t_test_result_sentence),
  paste0("- ", regression_result_sentence),
  paste0("- ", classification_result_sentence),
  paste0("- ", clustering_result_sentence),
  "",
  "**What the models mean**",
  "The test addresses a two-group salary question, the regression summarises conditional salary associations, the decision tree shows which observed posting characteristics separate the main job groups, and clustering provides an exploratory map of broader job profiles.",
  "",
  "**Limitations**",
  paste0("- ", classification_salary_note),
  paste0("- ", cluster_k_justification),
  "- Seniority was too sparse for reliable modelling, and the regression fit remained weak.",
  "",
  "**Recommendations**",
  "- Collect richer variables such as more detailed location, experience, and employer attributes.",
  "- If classification becomes a deployment task, rerun it without avg_salary as a predictor and compare the drop in performance.",
  "",
  "## Stakeholder Audience",
  "**Opening**",
  "This project was designed to explain what the cleaned jobs dataset can say about salaries, job types, and broad role groupings in a clear and cautious way.",
  "",
  "**Key findings**",
  paste0("- ", t_test_result_sentence),
  paste0("- ", regression_result_sentence),
  paste0("- ", classification_result_sentence),
  paste0("- ", clustering_result_sentence),
  "",
  "**What the models mean**",
  "The analysis is useful for understanding broad market patterns and role differences, but it is not strong enough for precise salary prediction at the individual posting level.",
  "",
  "**Limitations**",
  "- The dataset contains many more data scientist roles than smaller job groups.",
  "- Some fields, especially seniority, were sparse or incomplete.",
  "- The clustering output is an exploratory summary rather than a fixed segmentation.",
  "",
  "**Recommendations**",
  "- Use the results to support discussion, benchmarking, and planning for broad job families.",
  "- Improve future data collection for seniority and employer context before making stronger predictive claims.",
  "",
  "## Non-Technical Decision-Makers",
  "**Opening**",
  "The project uses the jobs dataset to identify clear patterns in salary, job category, and skill mix without overstating what the data can prove.",
  "",
  "**Key findings**",
  paste0("- ", t_test_result_sentence),
  "- The salary model was weak, so much of the variation in pay is still unexplained.",
  "- The classifier worked best for the largest role group and less well for smaller ones.",
  paste0("- ", clustering_result_sentence),
  "",
  "**What the models mean**",
  "The results help describe trends in the dataset. They are more suitable for explanation and comparison than for exact prediction.",
  "",
  "**Limitations**",
  "- The dataset is unbalanced across job categories.",
  "- Some useful variables were incomplete.",
  "- The clusters are exploratory groupings, not hard categories.",
  "",
  "**Recommendations**",
  "- Use the findings as evidence for discussion rather than as final proof.",
  "- Support future decisions with more detailed and more balanced data."
)

writeLines(
  presentation_notes_lines,
  file.path(project_paths$written_outputs, "Week 11 Presentation Notes.md")
)

# --------------------------------------------------
# Week 11 Project Snapshot.md
# --------------------------------------------------

project_snapshot_lines <- c(
  paste("#", project_title),
  "",
  paste0("- Project aim: Complete the Week 11 brief with one hypothesis test, one regression model, one classification tree, one k-means clustering analysis, and clear communication."),
  paste0("- Dataset overview: ", nrow(jobs_data), " cleaned postings from `Jobs Clean.csv`, with a strongly imbalanced `job_simp` distribution led by `", top_job_label, "`."),
  paste0("- Hypothesis test: ", t_test_result_sentence),
  paste0("- Regression: ", regression_result_sentence),
  paste0("- Classification: ", classification_result_sentence),
  paste0("- Clustering: ", clustering_result_sentence),
  paste0("- Biggest limitation: ", largest_limitation_sentence),
  paste0("- Final takeaway: ", final_takeaway_sentence)
)

writeLines(
  project_snapshot_lines,
  file.path(project_paths$written_outputs, "Week 11 Project Snapshot.md")
)

# --------------------------------------------------
# README.md
# --------------------------------------------------

readme_lines <- c(
  paste("#", project_title),
  "",
  "## Assignment Purpose",
  "This project finalises the Week 11 jobs mini-project using the cleaned Week 5 dataset work. It covers one hypothesis test, one linear regression model, one classification model, one clustering method, supporting figures, written interpretation, and audience-focused presentation notes.",
  "",
  "## Project Structure",
  "- `Data/Raw/Tabular_DS_Jobs.csv`",
  "- `Data/Clean/Jobs Clean.csv`",
  paste0("- `Scripts/", script_files, "`"),
  "- `Outputs/Tables/`",
  "- `Outputs/Figures/`",
  "- `Outputs/Models/`",
  "- `Week 11 Key Findings.md`",
  "- `Week 11 Presentation Notes.md`",
  "- `Week 11 Project Snapshot.md`",
  "- `README.md`",
  "",
  "## Exact Output File Names",
  "### Tables",
  paste0("- `Outputs/Tables/", table_output_files, "`"),
  "",
  "### Figures",
  paste0("- `Outputs/Figures/", figure_output_files, "`"),
  "",
  "### Models",
  paste0("- `Outputs/Models/", model_output_files, "`"),
  "",
  "## Packages",
  paste0("- `", required_packages, "`"),
  "",
  "## How to Run",
  "1. Open the `Project Week 11` folder in R or RStudio.",
  "2. Run `Scripts/00 Run Entire Project.R` from top to bottom.",
  "3. The runner checks that the input files, scripts, key objects, and main outputs all exist before finishing.",
  "",
  "## Outputs to Inspect First",
  "- `Week 11 Project Snapshot.md` for the shortest project overview.",
  "- `Week 11 Key Findings.md` for the main written summary.",
  "- `Outputs/Figures/Figure 07 Salary by Same State Group.png` for the hypothesis test result.",
  "- `Outputs/Tables/Table 08 Linear Regression Model Fit Statistics.csv` for the weak regression fit summary.",
  "- `Outputs/Figures/Figure 11 Decision Tree for Job Category Classification.png` and `Outputs/Tables/Table 12 Decision Tree Performance Metrics.csv` for the classification result.",
  "- `Outputs/Figures/Figure 14 Cluster Visualisation in Principal Components.png` and `Outputs/Tables/Table 14 Cluster Profile Summary.csv` for the clustering interpretation.",
  "",
  "## Interpretation Warning",
  "- The regression model is descriptive and weak overall, not a strong predictive model.",
  "- The classification tree is affected by class imbalance and should be read cautiously for smaller classes.",
  "- The clustering solution is exploratory and should not be treated as proof of fixed job categories."
)

writeLines(readme_lines, file.path(project_paths$root, "README.md"))

cat("\nWritten communication files created successfully.\n")
