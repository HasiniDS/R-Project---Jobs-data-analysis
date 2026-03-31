# ==================================================
# Jobs Analyse
# Script: 07 Written Summary and Presentation Notes.R
# Purpose: Create the written project outputs for submission and
#          presentation use.
# Inputs:  Results objects created in earlier scripts
# Outputs: Presentation/Week 11 Key Findings and Interpretation.md
#          Presentation/Week 11 Presentation Speaking Notes.md
#          README.md
# ==================================================

if (!exists("jobs_data")) {
  jobs_data <- readr::read_csv(clean_data_path, show_col_types = FALSE)
}

# --------------------------------------------------
# Helper text
# --------------------------------------------------

top_job_count <- jobs_data %>%
  count(job_simp, sort = TRUE) %>%
  slice(1)

t_test_p_value <- t_test_results_table$p_value[1]
t_test_difference <- t_test_results_table$mean_difference[1]
t_test_effect_size <- t_test_results_table$cohens_d[1]
t_test_interpretation <- if (t_test_p_value < 0.05) {
  paste0(
    "Average salary differed between the same-state groups. ",
    "Postings in the same state as headquarters were ",
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
    "between the same-state groups (p = ",
    format_p_value(t_test_p_value),
    ")."
  )
}

regression_r_squared <- regression_fit_table$r_squared[1]
regression_adjusted_r_squared <- regression_fit_table$adjusted_r_squared[1]
regression_model_p_value <- regression_fit_table$model_p_value[1]

important_regression_terms <- regression_coefficients_table %>%
  filter(term != "(Intercept)") %>%
  arrange(p.value) %>%
  slice_head(n = 3)

describe_regression_term <- function(term_name) {
  dplyr::case_when(
    term_name == "rating" ~ "higher company ratings",
    term_name == "company_age_imputed" ~ "older companies",
    term_name == "same_state" ~ "same-state postings",
    term_name == "python" ~ "roles asking for Python",
    term_name == "excel" ~ "roles asking for Excel",
    term_name == "hadoop" ~ "roles asking for Hadoop",
    term_name == "spark" ~ "roles asking for Spark",
    term_name == "aws" ~ "roles asking for AWS",
    term_name == "tableau" ~ "roles asking for Tableau",
    term_name == "big_data" ~ "roles mentioning big data",
    grepl("^job_simp_grouped", term_name) ~ {
      comparison_label <- gsub("^job_simp_grouped", "", term_name)
      comparison_label <- gsub("_", " ", comparison_label)
      paste0(comparison_label, " roles compared with data scientist roles")
    },
    TRUE ~ term_name
  )
}

important_regression_text <- paste(
  vapply(
    important_regression_terms$term,
    describe_regression_term,
    character(1)
  ),
  collapse = ", "
)

regression_interpretation <- if (regression_model_p_value < 0.05) {
  paste0(
    "The linear regression model explained only a limited share of salary variation ",
    "(R-squared = ", format_number(regression_r_squared, 3),
    ", adjusted R-squared = ", format_number(regression_adjusted_r_squared, 3),
    "). The lowest p-value terms were ", important_regression_text,
    ", so the model offers some descriptive direction but still leaves much of salary variation unexplained."
  )
} else {
  paste0(
    "The linear regression model was weak overall (R-squared = ",
    format_number(regression_r_squared, 3), ", adjusted R-squared = ",
    format_number(regression_adjusted_r_squared, 3),
    "; model p = ", format_p_value(regression_model_p_value), "). ",
    "Only limited coefficient evidence emerged, with same-state postings being the clearest single signal. ",
    "This model is best treated as a descriptive check rather than a strong explanatory model."
  )
}

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

classification_interpretation <- if (
  classification_accuracy > majority_class_baseline
) {
  paste0(
    "The decision tree reached an overall test accuracy of ",
    format_number(classification_accuracy, 3),
    ", which was only slightly above the majority-class baseline of ",
    format_number(majority_class_baseline, 3),
    ". The most influential variables were ",
    paste(top_tree_variables, collapse = ", "),
    ", but the weakest recall was for `", lowest_recall_class$class,
    "`, so the smaller classes remained difficult to separate."
  )
} else {
  paste0(
    "The decision tree reached an overall test accuracy of ",
    format_number(classification_accuracy, 3),
    ", which did not clearly outperform a simple majority-class baseline. ",
    "The most influential variables were ",
    paste(top_tree_variables, collapse = ", "),
    ", but minority classes were still predicted inconsistently."
  )
}

cluster_salary_order <- cluster_profiles_table %>%
  arrange(desc(mean_avg_salary))

highest_salary_cluster <- cluster_salary_order %>% slice(1)
lowest_salary_cluster <- cluster_salary_order %>% slice(n())

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

cluster_summary_text <- paste0(
  "The final k-means solution used k = ", final_k,
  ". The highest-paying cluster was Cluster ", highest_salary_cluster$cluster,
  " (mean salary ", format_number(highest_salary_cluster$mean_avg_salary, 1),
  " thousand USD), while Cluster ", lowest_salary_cluster$cluster,
  " had the lowest mean salary at ",
  format_number(lowest_salary_cluster$mean_avg_salary, 1),
  " thousand USD. The clearest differences between clusters were in ",
  paste(key_cluster_skills, collapse = " and "),
  ", which suggests an engineering-heavy cluster and a broader general cluster. ",
  "This should still be treated as exploratory rather than definitive."
)

# --------------------------------------------------
# Week 11 Key Findings and Interpretation.md
# --------------------------------------------------

key_findings_lines <- c(
  "# Week 11 Key Findings",
  "",
  "## Project Aim",
  "This project analysed the cleaned data science jobs dataset to test one hypothesis, fit one linear regression model, build one classification model, and explore job groupings with clustering.",
  "",
  "## Dataset Overview",
  paste0(
    "The analysis used ", nrow(jobs_data), " cleaned job postings with ", ncol(jobs_data),
    " variables. The largest job category was `", top_job_count$job_simp,
    "` with ", top_job_count$n, " postings, which confirms that the dataset is imbalanced."
  ),
  "The `seniority` field was mostly recorded as `na`, so it was described but not used as a main modelling target. Company ratings of 0 were kept because they represent unrated firms rather than missing values.",
  "",
  "## Main Hypothesis Test Finding",
  t_test_interpretation,
  "",
  "## Main Regression Finding",
  regression_interpretation,
  "",
  "## Main Classification Finding",
  classification_interpretation,
  "",
  "## Main Clustering Finding",
  cluster_summary_text,
  "",
  "## Limitations",
  "- `job_simp` was highly imbalanced, which affected both the classification results and the interpretation of grouped summaries.",
  "- `company_age` included missing values, so a simple median imputation was used in the modelling stages to keep the sample size stable.",
  "- `seniority` was too sparse to support reliable modelling.",
  "- The regression model was descriptive rather than causal, and the clustering solution should be treated as exploratory.",
  "",
  "## Conclusion",
  "Across the project, salary patterns were related to job type and some skill indicators, but the dataset still contained substantial unexplained variation. The most defensible conclusion is that the models provide useful structure for understanding the jobs market, while their practical predictions should be treated with caution."
)

writeLines(
  key_findings_lines,
  file.path(
    project_paths$presentation,
    "Week 11 Key Findings and Interpretation.md"
  )
)

# --------------------------------------------------
# Week 11 Presentation Speaking Notes.md
# --------------------------------------------------

presentation_notes_lines <- c(
  "# Week 11 Presentation Notes",
  "",
  "## 1. Technical Audience",
  "**Opening**",
  "This analysis used the cleaned data science jobs dataset to apply a Welch t-test, one linear regression model, one decision tree classifier, and one k-means clustering model.",
  "",
  "**Key findings**",
  paste0("- The hypothesis test result was: ", t_test_interpretation),
  paste0(
    "- The regression explained ", format_number(regression_r_squared, 3),
    " of the variance in `avg_salary`, so it should be treated as a weak descriptive model."
  ),
  paste0(
    "- The classification tree reached ", format_number(classification_accuracy, 3),
    " test accuracy, only slightly above the majority-class baseline of ",
    format_number(majority_class_baseline, 3), "."
  ),
  paste0(
    "- The clustering stage selected k = ", final_k,
    " using elbow and silhouette checks, and the strongest separation came from skill-mix differences rather than salary alone."
  ),
  "",
  "**What the models mean**",
  "The test suggests whether salary differs across a simple geographic grouping. The regression shows conditional salary associations. The tree shows which variables help separate job types. The clustering result gives an exploratory structure for role patterns.",
  "",
  "**Limitations**",
  "- Seniority was too sparse for serious modelling use.",
  "- `job_simp` imbalance affected the classifier.",
  "- The regression left a large share of salary variation unexplained.",
  "- Clustering should not be interpreted as proof of fixed job types.",
  "",
  "**Recommendations**",
  "- Add richer predictors such as experience level, location detail, and employer characteristics in future work.",
  "- Consider class-balancing strategies or alternative classifiers if job-type prediction becomes the main objective.",
  "",
  "## 2. Stakeholder Audience",
  "**Opening**",
  "This project looked at what the cleaned jobs data can tell us about salary patterns, job categories, and broad role groupings.",
  "",
  "**Key findings**",
  paste0("- ", t_test_interpretation),
  paste0(
    "- The salary model was weak overall and explained only a small part of the picture (R-squared = ",
    format_number(regression_r_squared, 3), ")."
  ),
  paste0(
    "- The job-type classifier was easiest for the largest class and less reliable for smaller role groups, even though overall accuracy was ",
    format_number(classification_accuracy, 3), " and the majority-class baseline was ",
    format_number(majority_class_baseline, 3), "."
  ),
  paste0("- ", cluster_summary_text),
  "",
  "**What the models mean**",
  "The results are useful for understanding broad patterns in the jobs market. They help compare role types and skills, but they are not detailed enough to make precise salary predictions for individual vacancies.",
  "",
  "**Limitations**",
  "- Some variables were missing or weakly populated.",
  "- Smaller job categories were under-represented.",
  "- The clustering output is best used as a planning aid rather than a final segmentation.",
  "",
  "**Recommendations**",
  "- Focus salary benchmarking on the larger and clearer job categories first.",
  "- Improve future data collection for seniority and employer details.",
  "",
  "## 3. Non-Technical Decision-Makers",
  "**Opening**",
  "The dataset was used to look for simple patterns in salary, job type, and skills across data science vacancies.",
  "",
  "**Key findings**",
  paste0("- ", t_test_interpretation),
  "- The salary model was weak, so there is still a lot we cannot explain from this dataset alone.",
  "- The job classifier worked better for common roles than uncommon ones, so it should not be treated as a perfect sorting tool.",
  paste0(
    "- The cluster analysis grouped jobs into ", final_k,
    " broad patterns, mainly separating roles by skill mix and only slightly by salary."
  ),
  "",
  "**What the models mean**",
  "These results are helpful for spotting trends. They are less suitable for making exact predictions about a single company or vacancy.",
  "",
  "**Limitations**",
  "- Some fields were incomplete.",
  "- The dataset had many more data scientist roles than other job types.",
  "- The clusters are exploratory groupings rather than fixed categories.",
  "",
  "**Recommendations**",
  "- Use the findings to guide discussion and planning, not as final evidence on their own.",
  "- Collect more balanced and detailed data before making high-stakes decisions."
)

writeLines(
  presentation_notes_lines,
  file.path(
    project_paths$presentation,
    "Week 11 Presentation Speaking Notes.md"
  )
)

# --------------------------------------------------
# README.md
# --------------------------------------------------

readme_lines <- c(
  "# Jobs Analyse",
  "",
  "## Assignment Context",
  "This project finalises the Week 11 group activity described in the module email and the file titled `Week 11 BIG Task`.",
  "It builds on the Week 5 jobs dataset work and is organised as a clear presentation-ready hand-in for a group presentation to the teaching team.",
  "The analysis covers one hypothesis test, one linear regression model, one classification model, one clustering method, interpretation, audience-focused communication, and reflection.",
  "",
  "## Project Title",
  "Jobs Analyse: Week 11 modelling, interpretation, and communication using the cleaned data science jobs dataset.",
  "",
  "## File Structure",
  "```text",
  "Project Week 11/",
  "|-- Data/",
  "|   |-- Raw/",
  "|   |   `-- Data Science Jobs Dataset - Raw.csv",
  "|   `-- Clean/",
  "|       `-- Data Science Jobs Dataset - Clean.csv",
  "|-- Scripts/",
  "|   |-- 00 Run Entire Project.R",
  "|   |-- 01 Data Quality Review.R",
  "|   |-- 02 Descriptive Figures.R",
  "|   |-- 03 Hypothesis Test - Salary by Location Match.R",
  "|   |-- 04 Linear Regression - Average Salary.R",
  "|   |-- 05 Classification Tree - Job Category.R",
  "|   |-- 06 K Means Clustering - Job Profiles.R",
  "|   `-- 07 Written Summary and Presentation Notes.R",
  "|-- Outputs/",
  "|   |-- Tables/",
  "|   |-- Figures/",
  "|   `-- Models/",
  "|-- Presentation/",
  "|   |-- Week 11 Key Findings and Interpretation.md",
  "|   `-- Week 11 Presentation Speaking Notes.md",
  "`-- README.md",
  "```",
  "",
  "## Required Packages",
  "- dplyr",
  "- ggplot2",
  "- tidyr",
  "- readr",
  "- broom",
  "- car",
  "- rpart",
  "- rpart.plot",
  "- cluster",
  "- forcats",
  "- scales",
  "",
  "## How to Run the Project",
  "1. Open the `Project Week 11` folder in R or RStudio.",
  "2. Run `Scripts/00 Run Entire Project.R` from top to bottom.",
  "3. Check the `Outputs` and `Presentation` folders for the saved results.",
  "",
  "## What a Lecturer Can Review Quickly",
  "- `Presentation/Week 11 Key Findings and Interpretation.md` for the short analytical summary.",
  "- `Presentation/Week 11 Presentation Speaking Notes.md` for the audience-specific speaking notes.",
  "- `Outputs/Tables/` for the saved statistical results.",
  "- `Outputs/Figures/` for the supporting visuals used in the presentation.",
  "",
  "## Output Description",
  "- `Outputs/Tables/` contains dataset checks, descriptive summaries, model tables, and clustering summaries.",
  "- `Outputs/Figures/` contains the analytical visualisations used in the report and presentation.",
  "- `Outputs/Models/` stores the saved regression, tree, and clustering models.",
  "- `Presentation/` contains a key findings summary and presentation notes for different audiences."
)

writeLines(readme_lines, file.path(project_paths$root, "README.md"))

cat("\nWritten communication files created successfully.\n")
