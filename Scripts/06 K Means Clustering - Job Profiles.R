# ==================================================
# Jobs Analyse
# Script: 06 K Means Clustering - Job Profiles.R
# Purpose: Use k-means clustering to explore useful groupings in
#          the jobs dataset.
# Inputs:  jobs_data from Script 01 or Data/Clean/Data Science Jobs Dataset - Clean.csv
# Outputs: Table 13 Cluster Selection Diagnostics.csv
#          Table 14 Cluster Profile Summary.csv
#          Table 15 Cluster Membership Counts.csv
#          Figure 12 K Means Elbow Curve.png
#          Figure 13 K Means Silhouette Comparison.png
#          Figure 14 Cluster Visualisation in Principal Components.png
#          Outputs/Models/K Means Clustering Model - Job Profiles.rds
# ==================================================

if (!exists("jobs_data")) {
  jobs_data <- readr::read_csv(clean_data_path, show_col_types = FALSE)
}

jobs_clustering_data <- add_model_fields(jobs_data)

# --------------------------------------------------
# Prepare clustering features
# --------------------------------------------------

cluster_feature_data <- jobs_clustering_data %>%
  transmute(
    avg_salary = avg_salary,
    rating = rating,
    company_age_imputed = company_age_imputed,
    same_state = same_state,
    python = python,
    excel = excel,
    hadoop = hadoop,
    spark = spark,
    aws = aws,
    tableau = tableau,
    big_data = big_data
  )

cluster_feature_matrix <- scale(cluster_feature_data)
cluster_distance <- dist(cluster_feature_matrix)

# --------------------------------------------------
# Choose k using elbow and silhouette checks
# --------------------------------------------------

candidate_k_values <- 2:6

cluster_selection_metrics <- lapply(candidate_k_values, function(k_value) {
  set.seed(7202)
  kmeans_fit <- kmeans(cluster_feature_matrix, centers = k_value, nstart = 25)
  silhouette_width <- cluster::silhouette(kmeans_fit$cluster, cluster_distance)

  tibble::tibble(
    k = k_value,
    total_withinss = kmeans_fit$tot.withinss,
    average_silhouette_width = mean(silhouette_width[, "sil_width"])
  )
}) %>%
  bind_rows()

final_k <- cluster_selection_metrics %>%
  filter(average_silhouette_width == max(average_silhouette_width)) %>%
  slice(1) %>%
  pull(k)

write_output_table(
  cluster_selection_metrics,
  "Table 13 Cluster Selection Diagnostics.csv"
)

# --------------------------------------------------
# Final k-means model
# --------------------------------------------------

set.seed(7202)
final_kmeans_model <- kmeans(cluster_feature_matrix, centers = final_k, nstart = 25)

saveRDS(
  final_kmeans_model,
  file.path(
    project_paths$outputs_models,
    "K Means Clustering Model - Job Profiles.rds"
  )
)

jobs_clustered_data <- jobs_clustering_data %>%
  mutate(cluster = factor(final_kmeans_model$cluster))

cluster_sizes_table <- jobs_clustered_data %>%
  count(cluster, name = "cluster_size") %>%
  arrange(cluster)

cluster_profiles_table <- jobs_clustered_data %>%
  group_by(cluster) %>%
  summarise(
    cluster_size = n(),
    mean_avg_salary = mean(avg_salary, na.rm = TRUE),
    mean_rating = mean(rating, na.rm = TRUE),
    mean_company_age = mean(company_age_imputed, na.rm = TRUE),
    same_state_rate = mean(same_state, na.rm = TRUE),
    python_rate = mean(python, na.rm = TRUE),
    excel_rate = mean(excel, na.rm = TRUE),
    hadoop_rate = mean(hadoop, na.rm = TRUE),
    spark_rate = mean(spark, na.rm = TRUE),
    aws_rate = mean(aws, na.rm = TRUE),
    tableau_rate = mean(tableau, na.rm = TRUE),
    big_data_rate = mean(big_data, na.rm = TRUE),
    dominant_job_simp = names(sort(table(job_simp), decreasing = TRUE))[1],
    .groups = "drop"
  )

write_output_table(
  cluster_profiles_table,
  "Table 14 Cluster Profile Summary.csv"
)
write_output_table(
  cluster_sizes_table,
  "Table 15 Cluster Membership Counts.csv"
)

# --------------------------------------------------
# Clustering plots
# --------------------------------------------------

elbow_curve_plot <- ggplot(
  cluster_selection_metrics,
  aes(x = k, y = total_withinss)
) +
  geom_line(colour = analysis_palette["primary"], linewidth = 0.9) +
  geom_point(colour = analysis_palette["primary"], size = 2.5) +
  geom_vline(xintercept = final_k, linetype = "dashed", colour = analysis_palette["accent"]) +
  scale_x_continuous(breaks = candidate_k_values) +
  labs(
    title = "Elbow Curve for K-Means Clustering",
    subtitle = paste("Selected solution: k =", final_k),
    x = "Number of clusters (k)",
    y = "Total within-cluster sum of squares"
  ) +
  analysis_theme()

silhouette_summary_plot <- ggplot(
  cluster_selection_metrics,
  aes(x = k, y = average_silhouette_width)
) +
  geom_line(colour = analysis_palette["secondary"], linewidth = 0.9) +
  geom_point(colour = analysis_palette["secondary"], size = 2.5) +
  geom_vline(xintercept = final_k, linetype = "dashed", colour = analysis_palette["accent"]) +
  scale_x_continuous(breaks = candidate_k_values) +
  labs(
    title = "Average Silhouette Width by Cluster Count",
    subtitle = paste("Highest average silhouette width at k =", final_k),
    x = "Number of clusters (k)",
    y = "Average silhouette width"
  ) +
  analysis_theme()

pca_model <- prcomp(cluster_feature_matrix, center = FALSE, scale. = FALSE)

pca_cluster_plot_data <- tibble::tibble(
  pc1 = pca_model$x[, 1],
  pc2 = pca_model$x[, 2],
  cluster = jobs_clustered_data$cluster
)

cluster_visualisation_plot <- ggplot(
  pca_cluster_plot_data,
  aes(x = pc1, y = pc2, colour = cluster)
) +
  geom_point(alpha = 0.75, size = 2.1) +
  stat_ellipse(linewidth = 0.8, alpha = 0.35) +
  scale_colour_manual(values = unname(c(
    analysis_palette["primary"],
    analysis_palette["secondary"],
    analysis_palette["accent"],
    analysis_palette["neutral"],
    "#4F7C5C",
    "#9368B7"
  ))) +
  labs(
    title = "PCA View of the Final K-Means Clusters",
    x = "Principal component 1",
    y = "Principal component 2",
    colour = "Cluster"
  ) +
  analysis_theme() +
  theme(legend.position = "right")

save_analysis_figure(
  elbow_curve_plot,
  "Figure 12 K Means Elbow Curve.png"
)
save_analysis_figure(
  silhouette_summary_plot,
  "Figure 13 K Means Silhouette Comparison.png"
)
save_analysis_figure(
  cluster_visualisation_plot,
  "Figure 14 Cluster Visualisation in Principal Components.png"
)

cat("\nCluster selection metrics:\n")
print(cluster_selection_metrics)
cat("\nFinal k selected:", final_k, "\n")
cat("\nCluster profiles:\n")
print(cluster_profiles_table)
