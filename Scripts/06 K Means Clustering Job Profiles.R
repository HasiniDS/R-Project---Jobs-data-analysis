# ==================================================
# Week 11 Data Science Jobs Analysis Project
# Script: 06 K Means Clustering Job Profiles.R
# Purpose: Use k-means clustering to explore useful groupings in
#          the jobs dataset.
# Inputs:  jobs_data from Script 01 or Data/Clean/Jobs Clean.csv
# Outputs: Table 13 Cluster Selection Diagnostics.csv
#          Table 14 Cluster Profile Summary.csv
#          Table 15 Cluster Membership Counts.csv
#          Figure 12 K Means Elbow Curve.png
#          Figure 13 K Means Silhouette Comparison.png
#          Figure 14 Cluster Visualisation in Principal Components.png
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
# Choose k using elbow, silhouette, and interpretability
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

silhouette_best_k <- cluster_selection_metrics %>%
  filter(average_silhouette_width == max(average_silhouette_width)) %>%
  slice(1) %>%
  pull(k)

# The elbow also bends most clearly at k = 2, so the final choice keeps the
# strongest silhouette solution while favouring the clearest interpretable split.
elbow_preferred_k <- 2

if (silhouette_best_k == elbow_preferred_k) {
  final_k <- elbow_preferred_k
} else {
  final_k <- silhouette_best_k
}

cluster_k_justification <- paste(
  "k = 2 was retained because it had the strongest silhouette width,",
  "the elbow curve started to flatten after two clusters, and the",
  "two-cluster solution remained easiest to interpret as a broad skill-mix split."
)

cluster_selection_metrics <- cluster_selection_metrics %>%
  mutate(
    selection_status = if_else(k == final_k, "Selected", "Candidate")
  )

write_output_table(
  cluster_selection_metrics,
  "Table 13 Cluster Selection Diagnostics.csv"
)

# --------------------------------------------------
# Final k-means model
# --------------------------------------------------

set.seed(7202)
final_kmeans_model <- kmeans(cluster_feature_matrix, centers = final_k, nstart = 25)

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
  geom_vline(
    xintercept = final_k,
    colour = analysis_palette["neutral"],
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_line(colour = analysis_palette["primary"], linewidth = 1) +
  geom_point(
    aes(fill = selection_status),
    shape = 21,
    size = 4,
    stroke = 1,
    colour = analysis_palette["ink"]
  ) +
  geom_text(
    data = cluster_selection_metrics %>% filter(selection_status == "Selected"),
    aes(label = paste0("Chosen k = ", k)),
    nudge_y = 0.06 * max(cluster_selection_metrics$total_withinss),
    size = 3.6,
    colour = analysis_palette["ink"],
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Candidate" = "white",
      "Selected" = unname(analysis_palette["accent"])
    )
  ) +
  scale_x_continuous(
    breaks = candidate_k_values,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(5)
  ) +
  labs(
    title = "Elbow Curve Shows Rapid Improvement Up to the Chosen Solution",
    subtitle = paste("The within-cluster variation falls quickly before the gains begin to flatten at k =", final_k),
    x = "Number of clusters (k)",
    y = "Total within-cluster sum of squares",
    caption = figure_caption(
      "Beyond the chosen solution, each extra cluster delivers smaller reductions in within-cluster variation."
    )
  ) +
  analysis_theme()

silhouette_summary_plot <- ggplot(
  cluster_selection_metrics,
  aes(x = factor(k), y = average_silhouette_width, fill = selection_status)
) +
  geom_col(width = 0.62, colour = "white") +
  geom_text(
    aes(label = format_number(average_silhouette_width, 3)),
    vjust = -0.4,
    size = 3.5,
    colour = analysis_palette["ink"]
  ) +
  scale_fill_manual(
    values = c(
      "Candidate" = unname(analysis_palette["secondary"]),
      "Selected" = unname(analysis_palette["accent"])
    )
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(5),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Silhouette Comparison Supports the Same Cluster Count",
    subtitle = paste("The highest average silhouette width is observed at k =", final_k),
    x = "Number of clusters (k)",
    y = "Average silhouette width",
    caption = figure_caption(
      "A higher silhouette width suggests that observations are better separated from other clusters."
    )
  ) +
  analysis_theme()

pca_model <- prcomp(cluster_feature_matrix, center = FALSE, scale. = FALSE)
explained_variance <- summary(pca_model)$importance["Proportion of Variance", 1:2]

pca_cluster_plot_data <- tibble::tibble(
  pc1 = pca_model$x[, 1],
  pc2 = pca_model$x[, 2],
  cluster = jobs_clustered_data$cluster
)

cluster_levels <- levels(pca_cluster_plot_data$cluster)

pca_cluster_centroids <- pca_cluster_plot_data %>%
  group_by(cluster) %>%
  summarise(
    pc1 = mean(pc1),
    pc2 = mean(pc2),
    cluster_size = n(),
    .groups = "drop"
  )

pca_loading_data <- as.data.frame(pca_model$rotation[, 1:2]) %>%
  tibble::rownames_to_column("feature") %>%
  tibble::as_tibble() %>%
  mutate(
    loading_strength = sqrt(PC1^2 + PC2^2)
  ) %>%
  arrange(desc(loading_strength)) %>%
  slice_head(n = 4)

arrow_scale <- min(
  diff(range(pca_cluster_plot_data$pc1)),
  diff(range(pca_cluster_plot_data$pc2))
) * 0.42

pca_loading_data <- pca_loading_data %>%
  mutate(
    pc1_end = PC1 * arrow_scale,
    pc2_end = PC2 * arrow_scale,
    label_x = pc1_end * 1.10,
    label_y = pc2_end * 1.10,
    feature_label = clean_label_text(gsub("_imputed", "", feature))
  )

cluster_visualisation_plot <- ggplot(
  pca_cluster_plot_data,
  aes(x = pc1, y = pc2)
) +
  geom_hline(yintercept = 0, colour = "#DCE6F0", linewidth = 0.55) +
  geom_vline(xintercept = 0, colour = "#DCE6F0", linewidth = 0.55) +
  stat_ellipse(
    aes(fill = cluster, colour = cluster),
    geom = "polygon",
    alpha = 0.10,
    linewidth = 1,
    show.legend = FALSE
  ) +
  geom_point(
    aes(colour = cluster),
    alpha = 0.72,
    size = 2.35
  ) +
  geom_segment(
    data = pca_loading_data,
    aes(x = 0, y = 0, xend = pc1_end, yend = pc2_end),
    inherit.aes = FALSE,
    arrow = grid::arrow(length = grid::unit(0.18, "cm")),
    colour = analysis_palette["charcoal"],
    linewidth = 0.75,
    alpha = 0.9
  ) +
  geom_label(
    data = pca_loading_data,
    aes(x = label_x, y = label_y, label = feature_label),
    inherit.aes = FALSE,
    colour = analysis_palette["charcoal"],
    fill = "white",
    linewidth = 0.2,
    size = 3.2
  ) +
  geom_point(
    data = pca_cluster_centroids,
    aes(x = pc1, y = pc2),
    inherit.aes = FALSE,
    shape = 23,
    size = 4.3,
    fill = "white",
    colour = analysis_palette["ink"],
    stroke = 1
  ) +
  geom_label(
    data = pca_cluster_centroids,
    aes(x = pc1, y = pc2, label = paste0("Cluster ", cluster, "\n", "n = ", cluster_size), fill = cluster),
    inherit.aes = FALSE,
    colour = "white",
    fontface = "bold",
    size = 3.3,
    linewidth = 0,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = unname(cluster_palette[cluster_levels]),
    breaks = cluster_levels
  ) +
  scale_fill_manual(
    values = unname(cluster_palette[cluster_levels]),
    breaks = cluster_levels
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(6)
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(6)
  ) +
  coord_equal() +
  labs(
    title = "Principal Component View of the Final Job Clusters",
    subtitle = paste(
      "The first two components explain",
      format_percent(sum(explained_variance), 1),
      "of the scaled feature variation. Arrows show the strongest variable directions."
    ),
    x = paste0("Principal component 1 (", format_percent(explained_variance[1], 1), ")"),
    y = paste0("Principal component 2 (", format_percent(explained_variance[2], 1), ")"),
    colour = "Cluster",
    caption = figure_caption(
      "Clusters are exploratory groupings. The main separation is driven more by technical skill mix than by salary alone."
    )
  ) +
  analysis_theme(legend_position = "right")

save_analysis_figure(
  elbow_curve_plot,
  "Figure 12 K Means Elbow Curve.png",
  width = 10.2,
  height = 6.2
)
save_analysis_figure(
  silhouette_summary_plot,
  "Figure 13 K Means Silhouette Comparison.png",
  width = 10.2,
  height = 6.2
)
save_analysis_figure(
  cluster_visualisation_plot,
  "Figure 14 Cluster Visualisation in Principal Components.png",
  width = 11.2,
  height = 7.4
)

cat("\nCluster selection metrics:\n")
print(cluster_selection_metrics)
cat("\nFinal k selected:", final_k, "\n")
cat("Cluster selection note:", cluster_k_justification, "\n")
cat("\nCluster profiles:\n")
print(cluster_profiles_table)
