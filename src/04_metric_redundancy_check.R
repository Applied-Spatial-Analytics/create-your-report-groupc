library(ggplot2)

dir.create("data/results/metric_selection", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/results/metric_selection", recursive = TRUE, showWarnings = FALSE)

delft <- read.csv("data/results/delft_grid_metrics.csv")
xian <- read.csv("data/results/xian_grid_metrics.csv")

delft$city <- "Delft"
xian$city <- "Xi'an"

all_data <- rbind(delft, xian)

candidate_metrics <- c(
  "green_percent",
  "area",
  "gyrate",
  "contig",
  "enn",
  "fwei_change_mean",
  "flood_share",
  "pland_flood",
  "np_flood",
  "flood_cohesion",
  "flood_clumpy",
  "flood_lpi",
  "mean_dist_green",
  "min_dist_green",
  "elevation_mean",
  "elevation_min",
  "slope_mean"
)

available_metrics <- candidate_metrics[candidate_metrics %in% names(all_data)]

metric_data <- all_data[, available_metrics]

for (m in names(metric_data)) {
  metric_data[[m]] <- as.numeric(metric_data[[m]])
  metric_data[[m]][is.nan(metric_data[[m]])] <- NA
  metric_data[[m]][is.infinite(metric_data[[m]])] <- NA
}

usable_metrics <- names(metric_data)[
  sapply(metric_data, function(x) {
    sum(!is.na(x)) >= 10 && sd(x, na.rm = TRUE) > 0
  })
]

metric_data <- metric_data[, usable_metrics]

cor_matrix <- cor(
  metric_data,
  use = "pairwise.complete.obs",
  method = "pearson"
)

write.csv(
  cor_matrix,
  "data/results/metric_selection/metric_redundancy_matrix.csv"
)

cor_pairs <- data.frame()

metric_names <- colnames(cor_matrix)

for (i in 1:(length(metric_names) - 1)) {
  for (j in (i + 1):length(metric_names)) {
    cor_pairs <- rbind(
      cor_pairs,
      data.frame(
        metric_1 = metric_names[i],
        metric_2 = metric_names[j],
        correlation = cor_matrix[i, j],
        abs_correlation = abs(cor_matrix[i, j])
      )
    )
  }
}

cor_pairs <- cor_pairs[order(-cor_pairs$abs_correlation), ]

high_cor_pairs <- cor_pairs[
  !is.na(cor_pairs$abs_correlation) &
    cor_pairs$abs_correlation >= 0.80,
]

write.csv(
  cor_pairs,
  "data/results/metric_selection/all_metric_correlations.csv",
  row.names = FALSE
)

write.csv(
  high_cor_pairs,
  "data/results/metric_selection/highly_correlated_metrics.csv",
  row.names = FALSE
)

metric_notes <- data.frame(
  metric = candidate_metrics,
  included_in_check = candidate_metrics %in% usable_metrics,
  current_thought = c(
    "keep: green amount",
    "keep: green patch size",
    "possibly remove: similar to patch size/spatial extent",
    "keep: green compactness/connectivity",
    "keep: green patch isolation",
    "keep: average FWEI change",
    "keep: detected water extent",
    "possibly remove: similar to flood_share",
    "keep: number of flood patches",
    "possibly remove: similar to other connectedness metrics",
    "keep: flood clustering",
    "possibly remove: similar to flood extent/dominance",
    "possibly remove from typology: distance metric, useful for interpretation",
    "possibly remove from typology: distance metric, useful for interpretation",
    "keep: average elevation",
    "possibly remove: similar to elevation_mean",
    "keep: average slope"
  )
)

write.csv(
  metric_notes,
  "data/results/metric_selection/metric_notes_before_final_selection.csv",
  row.names = FALSE
)

cor_long <- as.data.frame(as.table(cor_matrix))
names(cor_long) <- c("metric_1", "metric_2", "correlation")

p <- ggplot(cor_long, aes(x = metric_1, y = metric_2, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), size = 2.5) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Metric redundancy check",
    subtitle = "Delft and Xi'an combined",
    x = NULL,
    y = NULL,
    fill = "Pearson r"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  "figures/results/metric_selection/metric_redundancy_heatmap.png",
  p,
  width = 10,
  height = 8,
  dpi = 300
)

print(high_cor_pairs)

message("Metric redundancy check complete.")
message("Check this file first: data/results/metric_selection/highly_correlated_metrics.csv")
message("Also check this figure: figures/results/metric_selection/metric_redundancy_heatmap.png")
