library(sf)
library(ggplot2)
library(dplyr)
library(readr)

dir.create("figures/results", recursive = TRUE, showWarnings = FALSE)

delft <- st_read("data/results/delft_grid_metrics.gpkg", quiet = TRUE)
xian <- st_read("data/results/xian_grid_metrics.gpkg", quiet = TRUE)

green_metrics <- c("green_percent", "area", "gyrate", "contig", "enn", "mean_dist_green")
flood_metrics <- c("fwei_change_mean", "flood_share")

optional_metrics <- c(
  "heavy_rain_mean",
  "heavy_rain_max",
  "elevation_mean",
  "elevation_min",
  "slope_mean"
)

make_map <- function(data, column, title, filename) {
  if (!column %in% names(data)) {
    message("Skipping missing column: ", column)
    return(NULL)
  }

  p <- ggplot(data) +
    geom_sf(aes(fill = .data[[column]]), color = NA) +
    scale_fill_viridis_c(na.value = "grey90") +
    labs(title = title, fill = column) +
    theme_minimal()

  ggsave(filename, p, width = 8, height = 6, dpi = 300)
}

for (m in c(green_metrics, flood_metrics, optional_metrics)) {
  make_map(delft, m, paste("Delft", m), paste0("figures/results/delft_", m, ".png"))
  make_map(xian, m, paste("Xi'an", m), paste0("figures/results/xian_", m, ".png"))
}

make_correlations <- function(data, city_name) {
  metrics <- green_metrics[green_metrics %in% names(data)]

  if ("heavy_rain_mean" %in% names(data)) {
    targets <- c("fwei_change_mean", "flood_share", "heavy_rain_mean")
  } else {
    targets <- c("fwei_change_mean", "flood_share")
  }

  output <- data.frame()

  for (target in targets) {
    for (metric in metrics) {
      value <- cor(
        data[[metric]],
        data[[target]],
        use = "complete.obs"
      )

      output <- rbind(
        output,
        data.frame(
          city = city_name,
          metric = metric,
          target = target,
          correlation = value
        )
      )
    }
  }

  return(output)
}

cor_all <- bind_rows(
  make_correlations(delft, "Delft"),
  make_correlations(xian, "Xi'an")
)

write.csv(
  cor_all,
  "data/results/correlations_all.csv",
  row.names = FALSE
)

p_cor <- ggplot(cor_all, aes(x = metric, y = correlation, fill = city)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  facet_wrap(~ target) +
  labs(
    title = "Correlation between green metrics and flood-related indicators",
    x = "Green-space metric",
    y = "Pearson correlation",
    fill = "City"
  ) +
  theme_minimal()

ggsave(
  "figures/results/correlation_overview.png",
  p_cor,
  width = 10,
  height = 6,
  dpi = 300
)

typology_vars <- c("green_percent", "area", "gyrate", "contig", "enn")

make_typology <- function(data, city_name, output_gpkg, output_png) {
  values <- data %>%
    st_drop_geometry() %>%
    select(all_of(typology_vars))

  values[is.na(values)] <- 0
  values_scaled <- scale(values)

  set.seed(123)
  km <- kmeans(values_scaled, centers = 4, nstart = 25)

  data$typology <- factor(
    km$cluster,
    labels = paste0("Typology ", 1:4)
  )

  st_write(data, output_gpkg, delete_dsn = TRUE)

  p <- ggplot(data) +
    geom_sf(aes(fill = typology), color = NA) +
    labs(title = paste("Green-space typologies in", city_name), fill = "Typology") +
    theme_minimal()

  ggsave(output_png, p, width = 8, height = 6, dpi = 300)

  return(data)
}

delft_typology <- make_typology(
  delft,
  "Delft",
  "data/results/delft_typology.gpkg",
  "figures/results/delft_typology.png"
)

xian_typology <- make_typology(
  xian,
  "Xi'an",
  "data/results/xian_typology.gpkg",
  "figures/results/xian_typology.png"
)

typology_summary <- bind_rows(
  delft_typology %>%
    st_drop_geometry() %>%
    mutate(city = "Delft"),
  xian_typology %>%
    st_drop_geometry() %>%
    mutate(city = "Xi'an")
) %>%
  group_by(city, typology) %>%
  summarise(
    n_cells = n(),
    mean_green_percent = mean(green_percent, na.rm = TRUE),
    mean_area = mean(area, na.rm = TRUE),
    mean_contig = mean(contig, na.rm = TRUE),
    mean_enn = mean(enn, na.rm = TRUE),
    mean_fwei_change = mean(fwei_change_mean, na.rm = TRUE),
    mean_flood_share = mean(flood_share, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  typology_summary,
  "data/results/typology_summary.csv",
  row.names = FALSE
)

print(cor_all)
print(typology_summary)

message("All maps, plots, correlations, and typologies created.")
