library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(grid)

dir.create("data/results", recursive = TRUE, showWarnings = FALSE)
dir.create("data/results/metric_selection", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/results", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/results/metric_selection", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/results/combined_typologies", recursive = TRUE, showWarnings = FALSE)

delft <- st_read("data/results/delft_grid_metrics.gpkg", quiet = TRUE)
xian <- st_read("data/results/xian_grid_metrics.gpkg", quiet = TRUE)

read_context_layer <- function(path, target_crs) {
  if (!file.exists(path)) {
    message("Missing context layer: ", path)
    return(NULL)
  }

  layer <- st_read(path, quiet = TRUE)

  if (nrow(layer) == 0) {
    message("Empty context layer: ", path)
    return(NULL)
  }

  layer <- st_make_valid(layer)
  layer <- st_transform(layer, target_crs)

  return(layer)
}

load_city_context <- function(city_name, target_crs) {
  if (city_name == "Delft") {
    folder <- "data/context/delft"

    context <- list(
      buildings = read_context_layer(file.path(folder, "delft_buildings.gpkg"), target_crs),
      primary_roads = read_context_layer(file.path(folder, "delft_primary_roads.gpkg"), target_crs),
      secondary_roads = read_context_layer(file.path(folder, "delft_secondary_roads.gpkg"), target_crs),
      tertiary_roads = read_context_layer(file.path(folder, "delft_tertiary_roads.gpkg"), target_crs),
      water = read_context_layer(file.path(folder, "delft_water.gpkg"), target_crs),
      parks = read_context_layer(file.path(folder, "delft_parks.gpkg"), target_crs),
      residential = read_context_layer(file.path(folder, "delft_landuse_residential.gpkg"), target_crs),
      grass = read_context_layer(file.path(folder, "delft_landuse_grass.gpkg"), target_crs),
      greenfield = read_context_layer(file.path(folder, "delft_landuse_greenfield.gpkg"), target_crs),
      meadow = read_context_layer(file.path(folder, "delft_landuse_meadow.gpkg"), target_crs),
      tourism = read_context_layer(file.path(folder, "delft_tourism_attraction.gpkg"), target_crs),
      municipality_boundary = read_context_layer(file.path(folder, "delft_municipality_boundary.gpkg"), target_crs)
    )
  } else {
    folder <- "data/context/xian"

    context <- list(
      buildings = read_context_layer(file.path(folder, "xian_buildings.gpkg"), target_crs),
      primary_roads = read_context_layer(file.path(folder, "xian_primary_roads.gpkg"), target_crs),
      secondary_roads = read_context_layer(file.path(folder, "xian_secondary_roads.gpkg"), target_crs),
      tertiary_roads = read_context_layer(file.path(folder, "xian_tertiary_roads.gpkg"), target_crs),
      water = read_context_layer(file.path(folder, "xian_water.gpkg"), target_crs),
      parks = read_context_layer(file.path(folder, "xian_parks.gpkg"), target_crs),
      residential = read_context_layer(file.path(folder, "xian_landuse_residential.gpkg"), target_crs),
      grass = read_context_layer(file.path(folder, "xian_landuse_grass.gpkg"), target_crs),
      greenfield = read_context_layer(file.path(folder, "xian_landuse_greenfield.gpkg"), target_crs),
      meadow = read_context_layer(file.path(folder, "xian_landuse_meadow.gpkg"), target_crs),
      tourism = read_context_layer(file.path(folder, "xian_tourism_attraction.gpkg"), target_crs),
      municipality_boundary = NULL
    )
  }

  return(context)
}

make_study_boundary <- function(data) {
  boundary <- st_as_sf(st_as_sfc(st_bbox(data)))
  st_crs(boundary) <- st_crs(data)
  return(boundary)
}

add_context_layers <- function(p, context, study_boundary) {
  if (!is.null(context$residential)) p <- p + geom_sf(data = context$residential, fill = "#E3DED6", color = NA, alpha = 0.65)
  if (!is.null(context$grass)) p <- p + geom_sf(data = context$grass, fill = "#D8EEDC", color = NA, alpha = 0.65)
  if (!is.null(context$greenfield)) p <- p + geom_sf(data = context$greenfield, fill = "#CFE8D2", color = NA, alpha = 0.65)
  if (!is.null(context$meadow)) p <- p + geom_sf(data = context$meadow, fill = "#E2EBCF", color = NA, alpha = 0.65)
  if (!is.null(context$parks)) p <- p + geom_sf(data = context$parks, fill = "#C9E6CF", color = NA, alpha = 0.75)
  if (!is.null(context$water)) p <- p + geom_sf(data = context$water, fill = "#A9D7E8", color = "#7EB8CC", linewidth = 0.15, alpha = 0.80)
  if (!is.null(context$buildings)) p <- p + geom_sf(data = context$buildings, fill = "#D6D1C8", color = NA, alpha = 0.65)
  if (!is.null(context$tourism)) p <- p + geom_sf(data = context$tourism, fill = "#C8B5D8", color = "#8E77A8", linewidth = 0.15, alpha = 0.65)
  if (!is.null(context$tertiary_roads)) p <- p + geom_sf(data = context$tertiary_roads, color = "#D2D9DF", linewidth = 0.18, alpha = 0.75)
  if (!is.null(context$secondary_roads)) p <- p + geom_sf(data = context$secondary_roads, color = "#B8C2CC", linewidth = 0.28, alpha = 0.80)
  if (!is.null(context$primary_roads)) p <- p + geom_sf(data = context$primary_roads, color = "#9CA8B3", linewidth = 0.42, alpha = 0.85)

  if (!is.null(context$municipality_boundary)) {
    p <- p + geom_sf(data = context$municipality_boundary, fill = NA, color = "#7FA39A", linewidth = 0.45, linetype = "dashed", alpha = 0.85)
  }

  p <- p + geom_sf(data = study_boundary, fill = NA, color = "#6F7378", linewidth = 0.65, linetype = "dashed", alpha = 1)

  return(p)
}

message("Loading context layers...")

delft_context <- load_city_context("Delft", st_crs(delft))
xian_context <- load_city_context("Xi'an", st_crs(xian))

redundant_table <- data.frame(
  correlated_metrics = c("flood_share + pland_flood","elevation_mean + elevation_min","flood_share + flood_lpi","pland_flood + flood_lpi","mean_dist_green + min_dist_green","gyrate + contig","area + gyrate","green_percent + area"),
  correlation = c(1.000,1.000,0.979,0.979,0.924,0.885,0.873,0.837),
  decision = c("keep flood_share, remove pland_flood","keep elevation_mean, remove elevation_min","keep flood_share, remove flood_lpi","remove pland_flood and flood_lpi","remove both from typology, keep only for interpretation","keep contig, remove gyrate","keep contig, remove gyrate","keep green_percent, remove area"),
  reason = c("both measure detected water extent","both describe almost the same elevation pattern","largest flood patch mostly follows flood extent","both overlap strongly with flood extent","both measure distance to green space","patch spread overlaps with compactness","patch size overlaps with patch spread","green amount overlaps with patch size")
)

write_csv(redundant_table, "data/results/metric_selection/redundant_metric_decisions.csv")

png("figures/results/metric_selection/redundant_metric_decisions_table.png", width = 2400, height = 1200, res = 200)
grid.newpage()
grid.text("Metric redundancy decisions", x = 0.5, y = 0.95, gp = gpar(fontsize = 18, fontface = "bold"))
x_pos <- c(0.10, 0.38, 0.61, 0.83)
headers <- c("Correlated metrics", "r", "Decision", "Reason")

for (i in seq_along(headers)) {
  grid.text(headers[i], x = x_pos[i], y = 0.88, gp = gpar(fontsize = 11, fontface = "bold"))
}

y_start <- 0.80
row_gap <- 0.085

for (i in seq_len(nrow(redundant_table))) {
  y <- y_start - (i - 1) * row_gap
  grid.text(redundant_table$correlated_metrics[i], x = x_pos[1], y = y, just = "center", gp = gpar(fontsize = 9))
  grid.text(round(redundant_table$correlation[i], 3), x = x_pos[2], y = y, just = "center", gp = gpar(fontsize = 9))
  grid.text(redundant_table$decision[i], x = x_pos[3], y = y, just = "center", gp = gpar(fontsize = 8.5))
  grid.text(redundant_table$reason[i], x = x_pos[4], y = y, just = "center", gp = gpar(fontsize = 8.5))
}
dev.off()

green_flood_metrics <- c("green_percent","contig","enn","fwei_change_mean","flood_share","np_flood","flood_clumpy")
green_flood_dem_metrics <- c("green_percent","contig","enn","fwei_change_mean","flood_share","np_flood","flood_clumpy","elevation_mean","slope_mean")

selected_metrics_table <- data.frame(
  typology = c(rep("green_flood", length(green_flood_metrics)), rep("green_flood_dem", length(green_flood_dem_metrics))),
  metric = c(green_flood_metrics, green_flood_dem_metrics)
)

write_csv(selected_metrics_table, "data/results/metric_selection/selected_metrics_for_shared_typologies.csv")

type_palette <- c("Type 1" = "#5AA6A9", "Type 2" = "#D06C9F", "Type 3" = "#8797D8", "Type 4" = "#E8B7B0")

get_available_metrics <- function(delft_data, xian_data, metrics) {
  metrics[metrics %in% names(delft_data) & metrics %in% names(xian_data)]
}

prepare_shared_values <- function(delft_data, xian_data, metrics) {
  available_metrics <- get_available_metrics(delft_data, xian_data, metrics)

  delft_values <- delft_data %>% st_drop_geometry() %>% select(all_of(available_metrics)) %>% mutate(city = "Delft", row_id = row_number())
  xian_values <- xian_data %>% st_drop_geometry() %>% select(all_of(available_metrics)) %>% mutate(city = "Xi'an", row_id = row_number())
  combined <- bind_rows(delft_values, xian_values)
  values <- combined %>% select(all_of(available_metrics))

  for (m in names(values)) {
    values[[m]] <- as.numeric(values[[m]])
    values[[m]][is.na(values[[m]])] <- 0
    values[[m]][is.nan(values[[m]])] <- 0
    values[[m]][is.infinite(values[[m]])] <- 0
  }

  usable_metrics <- names(values)[sapply(values, function(x) sd(x, na.rm = TRUE) > 0)]
  values <- values %>% select(all_of(usable_metrics))
  values_scaled <- scale(values)

  return(list(combined = combined, values = values, values_scaled = values_scaled, usable_metrics = usable_metrics))
}

make_shared_typology <- function(delft_data, xian_data, metrics, type_column, centers = 4) {
  prepared <- prepare_shared_values(delft_data, xian_data, metrics)
  set.seed(123)

  km <- kmeans(prepared$values_scaled, centers = centers, nstart = 25, iter.max = 1000, algorithm = "MacQueen")

  combined <- prepared$combined
  combined[[type_column]] <- factor(paste0("Type ", km$cluster), levels = paste0("Type ", 1:centers))

  delft_types <- combined %>% filter(city == "Delft") %>% arrange(row_id) %>% pull(.data[[type_column]])
  xian_types <- combined %>% filter(city == "Xi'an") %>% arrange(row_id) %>% pull(.data[[type_column]])

  delft_data[[type_column]] <- factor(delft_types, levels = paste0("Type ", 1:centers))
  xian_data[[type_column]] <- factor(xian_types, levels = paste0("Type ", 1:centers))

  centers_scaled <- as.data.frame(km$centers)
  centers_scaled$type <- paste0("Type ", seq_len(nrow(centers_scaled)))
  centers_scaled <- centers_scaled %>% select(type, everything())

  raw_values <- prepared$values
  raw_values[[type_column]] <- combined[[type_column]]

  centers_raw <- raw_values %>% group_by(.data[[type_column]]) %>% summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>% rename(type = 1)

  return(list(delft_data = delft_data, xian_data = xian_data, centers_scaled = centers_scaled, centers_raw = centers_raw, usable_metrics = prepared$usable_metrics))
}

make_combined_elbow_plot <- function(delft_data, xian_data, metrics, title, output_png, output_csv) {
  prepared <- prepare_shared_values(delft_data, xian_data, metrics)
  set.seed(123)

  elbow <- data.frame(k = 2:8, within_cluster_ss = NA_real_)

  for (i in seq_along(elbow$k)) {
    km <- kmeans(prepared$values_scaled, centers = elbow$k[i], nstart = 25, iter.max = 1000, algorithm = "MacQueen")
    elbow$within_cluster_ss[i] <- km$tot.withinss
  }

  p <- ggplot(elbow, aes(x = k, y = within_cluster_ss)) +
    geom_line() +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = 2:8) +
    labs(title = title, x = "Number of clusters (k)", y = "Total within-cluster sum of squares") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  ggsave(output_png, p, width = 7, height = 5, dpi = 300)
  write_csv(elbow, output_csv)
  return(elbow)
}

make_type_context_map <- function(data, context, type_column, title, filename) {
  study_boundary <- make_study_boundary(data)

  p <- ggplot()
  p <- add_context_layers(p, context, study_boundary)

  p <- p +
    geom_sf(data = data, aes(fill = .data[[type_column]]), color = NA, alpha = 0.62) +
    scale_fill_manual(values = type_palette, drop = FALSE, na.value = NA) +
    geom_sf(data = study_boundary, fill = NA, color = "#6F7378", linewidth = 0.70, linetype = "dashed") +
    labs(title = title, fill = "Type") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 14), legend.title = element_text(size = 9), legend.text = element_text(size = 8), plot.background = element_rect(fill = "white", color = NA))

  ggsave(filename, p, width = 8, height = 6, dpi = 300)
  return(p)
}

make_donut <- function(data, type_column, title, filename) {
  counts <- data %>% st_drop_geometry() %>% count(.data[[type_column]], name = "n_cells") %>% rename(type = 1) %>% mutate(percent = n_cells / sum(n_cells) * 100, label = paste0(round(percent), "%"))

  p <- ggplot(counts, aes(x = 2, y = percent, fill = type)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3.5) +
    scale_fill_manual(values = type_palette, drop = FALSE) +
    labs(title = title, fill = "Type") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5), legend.title = element_text(size = 9), legend.text = element_text(size = 8))

  ggsave(filename, p, width = 6, height = 6, dpi = 300)
  return(p)
}

make_overview <- function(map_plot, donut_plot, filename) {
  png(filename, width = 3200, height = 1500, res = 300)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, widths = unit(c(1.25, 1), "null"))))
  print(map_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(donut_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  dev.off()
}

summarise_by_type <- function(data, city_name, type_column) {
  summary_metrics <- c("green_percent","area","gyrate","contig","enn","fwei_change_mean","flood_share","pland_flood","np_flood","flood_cohesion","flood_clumpy","flood_lpi","mean_dist_green","min_dist_green","elevation_mean","elevation_min","slope_mean")
  available_metrics <- summary_metrics[summary_metrics %in% names(data)]

  output <- data %>%
    st_drop_geometry() %>%
    mutate(city = city_name) %>%
    group_by(city, .data[[type_column]]) %>%
    summarise(n_cells = n(), across(all_of(available_metrics), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"), .groups = "drop") %>%
    rename(type = 2) %>%
    group_by(city) %>%
    mutate(percent_cells = n_cells / sum(n_cells) * 100) %>%
    ungroup() %>%
    select(city, type, n_cells, percent_cells, everything())

  return(output)
}

message("Making elbow plots...")

make_combined_elbow_plot(delft, xian, green_flood_metrics, "Shared green + flood typology", "figures/results/combined_typologies/green_flood_elbow.png", "data/results/green_flood_elbow.csv")
make_combined_elbow_plot(delft, xian, green_flood_dem_metrics, "Shared green + flood + DEM typology", "figures/results/combined_typologies/green_flood_dem_elbow.png", "data/results/green_flood_dem_elbow.csv")

message("Making shared green + flood typology...")

green_flood_cluster <- make_shared_typology(delft, xian, green_flood_metrics, "green_flood_type", centers = 4)
delft <- green_flood_cluster$delft_data
xian <- green_flood_cluster$xian_data

write_csv(green_flood_cluster$centers_raw, "data/results/green_flood_type_characteristics.csv")
write_csv(green_flood_cluster$centers_scaled, "data/results/green_flood_type_cluster_centres_scaled.csv")
write_csv(data.frame(metric = green_flood_cluster$usable_metrics), "data/results/metric_selection/green_flood_metrics_used.csv")

message("Making shared green + flood + DEM typology...")

green_flood_dem_cluster <- make_shared_typology(delft, xian, green_flood_dem_metrics, "green_flood_dem_type", centers = 4)
delft <- green_flood_dem_cluster$delft_data
xian <- green_flood_dem_cluster$xian_data

write_csv(green_flood_dem_cluster$centers_raw, "data/results/green_flood_dem_type_characteristics.csv")
write_csv(green_flood_dem_cluster$centers_scaled, "data/results/green_flood_dem_type_cluster_centres_scaled.csv")
write_csv(data.frame(metric = green_flood_dem_cluster$usable_metrics), "data/results/metric_selection/green_flood_dem_metrics_used.csv")

delft$final_typology <- delft$green_flood_dem_type
xian$final_typology <- xian$green_flood_dem_type

write_csv(data.frame(final_typology_used = "green_flood_dem_type"), "data/results/final_typology_used.csv")

message("Making maps and donut figures with context as base layer...")

delft_gf_map <- make_type_context_map(delft, delft_context, "green_flood_type", "Delft shared green + flood typology with urban context", "figures/results/combined_typologies/delft_green_flood_type_context.png")
xian_gf_map <- make_type_context_map(xian, xian_context, "green_flood_type", "Xi'an shared green + flood typology with urban context", "figures/results/combined_typologies/xian_green_flood_type_context.png")
delft_gf_donut <- make_donut(delft, "green_flood_type", "Delft", "figures/results/combined_typologies/delft_green_flood_type_donut.png")
xian_gf_donut <- make_donut(xian, "green_flood_type", "Xi'an", "figures/results/combined_typologies/xian_green_flood_type_donut.png")

make_overview(delft_gf_map, delft_gf_donut, "figures/results/combined_typologies/delft_green_flood_type_context_overview.png")
make_overview(xian_gf_map, xian_gf_donut, "figures/results/combined_typologies/xian_green_flood_type_context_overview.png")

delft_gfd_map <- make_type_context_map(delft, delft_context, "green_flood_dem_type", "Delft shared green + flood + DEM typology with urban context", "figures/results/combined_typologies/delft_green_flood_dem_type_context.png")
xian_gfd_map <- make_type_context_map(xian, xian_context, "green_flood_dem_type", "Xi'an shared green + flood + DEM typology with urban context", "figures/results/combined_typologies/xian_green_flood_dem_type_context.png")
delft_gfd_donut <- make_donut(delft, "green_flood_dem_type", "Delft", "figures/results/combined_typologies/delft_green_flood_dem_type_donut.png")
xian_gfd_donut <- make_donut(xian, "green_flood_dem_type", "Xi'an", "figures/results/combined_typologies/xian_green_flood_dem_type_donut.png")

make_overview(delft_gfd_map, delft_gfd_donut, "figures/results/combined_typologies/delft_green_flood_dem_type_context_overview.png")
make_overview(xian_gfd_map, xian_gfd_donut, "figures/results/combined_typologies/xian_green_flood_dem_type_context_overview.png")

message("Making summaries...")

green_flood_summary <- bind_rows(summarise_by_type(delft, "Delft", "green_flood_type"), summarise_by_type(xian, "Xi'an", "green_flood_type"))
green_flood_dem_summary <- bind_rows(summarise_by_type(delft, "Delft", "green_flood_dem_type"), summarise_by_type(xian, "Xi'an", "green_flood_dem_type"))
final_typology_summary <- bind_rows(summarise_by_type(delft, "Delft", "final_typology"), summarise_by_type(xian, "Xi'an", "final_typology"))

write_csv(green_flood_summary, "data/results/green_flood_type_summary.csv")
write_csv(green_flood_dem_summary, "data/results/green_flood_dem_type_summary.csv")
write_csv(final_typology_summary, "data/results/final_typology_summary.csv")

scale_01 <- function(x) {
  if (all(is.na(x))) return(rep(0, length(x)))
  if (sd(x, na.rm = TRUE) == 0) return(rep(0, length(x)))
  output <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  output[is.na(output)] <- 0
  return(output)
}

problematic_types <- final_typology_summary %>%
  group_by(city) %>%
  mutate(flood_score = scale_01(mean_flood_share), fwei_score = scale_01(mean_fwei_change_mean), low_green_score = 1 - scale_01(mean_green_percent), patch_score = scale_01(mean_np_flood), low_elevation_score = 1 - scale_01(mean_elevation_mean), problematic_score = 0.35 * flood_score + 0.25 * fwei_score + 0.20 * low_green_score + 0.15 * patch_score + 0.05 * low_elevation_score) %>%
  ungroup() %>%
  arrange(city, desc(problematic_score)) %>%
  mutate(
    interpretation = case_when(
      mean_green_percent < median(mean_green_percent, na.rm = TRUE) & mean_flood_share > median(mean_flood_share, na.rm = TRUE) ~ "low green and relatively high detected water",
      mean_green_percent > median(mean_green_percent, na.rm = TRUE) & mean_flood_share > median(mean_flood_share, na.rm = TRUE) ~ "green area with detected water, possibly storage or easier detection",
      mean_np_flood > median(mean_np_flood, na.rm = TRUE) ~ "many separate detected water patches",
      TRUE ~ "less problematic or unclear"
    ),
    possible_solution_direction = case_when(
      interpretation == "low green and relatively high detected water" ~ "depaving, rain gardens, permeable surfaces",
      interpretation == "green area with detected water, possibly storage or easier detection" ~ "floodable parks, retention basins, wetland enhancement",
      interpretation == "many separate detected water patches" ~ "bioswales, green corridors, connected pocket parks",
      TRUE ~ "no clear solution from typology alone"
    )
  )

write_csv(problematic_types, "data/results/problematic_typology_candidates.csv")

st_write(delft, "data/results/delft_combined_typology.gpkg", delete_dsn = TRUE)
st_write(xian, "data/results/xian_combined_typology.gpkg", delete_dsn = TRUE)

print(redundant_table)
print(green_flood_summary)
print(green_flood_dem_summary)
print(problematic_types)

message("Script 05 complete.")
