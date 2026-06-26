library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(grid)

dir.create("figures/results", recursive = TRUE, showWarnings = FALSE)
dir.create("data/results", recursive = TRUE, showWarnings = FALSE)

delft <- st_read("data/results/delft_grid_metrics.gpkg", quiet = TRUE)
xian <- st_read("data/results/xian_grid_metrics.gpkg", quiet = TRUE)

green_metrics <- c(
  "green_percent",
  "area",
  "gyrate",
  "contig",
  "enn",
  "mean_dist_green",
  "min_dist_green"
)

green_type_vars <- c(
  "green_percent",
  "area",
  "gyrate",
  "contig",
  "enn"
)

flood_metrics <- c(
  "fwei_change_mean",
  "flood_share",
  "pland_flood",
  "np_flood",
  "flood_cohesion",
  "flood_clumpy",
  "flood_lpi"
)

flood_type_vars <- c(
  "flood_share",
  "np_flood",
  "flood_cohesion",
  "flood_clumpy",
  "flood_lpi"
)

optional_metrics <- c(
  "heavy_rain_mean",
  "heavy_rain_max",
  "elevation_mean",
  "elevation_min",
  "slope_mean"
)

context_metric_maps <- c(
  "green_percent",
  "fwei_change_mean",
  "flood_share",
  "elevation_mean"
)

green_type_palette <- c(
  "Type 1" = "#5AA6A9",
  "Type 2" = "#D06C9F",
  "Type 3" = "#8797D8",
  "Type 4" = "#E8B7B0"
)

flood_type_palette <- c(
  "Type 1" = "#D8EEF3",
  "Type 2" = "#7DB9CF",
  "Type 3" = "#2F7DA3",
  "Type 4" = "#0B3C5D"
)

pretty_label <- function(x) {
  x <- gsub("_", " ", x)
  x <- gsub("fwei", "FWEI", x)
  x <- gsub("dem", "DEM", x)
  x
}

safe_cor <- function(x, y) {
  ok <- complete.cases(x, y)

  if (sum(ok) < 3) return(NA_real_)
  if (sd(x[ok], na.rm = TRUE) == 0) return(NA_real_)
  if (sd(y[ok], na.rm = TRUE) == 0) return(NA_real_)

  cor(x[ok], y[ok])
}

read_context_layer <- function(path, target_crs) {
  if (!file.exists(path)) {
    message("Context layer missing, skipping: ", path)
    return(NULL)
  }

  layer <- st_read(path, quiet = TRUE)

  if (nrow(layer) == 0) {
    message("Context layer is empty, skipping: ", path)
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
      buildings = NULL,
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
      buildings = NULL,
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
  if (!is.null(context$residential)) {
    p <- p + geom_sf(data = context$residential, fill = "#E3DED6", color = NA, alpha = 0.65)
  }

  if (!is.null(context$grass)) {
    p <- p + geom_sf(data = context$grass, fill = "#D8EEDC", color = NA, alpha = 0.65)
  }

  if (!is.null(context$greenfield)) {
    p <- p + geom_sf(data = context$greenfield, fill = "#CFE8D2", color = NA, alpha = 0.65)
  }

  if (!is.null(context$meadow)) {
    p <- p + geom_sf(data = context$meadow, fill = "#E2EBCF", color = NA, alpha = 0.65)
  }

  if (!is.null(context$parks)) {
    p <- p + geom_sf(data = context$parks, fill = "#C9E6CF", color = NA, alpha = 0.75)
  }

  if (!is.null(context$water)) {
    p <- p + geom_sf(data = context$water, fill = "#A9D7E8", color = "#7EB8CC", linewidth = 0.15, alpha = 0.80)
  }

  if (!is.null(context$buildings)) {
    p <- p + geom_sf(data = context$buildings, fill = "#D6D1C8", color = NA, alpha = 0.65)
  }

  if (!is.null(context$tourism)) {
    p <- p + geom_sf(data = context$tourism, fill = "#C8B5D8", color = "#8E77A8", linewidth = 0.15, alpha = 0.65)
  }

  if (!is.null(context$tertiary_roads)) {
    p <- p + geom_sf(data = context$tertiary_roads, color = "#D2D9DF", linewidth = 0.18, alpha = 0.75)
  }

  if (!is.null(context$secondary_roads)) {
    p <- p + geom_sf(data = context$secondary_roads, color = "#B8C2CC", linewidth = 0.28, alpha = 0.80)
  }

  if (!is.null(context$primary_roads)) {
    p <- p + geom_sf(data = context$primary_roads, color = "#9CA8B3", linewidth = 0.42, alpha = 0.85)
  }

  if (!is.null(context$municipality_boundary)) {
    p <- p + geom_sf(
      data = context$municipality_boundary,
      fill = NA,
      color = "#7FA39A",
      linewidth = 0.45,
      linetype = "dashed",
      alpha = 0.85
    )
  }

  p <- p + geom_sf(
    data = study_boundary,
    fill = NA,
    color = "#6F7378",
    linewidth = 0.65,
    linetype = "dashed",
    alpha = 1
  )

  return(p)
}

prepare_cluster_values <- function(data, vars) {
  available_vars <- vars[vars %in% names(data)]

  if (length(available_vars) == 0) {
    stop("None of the clustering variables are available.")
  }

  values <- data %>%
    st_drop_geometry() %>%
    select(all_of(available_vars))

  for (v in names(values)) {
    values[[v]][is.nan(values[[v]])] <- NA
    values[[v]][is.infinite(values[[v]])] <- NA
    values[[v]][is.na(values[[v]])] <- 0
  }

  values_scaled <- scale(values)

  return(list(
    values = values,
    values_scaled = values_scaled,
    available_vars = available_vars
  ))
}

prepare_combined_cluster_values <- function(delft_data, xian_data, vars) {
  available_vars <- vars[
    vars %in% names(delft_data) &
      vars %in% names(xian_data)
  ]

  if (length(available_vars) == 0) {
    stop("None of the shared clustering variables are available.")
  }

  delft_values <- delft_data %>%
    st_drop_geometry() %>%
    select(all_of(available_vars)) %>%
    mutate(city = "Delft", row_id = row_number())

  xian_values <- xian_data %>%
    st_drop_geometry() %>%
    select(all_of(available_vars)) %>%
    mutate(city = "Xi'an", row_id = row_number())

  combined <- bind_rows(delft_values, xian_values)

  values <- combined %>%
    select(all_of(available_vars))

  for (v in names(values)) {
    values[[v]][is.nan(values[[v]])] <- NA
    values[[v]][is.infinite(values[[v]])] <- NA
    values[[v]][is.na(values[[v]])] <- 0
  }

  values_scaled <- scale(values)

  return(list(
    combined = combined,
    values = values,
    values_scaled = values_scaled,
    available_vars = available_vars
  ))
}

make_continuous_map <- function(data, column, title, filename, palette_type = "green") {
  if (!column %in% names(data)) {
    message("Skipping missing column: ", column)
    return(NULL)
  }

  palette <- switch(
    palette_type,
    "green" = c("#F7FCF5", "#C7E9C0", "#74C476", "#238B45", "#00441B"),
    "flood" = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    "dem" = c("#F7FCB9", "#ADDD8E", "#31A354", "#756BB1", "#54278F"),
    c("#F7F7F7", "#CCCCCC", "#969696", "#525252")
  )

  p <- ggplot(data) +
    geom_sf(aes(fill = .data[[column]]), color = NA) +
    scale_fill_gradientn(colors = palette, na.value = "grey90") +
    labs(
      title = title,
      fill = pretty_label(column)
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )

  ggsave(filename, p, width = 8, height = 6, dpi = 300)
  return(p)
}

make_continuous_context_map <- function(data, context, column, title, filename, palette_type = "green") {
  if (!column %in% names(data)) {
    message("Skipping missing column: ", column)
    return(NULL)
  }

  palette <- switch(
    palette_type,
    "green" = c("#F7FCF5", "#C7E9C0", "#74C476", "#238B45", "#00441B"),
    "flood" = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    "dem" = c("#F7FCB9", "#ADDD8E", "#31A354", "#756BB1", "#54278F"),
    c("#F7F7F7", "#CCCCCC", "#969696", "#525252")
  )

  study_boundary <- make_study_boundary(data)

  p <- ggplot()
  p <- add_context_layers(p, context, study_boundary)

  p <- p +
    geom_sf(
      data = data,
      aes(fill = .data[[column]]),
      color = NA,
      alpha = 0.62
    ) +
    scale_fill_gradientn(colors = palette, na.value = NA) +
    geom_sf(
      data = study_boundary,
      fill = NA,
      color = "#6F7378",
      linewidth = 0.70,
      linetype = "dashed"
    ) +
    labs(
      title = title,
      fill = pretty_label(column)
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      plot.background = element_rect(fill = "white", color = NA)
    )

  ggsave(filename, p, width = 8, height = 6, dpi = 300)
  return(p)
}

make_type_map <- function(data, type_column, title, filename, palette) {
  if (!type_column %in% names(data)) {
    message("Skipping missing type column: ", type_column)
    return(NULL)
  }

  p <- ggplot(data) +
    geom_sf(aes(fill = .data[[type_column]]), color = NA) +
    scale_fill_manual(values = palette, drop = FALSE, na.value = "grey90") +
    labs(
      title = title,
      fill = "Type"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )

  ggsave(filename, p, width = 8, height = 6, dpi = 300)
  return(p)
}

make_type_context_map <- function(data, context, type_column, title, filename, palette) {
  if (!type_column %in% names(data)) {
    message("Skipping missing type column: ", type_column)
    return(NULL)
  }

  study_boundary <- make_study_boundary(data)

  p <- ggplot()
  p <- add_context_layers(p, context, study_boundary)

  p <- p +
    geom_sf(
      data = data,
      aes(fill = .data[[type_column]]),
      color = NA,
      alpha = 0.70
    ) +
    scale_fill_manual(values = palette, drop = FALSE, na.value = NA) +
    geom_sf(
      data = study_boundary,
      fill = NA,
      color = "#6F7378",
      linewidth = 0.70,
      linetype = "dashed"
    ) +
    labs(
      title = title,
      fill = "Type"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      plot.background = element_rect(fill = "white", color = NA)
    )

  ggsave(filename, p, width = 8, height = 6, dpi = 300)
  return(p)
}

make_donut <- function(data, type_column, title, filename, palette) {
  if (!type_column %in% names(data)) {
    message("Skipping missing donut column: ", type_column)
    return(NULL)
  }

  counts <- data %>%
    st_drop_geometry() %>%
    count(.data[[type_column]], name = "n_cells") %>%
    rename(type = 1) %>%
    mutate(
      percent = n_cells / sum(n_cells) * 100,
      label = ifelse(
        percent < 2,
        paste0(round(percent, 1), "%"),
        paste0(round(percent), "%")
      )
    )

  p <- ggplot(counts, aes(x = 2, y = percent, fill = type)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 3.5
    ) +
    scale_fill_manual(values = palette, drop = FALSE) +
    labs(
      title = title,
      fill = "Type"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )

  ggsave(filename, p, width = 6, height = 6, dpi = 300)
  return(p)
}

make_overview <- function(map_plot, donut_plot, filename) {
  if (is.null(map_plot) || is.null(donut_plot)) {
    message("Skipping overview because one plot is missing: ", filename)
    return(NULL)
  }

  png(filename, width = 3200, height = 1500, res = 300)

  grid.newpage()

  pushViewport(
    viewport(
      layout = grid.layout(
        nrow = 1,
        ncol = 2,
        widths = unit(c(1.25, 1), "null")
      )
    )
  )

  print(
    map_plot,
    vp = viewport(layout.pos.row = 1, layout.pos.col = 1)
  )

  print(
    donut_plot,
    vp = viewport(layout.pos.row = 1, layout.pos.col = 2)
  )

  dev.off()
}

make_elbow_plot <- function(data, vars, city_name, output_png, output_csv) {
  prepared <- prepare_cluster_values(data, vars)
  values_scaled <- prepared$values_scaled

  set.seed(123)

  elbow <- data.frame(
    k = 2:8,
    within_cluster_ss = NA_real_
  )

  for (i in seq_along(elbow$k)) {
    km <- kmeans(
      values_scaled,
      centers = elbow$k[i],
      nstart = 25,
      iter.max = 1000,
      algorithm = "MacQueen"
    )

    elbow$within_cluster_ss[i] <- km$tot.withinss
  }

  p <- ggplot(elbow, aes(x = k, y = within_cluster_ss)) +
    geom_line() +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = 2:8) +
    labs(
      title = paste("Elbow method for", city_name),
      x = "Number of clusters (k)",
      y = "Total within-cluster sum of squares"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14)
    )

  ggsave(output_png, p, width = 7, height = 5, dpi = 300)
  write_csv(elbow, output_csv)

  return(elbow)
}

make_combined_elbow_plot <- function(delft_data, xian_data, vars, title, output_png, output_csv) {
  prepared <- prepare_combined_cluster_values(delft_data, xian_data, vars)
  values_scaled <- prepared$values_scaled

  set.seed(123)

  elbow <- data.frame(
    k = 2:8,
    within_cluster_ss = NA_real_
  )

  for (i in seq_along(elbow$k)) {
    km <- kmeans(
      values_scaled,
      centers = elbow$k[i],
      nstart = 25,
      iter.max = 1000,
      algorithm = "MacQueen"
    )

    elbow$within_cluster_ss[i] <- km$tot.withinss
  }

  p <- ggplot(elbow, aes(x = k, y = within_cluster_ss)) +
    geom_line() +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = 2:8) +
    labs(
      title = title,
      x = "Number of clusters (k)",
      y = "Total within-cluster sum of squares"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14)
    )

  ggsave(output_png, p, width = 7, height = 5, dpi = 300)
  write_csv(elbow, output_csv)

  return(elbow)
}

make_cluster_type <- function(data, vars, type_column, centers = 4, seed = 123) {
  prepared <- prepare_cluster_values(data, vars)

  set.seed(seed)

  km <- kmeans(
    prepared$values_scaled,
    centers = centers,
    nstart = 25,
    iter.max = 1000,
    algorithm = "MacQueen"
  )

  data[[type_column]] <- factor(
    paste0("Type ", km$cluster),
    levels = paste0("Type ", 1:centers)
  )

  centers_scaled <- as.data.frame(km$centers)
  centers_scaled$type <- paste0("Type ", seq_len(nrow(centers_scaled)))
  centers_scaled <- centers_scaled %>%
    select(type, everything())

  raw_values <- prepared$values
  raw_values[[type_column]] <- data[[type_column]]

  centers_raw <- raw_values %>%
    group_by(.data[[type_column]]) %>%
    summarise(
      across(
        everything(),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    rename(type = 1)

  return(list(
    data = data,
    centers_scaled = centers_scaled,
    centers_raw = centers_raw
  ))
}

make_shared_cluster_type <- function(delft_data, xian_data, vars, type_column, centers = 4, seed = 123) {
  prepared <- prepare_combined_cluster_values(delft_data, xian_data, vars)

  set.seed(seed)

  km <- kmeans(
    prepared$values_scaled,
    centers = centers,
    nstart = 25,
    iter.max = 1000,
    algorithm = "MacQueen"
  )

  combined <- prepared$combined
  combined[[type_column]] <- factor(
    paste0("Type ", km$cluster),
    levels = paste0("Type ", 1:centers)
  )

  delft_types <- combined %>%
    filter(city == "Delft") %>%
    arrange(row_id) %>%
    pull(.data[[type_column]])

  xian_types <- combined %>%
    filter(city == "Xi'an") %>%
    arrange(row_id) %>%
    pull(.data[[type_column]])

  delft_data[[type_column]] <- factor(delft_types, levels = paste0("Type ", 1:centers))
  xian_data[[type_column]] <- factor(xian_types, levels = paste0("Type ", 1:centers))

  centers_scaled <- as.data.frame(km$centers)
  centers_scaled$type <- paste0("Type ", seq_len(nrow(centers_scaled)))
  centers_scaled <- centers_scaled %>%
    select(type, everything())

  raw_values <- prepared$values
  raw_values[[type_column]] <- combined[[type_column]]

  centers_raw <- raw_values %>%
    group_by(.data[[type_column]]) %>%
    summarise(
      across(
        everything(),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    rename(type = 1)

  return(list(
    delft_data = delft_data,
    xian_data = xian_data,
    centers_scaled = centers_scaled,
    centers_raw = centers_raw
  ))
}

summarise_by_type <- function(data, city_name, type_column) {
  summary_metrics <- c(
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

  available_metrics <- summary_metrics[summary_metrics %in% names(data)]

  output <- data %>%
    st_drop_geometry() %>%
    mutate(city = city_name) %>%
    group_by(city, .data[[type_column]]) %>%
    summarise(
      n_cells = n(),
      across(
        all_of(available_metrics),
        ~ mean(.x, na.rm = TRUE),
        .names = "mean_{.col}"
      ),
      .groups = "drop"
    ) %>%
    rename(type = 2) %>%
    group_by(city) %>%
    mutate(percent_cells = n_cells / sum(n_cells) * 100) %>%
    ungroup() %>%
    select(city, type, n_cells, percent_cells, everything())

  return(output)
}

make_correlations <- function(data, city_name) {
  metrics <- green_metrics[green_metrics %in% names(data)]
  targets <- flood_metrics[flood_metrics %in% names(data)]

  output <- data.frame()

  for (target in targets) {
    for (metric in metrics) {
      value <- safe_cor(data[[metric]], data[[target]])

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

message("Loading context layers...")

delft_context <- load_city_context("Delft", st_crs(delft))
xian_context <- load_city_context("Xi'an", st_crs(xian))

message("Creating continuous metric maps...")

for (m in green_metrics) {
  make_continuous_map(
    delft,
    m,
    paste("Delft", pretty_label(m)),
    paste0("figures/results/delft_", m, ".png"),
    "green"
  )

  make_continuous_map(
    xian,
    m,
    paste("Xi'an", pretty_label(m)),
    paste0("figures/results/xian_", m, ".png"),
    "green"
  )
}

for (m in flood_metrics) {
  make_continuous_map(
    delft,
    m,
    paste("Delft", pretty_label(m)),
    paste0("figures/results/delft_", m, ".png"),
    "flood"
  )

  make_continuous_map(
    xian,
    m,
    paste("Xi'an", pretty_label(m)),
    paste0("figures/results/xian_", m, ".png"),
    "flood"
  )
}

for (m in optional_metrics) {
  make_continuous_map(
    delft,
    m,
    paste("Delft", pretty_label(m)),
    paste0("figures/results/delft_", m, ".png"),
    "dem"
  )

  make_continuous_map(
    xian,
    m,
    paste("Xi'an", pretty_label(m)),
    paste0("figures/results/xian_", m, ".png"),
    "dem"
  )
}

message("Creating contextual continuous metric maps...")

for (m in context_metric_maps) {
  palette_type <- ifelse(
    m %in% c("fwei_change_mean", "flood_share"),
    "flood",
    ifelse(m %in% c("elevation_mean", "elevation_min", "slope_mean"), "dem", "green")
  )

  make_continuous_context_map(
    delft,
    delft_context,
    m,
    paste("Delft", pretty_label(m), "with urban context"),
    paste0("figures/results/delft_", m, "_context.png"),
    palette_type
  )

  make_continuous_context_map(
    xian,
    xian_context,
    m,
    paste("Xi'an", pretty_label(m), "with urban context"),
    paste0("figures/results/xian_", m, "_context.png"),
    palette_type
  )
}

message("Creating correlation table and plot...")

cor_all <- bind_rows(
  make_correlations(delft, "Delft"),
  make_correlations(xian, "Xi'an")
)

write_csv(
  cor_all,
  "data/results/correlations_all.csv"
)

p_cor <- ggplot(cor_all, aes(x = metric, y = correlation, fill = city)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~ target) +
  labs(
    title = "Correlation between green-space metrics and FWEI-derived flood-pattern indicators",
    x = "Green-space metric",
    y = "Pearson correlation",
    fill = "City"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggsave(
  "figures/results/correlation_overview.png",
  p_cor,
  width = 12,
  height = 8,
  dpi = 300
)

message("Creating elbow method plots...")

make_elbow_plot(
  delft,
  green_type_vars,
  "Delft city-specific green-space types",
  "figures/results/delft_green_type_elbow.png",
  "data/results/delft_green_type_elbow.csv"
)

make_elbow_plot(
  xian,
  green_type_vars,
  "Xi'an city-specific green-space types",
  "figures/results/xian_green_type_elbow.png",
  "data/results/xian_green_type_elbow.csv"
)

make_elbow_plot(
  delft,
  flood_type_vars,
  "Delft city-specific flood-pattern types",
  "figures/results/delft_flood_pattern_type_elbow.png",
  "data/results/delft_flood_pattern_type_elbow.csv"
)

make_elbow_plot(
  xian,
  flood_type_vars,
  "Xi'an city-specific flood-pattern types",
  "figures/results/xian_flood_pattern_type_elbow.png",
  "data/results/xian_flood_pattern_type_elbow.csv"
)

make_combined_elbow_plot(
  delft,
  xian,
  green_type_vars,
  "Shared green-space types for Delft and Xi'an",
  "figures/results/shared_green_type_elbow.png",
  "data/results/shared_green_type_elbow.csv"
)

make_combined_elbow_plot(
  delft,
  xian,
  flood_type_vars,
  "Shared flood-pattern types for Delft and Xi'an",
  "figures/results/shared_flood_pattern_type_elbow.png",
  "data/results/shared_flood_pattern_type_elbow.csv"
)

message("Creating city-specific green-space types...")

delft_green_cluster <- make_cluster_type(
  delft,
  green_type_vars,
  "green_type",
  centers = 4
)

xian_green_cluster <- make_cluster_type(
  xian,
  green_type_vars,
  "green_type",
  centers = 4
)

delft_typology <- delft_green_cluster$data
xian_typology <- xian_green_cluster$data

delft_typology$typology <- delft_typology$green_type
xian_typology$typology <- xian_typology$green_type

write_csv(
  delft_green_cluster$centers_raw,
  "data/results/delft_green_type_characteristics.csv"
)

write_csv(
  xian_green_cluster$centers_raw,
  "data/results/xian_green_type_characteristics.csv"
)

write_csv(
  delft_green_cluster$centers_scaled,
  "data/results/delft_green_type_cluster_centres_scaled.csv"
)

write_csv(
  xian_green_cluster$centers_scaled,
  "data/results/xian_green_type_cluster_centres_scaled.csv"
)

message("Creating city-specific flood-pattern types...")

delft_flood_cluster <- make_cluster_type(
  delft_typology,
  flood_type_vars,
  "flood_pattern_type",
  centers = 4
)

xian_flood_cluster <- make_cluster_type(
  xian_typology,
  flood_type_vars,
  "flood_pattern_type",
  centers = 4
)

delft_typology <- delft_flood_cluster$data
xian_typology <- xian_flood_cluster$data

write_csv(
  delft_flood_cluster$centers_raw,
  "data/results/delft_flood_pattern_type_characteristics.csv"
)

write_csv(
  xian_flood_cluster$centers_raw,
  "data/results/xian_flood_pattern_type_characteristics.csv"
)

write_csv(
  delft_flood_cluster$centers_scaled,
  "data/results/delft_flood_pattern_type_cluster_centres_scaled.csv"
)

write_csv(
  xian_flood_cluster$centers_scaled,
  "data/results/xian_flood_pattern_type_cluster_centres_scaled.csv"
)

message("Creating shared green-space types...")

shared_green_cluster <- make_shared_cluster_type(
  delft_typology,
  xian_typology,
  green_type_vars,
  "shared_green_type",
  centers = 4
)

delft_typology <- shared_green_cluster$delft_data
xian_typology <- shared_green_cluster$xian_data

write_csv(
  shared_green_cluster$centers_raw,
  "data/results/shared_green_type_characteristics.csv"
)

write_csv(
  shared_green_cluster$centers_scaled,
  "data/results/shared_green_type_cluster_centres_scaled.csv"
)

message("Creating shared flood-pattern types...")

shared_flood_cluster <- make_shared_cluster_type(
  delft_typology,
  xian_typology,
  flood_type_vars,
  "shared_flood_pattern_type",
  centers = 4
)

delft_typology <- shared_flood_cluster$delft_data
xian_typology <- shared_flood_cluster$xian_data

write_csv(
  shared_flood_cluster$centers_raw,
  "data/results/shared_flood_pattern_type_characteristics.csv"
)

write_csv(
  shared_flood_cluster$centers_scaled,
  "data/results/shared_flood_pattern_type_cluster_centres_scaled.csv"
)

message("Saving typology GeoPackages...")

st_write(
  delft_typology,
  "data/results/delft_typology.gpkg",
  delete_dsn = TRUE
)

st_write(
  xian_typology,
  "data/results/xian_typology.gpkg",
  delete_dsn = TRUE
)

message("Creating city-specific green-space type maps, donuts, and overviews...")

delft_green_map <- make_type_map(
  delft_typology,
  "green_type",
  "Delft city-specific green-space types",
  "figures/results/delft_typology.png",
  green_type_palette
)

xian_green_map <- make_type_map(
  xian_typology,
  "green_type",
  "Xi'an city-specific green-space types",
  "figures/results/xian_typology.png",
  green_type_palette
)

delft_green_context_map <- make_type_context_map(
  delft_typology,
  delft_context,
  "green_type",
  "Delft city-specific green-space types with urban context",
  "figures/results/delft_green_type_context.png",
  green_type_palette
)

xian_green_context_map <- make_type_context_map(
  xian_typology,
  xian_context,
  "green_type",
  "Xi'an city-specific green-space types with urban context",
  "figures/results/xian_green_type_context.png",
  green_type_palette
)

delft_green_donut <- make_donut(
  delft_typology,
  "green_type",
  "Delft",
  "figures/results/delft_green_type_donut.png",
  green_type_palette
)

xian_green_donut <- make_donut(
  xian_typology,
  "green_type",
  "Xi'an",
  "figures/results/xian_green_type_donut.png",
  green_type_palette
)

make_overview(
  delft_green_map,
  delft_green_donut,
  "figures/results/delft_green_type_overview.png"
)

make_overview(
  xian_green_map,
  xian_green_donut,
  "figures/results/xian_green_type_overview.png"
)

make_overview(
  delft_green_context_map,
  delft_green_donut,
  "figures/results/delft_green_type_context_overview.png"
)

make_overview(
  xian_green_context_map,
  xian_green_donut,
  "figures/results/xian_green_type_context_overview.png"
)

message("Creating shared green-space type maps, donuts, and overviews...")

delft_shared_green_map <- make_type_map(
  delft_typology,
  "shared_green_type",
  "Delft shared green-space types",
  "figures/results/delft_shared_green_type.png",
  green_type_palette
)

xian_shared_green_map <- make_type_map(
  xian_typology,
  "shared_green_type",
  "Xi'an shared green-space types",
  "figures/results/xian_shared_green_type.png",
  green_type_palette
)

delft_shared_green_context_map <- make_type_context_map(
  delft_typology,
  delft_context,
  "shared_green_type",
  "Delft shared green-space types with urban context",
  "figures/results/delft_shared_green_type_context.png",
  green_type_palette
)

xian_shared_green_context_map <- make_type_context_map(
  xian_typology,
  xian_context,
  "shared_green_type",
  "Xi'an shared green-space types with urban context",
  "figures/results/xian_shared_green_type_context.png",
  green_type_palette
)

delft_shared_green_donut <- make_donut(
  delft_typology,
  "shared_green_type",
  "Delft",
  "figures/results/delft_shared_green_type_donut.png",
  green_type_palette
)

xian_shared_green_donut <- make_donut(
  xian_typology,
  "shared_green_type",
  "Xi'an",
  "figures/results/xian_shared_green_type_donut.png",
  green_type_palette
)

make_overview(
  delft_shared_green_map,
  delft_shared_green_donut,
  "figures/results/delft_shared_green_type_overview.png"
)

make_overview(
  xian_shared_green_map,
  xian_shared_green_donut,
  "figures/results/xian_shared_green_type_overview.png"
)

make_overview(
  delft_shared_green_context_map,
  delft_shared_green_donut,
  "figures/results/delft_shared_green_type_context_overview.png"
)

make_overview(
  xian_shared_green_context_map,
  xian_shared_green_donut,
  "figures/results/xian_shared_green_type_context_overview.png"
)

message("Creating city-specific flood-pattern type maps, donuts, and overviews...")

delft_flood_type_map <- make_type_map(
  delft_typology,
  "flood_pattern_type",
  "Delft city-specific FWEI-derived flood-pattern types",
  "figures/results/delft_flood_pattern_type.png",
  flood_type_palette
)

xian_flood_type_map <- make_type_map(
  xian_typology,
  "flood_pattern_type",
  "Xi'an city-specific FWEI-derived flood-pattern types",
  "figures/results/xian_flood_pattern_type.png",
  flood_type_palette
)

delft_flood_context_map <- make_type_context_map(
  delft_typology,
  delft_context,
  "flood_pattern_type",
  "Delft city-specific FWEI-derived flood-pattern types with urban context",
  "figures/results/delft_flood_pattern_type_context.png",
  flood_type_palette
)

xian_flood_context_map <- make_type_context_map(
  xian_typology,
  xian_context,
  "flood_pattern_type",
  "Xi'an city-specific FWEI-derived flood-pattern types with urban context",
  "figures/results/xian_flood_pattern_type_context.png",
  flood_type_palette
)

delft_flood_type_donut <- make_donut(
  delft_typology,
  "flood_pattern_type",
  "Delft",
  "figures/results/delft_flood_pattern_type_donut.png",
  flood_type_palette
)

xian_flood_type_donut <- make_donut(
  xian_typology,
  "flood_pattern_type",
  "Xi'an",
  "figures/results/xian_flood_pattern_type_donut.png",
  flood_type_palette
)

make_overview(
  delft_flood_type_map,
  delft_flood_type_donut,
  "figures/results/delft_flood_pattern_type_overview.png"
)

make_overview(
  xian_flood_type_map,
  xian_flood_type_donut,
  "figures/results/xian_flood_pattern_type_overview.png"
)

make_overview(
  delft_flood_context_map,
  delft_flood_type_donut,
  "figures/results/delft_flood_pattern_type_context_overview.png"
)

make_overview(
  xian_flood_context_map,
  xian_flood_type_donut,
  "figures/results/xian_flood_pattern_type_context_overview.png"
)

message("Creating shared flood-pattern type maps, donuts, and overviews...")

delft_shared_flood_type_map <- make_type_map(
  delft_typology,
  "shared_flood_pattern_type",
  "Delft shared FWEI-derived flood-pattern types",
  "figures/results/delft_shared_flood_pattern_type.png",
  flood_type_palette
)

xian_shared_flood_type_map <- make_type_map(
  xian_typology,
  "shared_flood_pattern_type",
  "Xi'an shared FWEI-derived flood-pattern types",
  "figures/results/xian_shared_flood_pattern_type.png",
  flood_type_palette
)

delft_shared_flood_context_map <- make_type_context_map(
  delft_typology,
  delft_context,
  "shared_flood_pattern_type",
  "Delft shared FWEI-derived flood-pattern types with urban context",
  "figures/results/delft_shared_flood_pattern_type_context.png",
  flood_type_palette
)

xian_shared_flood_context_map <- make_type_context_map(
  xian_typology,
  xian_context,
  "shared_flood_pattern_type",
  "Xi'an shared FWEI-derived flood-pattern types with urban context",
  "figures/results/xian_shared_flood_pattern_type_context.png",
  flood_type_palette
)

delft_shared_flood_type_donut <- make_donut(
  delft_typology,
  "shared_flood_pattern_type",
  "Delft",
  "figures/results/delft_shared_flood_pattern_type_donut.png",
  flood_type_palette
)

xian_shared_flood_type_donut <- make_donut(
  xian_typology,
  "shared_flood_pattern_type",
  "Xi'an",
  "figures/results/xian_shared_flood_pattern_type_donut.png",
  flood_type_palette
)

make_overview(
  delft_shared_flood_type_map,
  delft_shared_flood_type_donut,
  "figures/results/delft_shared_flood_pattern_type_overview.png"
)

make_overview(
  xian_shared_flood_type_map,
  xian_shared_flood_type_donut,
  "figures/results/xian_shared_flood_pattern_type_overview.png"
)

make_overview(
  delft_shared_flood_context_map,
  delft_shared_flood_type_donut,
  "figures/results/delft_shared_flood_pattern_type_context_overview.png"
)

make_overview(
  xian_shared_flood_context_map,
  xian_shared_flood_type_donut,
  "figures/results/xian_shared_flood_pattern_type_context_overview.png"
)

message("Creating summaries...")

city_specific_green_type_summary <- bind_rows(
  summarise_by_type(delft_typology, "Delft", "green_type"),
  summarise_by_type(xian_typology, "Xi'an", "green_type")
)

city_specific_flood_pattern_type_summary <- bind_rows(
  summarise_by_type(delft_typology, "Delft", "flood_pattern_type"),
  summarise_by_type(xian_typology, "Xi'an", "flood_pattern_type")
)

shared_green_type_summary <- bind_rows(
  summarise_by_type(delft_typology, "Delft", "shared_green_type"),
  summarise_by_type(xian_typology, "Xi'an", "shared_green_type")
)

shared_flood_pattern_type_summary <- bind_rows(
  summarise_by_type(delft_typology, "Delft", "shared_flood_pattern_type"),
  summarise_by_type(xian_typology, "Xi'an", "shared_flood_pattern_type")
)

write_csv(
  city_specific_green_type_summary,
  "data/results/city_specific_green_type_summary.csv"
)

write_csv(
  city_specific_flood_pattern_type_summary,
  "data/results/city_specific_flood_pattern_type_summary.csv"
)

write_csv(
  shared_green_type_summary,
  "data/results/shared_green_type_summary.csv"
)

write_csv(
  shared_flood_pattern_type_summary,
  "data/results/shared_flood_pattern_type_summary.csv"
)

write_csv(
  shared_green_type_summary,
  "data/results/green_type_summary.csv"
)

write_csv(
  shared_flood_pattern_type_summary,
  "data/results/flood_pattern_type_summary.csv"
)

write_csv(
  shared_green_type_summary,
  "data/results/typology_summary.csv"
)

print(cor_all)
print(shared_green_type_summary)
print(shared_flood_pattern_type_summary)

message("All maps, plots, correlations, shared and city-specific type summaries, elbow plots, and overview figures created.")

message("Checking metric redundancy for the typologies...")

dir.create("data/results/metric_selection", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/results/metric_selection", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/results/combined_typologies", recursive = TRUE, showWarnings = FALSE)

all_typology_metrics <- c(
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
  "elevation_mean",
  "elevation_min",
  "slope_mean"
)

green_flood_metrics_reduced <- c(
  "green_percent",
  "area",
  "contig",
  "enn",
  "fwei_change_mean",
  "flood_share",
  "np_flood",
  "flood_clumpy"
)

green_flood_dem_metrics_reduced <- c(
  "green_percent",
  "area",
  "contig",
  "enn",
  "fwei_change_mean",
  "flood_share",
  "np_flood",
  "flood_clumpy",
  "elevation_mean",
  "slope_mean"
)

metric_selection <- data.frame(
  metric = all_typology_metrics,
  decision = ifelse(
    all_typology_metrics %in% green_flood_dem_metrics_reduced,
    "kept",
    "removed"
  ),
  reason = c(
    "green amount",
    "green patch size",
    "similar to patch size/spread, so area is kept instead",
    "green compactness/connectivity",
    "green patch isolation",
    "average FWEI surface-water change",
    "amount of detected surface-water increase",
    "similar to flood_share, so flood_share is kept instead",
    "number of separate flood patches",
    "similar to other flood-pattern metrics, so not used in final typology",
    "clustering of detected water",
    "similar to flood extent/dominance, so flood_share is kept instead",
    "average elevation",
    "similar to elevation_mean, so elevation_mean is kept instead",
    "average slope"
  )
)

write_csv(
  metric_selection,
  "data/results/metric_selection/metric_selection_notes.csv"
)

write_csv(
  metric_selection %>% filter(decision == "kept"),
  "data/results/metric_selection/selected_typology_metrics.csv"
)

write_csv(
  metric_selection %>% filter(decision == "removed"),
  "data/results/metric_selection/removed_redundant_metrics.csv"
)

get_available_metrics <- function(delft_data, xian_data, metrics) {
  metrics[
    metrics %in% names(delft_data) &
      metrics %in% names(xian_data)
  ]
}

make_redundancy_check <- function(delft_data, xian_data, metrics, name) {
  available_metrics <- get_available_metrics(delft_data, xian_data, metrics)

  delft_values <- delft_data %>%
    st_drop_geometry() %>%
    select(all_of(available_metrics)) %>%
    mutate(city = "Delft")

  xian_values <- xian_data %>%
    st_drop_geometry() %>%
    select(all_of(available_metrics)) %>%
    mutate(city = "Xi'an")

  values <- bind_rows(delft_values, xian_values)

  values_only <- values %>%
    select(all_of(available_metrics))

  for (m in names(values_only)) {
    values_only[[m]] <- as.numeric(values_only[[m]])
    values_only[[m]][is.na(values_only[[m]])] <- 0
    values_only[[m]][is.nan(values_only[[m]])] <- 0
    values_only[[m]][is.infinite(values_only[[m]])] <- 0
  }

  usable_metrics <- names(values_only)[
    sapply(values_only, function(x) sd(x, na.rm = TRUE) > 0)
  ]

  values_only <- values_only %>%
    select(all_of(usable_metrics))

  cor_matrix <- cor(
    values_only,
    use = "pairwise.complete.obs",
    method = "pearson"
  )

  write.csv(
    cor_matrix,
    paste0("data/results/metric_selection/", name, "_correlation_matrix.csv")
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

  cor_pairs <- cor_pairs %>%
    arrange(desc(abs_correlation))

  high_cor_pairs <- cor_pairs %>%
    filter(abs_correlation >= 0.80)

  write_csv(
    cor_pairs,
    paste0("data/results/metric_selection/", name, "_all_metric_correlations.csv")
  )

  write_csv(
    high_cor_pairs,
    paste0("data/results/metric_selection/", name, "_highly_correlated_metrics.csv")
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
    paste0("figures/results/metric_selection/", name, "_redundancy_heatmap.png"),
    p,
    width = 10,
    height = 8,
    dpi = 300
  )

  return(list(
    available_metrics = usable_metrics,
    correlation_matrix = cor_matrix,
    all_pairs = cor_pairs,
    high_pairs = high_cor_pairs
  ))
}

redundancy_check <- make_redundancy_check(
  delft,
  xian,
  all_typology_metrics,
  "green_flood_dem_metrics"
)

green_flood_available <- get_available_metrics(
  delft,
  xian,
  green_flood_metrics_reduced
)

green_flood_dem_available <- get_available_metrics(
  delft,
  xian,
  green_flood_dem_metrics_reduced
)

write_csv(
  data.frame(metric = green_flood_available),
  "data/results/metric_selection/green_flood_metrics_used.csv"
)

write_csv(
  data.frame(metric = green_flood_dem_available),
  "data/results/metric_selection/green_flood_dem_metrics_used.csv"
)

message("Creating green + flood typology...")

make_combined_elbow_plot(
  delft_typology,
  xian_typology,
  green_flood_available,
  "Shared green + flood typology",
  "figures/results/combined_typologies/shared_green_flood_elbow.png",
  "data/results/shared_green_flood_elbow.csv"
)

green_flood_cluster <- make_shared_cluster_type(
  delft_typology,
  xian_typology,
  green_flood_available,
  "green_flood_type",
  centers = 4
)

delft_typology <- green_flood_cluster$delft_data
xian_typology <- green_flood_cluster$xian_data

write_csv(
  green_flood_cluster$centers_raw,
  "data/results/green_flood_type_characteristics.csv"
)

write_csv(
  green_flood_cluster$centers_scaled,
  "data/results/green_flood_type_cluster_centres_scaled.csv"
)

message("Creating green + flood + DEM typology...")

make_combined_elbow_plot(
  delft_typology,
  xian_typology,
  green_flood_dem_available,
  "Shared green + flood + DEM typology",
  "figures/results/combined_typologies/shared_green_flood_dem_elbow.png",
  "data/results/shared_green_flood_dem_elbow.csv"
)

green_flood_dem_cluster <- make_shared_cluster_type(
  delft_typology,
  xian_typology,
  green_flood_dem_available,
  "green_flood_dem_type",
  centers = 4
)

delft_typology <- green_flood_dem_cluster$delft_data
xian_typology <- green_flood_dem_cluster$xian_data

write_csv(
  green_flood_dem_cluster$centers_raw,
  "data/results/green_flood_dem_type_characteristics.csv"
)

write_csv(
  green_flood_dem_cluster$centers_scaled,
  "data/results/green_flood_dem_type_cluster_centres_scaled.csv"
)

delft_typology$final_typology <- delft_typology$green_flood_dem_type
xian_typology$final_typology <- xian_typology$green_flood_dem_type

write_csv(
  data.frame(final_typology_used = "green_flood_dem_type"),
  "data/results/final_typology_used.csv"
)

combined_type_palette <- c(
  "Type 1" = "#5AA6A9",
  "Type 2" = "#D06C9F",
  "Type 3" = "#8797D8",
  "Type 4" = "#E8B7B0"
)

message("Making maps for green + flood typology...")

delft_green_flood_map <- make_type_context_map(
  delft_typology,
  delft_context,
  "green_flood_type",
  "Delft shared green + flood typology with urban context",
  "figures/results/combined_typologies/delft_green_flood_type_context.png",
  combined_type_palette
)

xian_green_flood_map <- make_type_context_map(
  xian_typology,
  xian_context,
  "green_flood_type",
  "Xi'an shared green + flood typology with urban context",
  "figures/results/combined_typologies/xian_green_flood_type_context.png",
  combined_type_palette
)

delft_green_flood_donut <- make_donut(
  delft_typology,
  "green_flood_type",
  "Delft",
  "figures/results/combined_typologies/delft_green_flood_type_donut.png",
  combined_type_palette
)

xian_green_flood_donut <- make_donut(
  xian_typology,
  "green_flood_type",
  "Xi'an",
  "figures/results/combined_typologies/xian_green_flood_type_donut.png",
  combined_type_palette
)

make_overview(
  delft_green_flood_map,
  delft_green_flood_donut,
  "figures/results/combined_typologies/delft_green_flood_type_overview.png"
)

make_overview(
  xian_green_flood_map,
  xian_green_flood_donut,
  "figures/results/combined_typologies/xian_green_flood_type_overview.png"
)

message("Making maps for green + flood + DEM typology...")

delft_green_flood_dem_map <- make_type_context_map(
  delft_typology,
  delft_context,
  "green_flood_dem_type",
  "Delft shared green + flood + DEM typology with urban context",
  "figures/results/combined_typologies/delft_green_flood_dem_type_context.png",
  combined_type_palette
)

xian_green_flood_dem_map <- make_type_context_map(
  xian_typology,
  xian_context,
  "green_flood_dem_type",
  "Xi'an shared green + flood + DEM typology with urban context",
  "figures/results/combined_typologies/xian_green_flood_dem_type_context.png",
  combined_type_palette
)

delft_green_flood_dem_donut <- make_donut(
  delft_typology,
  "green_flood_dem_type",
  "Delft",
  "figures/results/combined_typologies/delft_green_flood_dem_type_donut.png",
  combined_type_palette
)

xian_green_flood_dem_donut <- make_donut(
  xian_typology,
  "green_flood_dem_type",
  "Xi'an",
  "figures/results/combined_typologies/xian_green_flood_dem_type_donut.png",
  combined_type_palette
)

make_overview(
  delft_green_flood_dem_map,
  delft_green_flood_dem_donut,
  "figures/results/combined_typologies/delft_green_flood_dem_type_overview.png"
)

make_overview(
  xian_green_flood_dem_map,
  xian_green_flood_dem_donut,
  "figures/results/combined_typologies/xian_green_flood_dem_type_overview.png"
)

message("Making final typology summaries...")

green_flood_summary <- bind_rows(
  summarise_by_type(delft_typology, "Delft", "green_flood_type"),
  summarise_by_type(xian_typology, "Xi'an", "green_flood_type")
)

green_flood_dem_summary <- bind_rows(
  summarise_by_type(delft_typology, "Delft", "green_flood_dem_type"),
  summarise_by_type(xian_typology, "Xi'an", "green_flood_dem_type")
)

final_typology_summary <- bind_rows(
  summarise_by_type(delft_typology, "Delft", "final_typology"),
  summarise_by_type(xian_typology, "Xi'an", "final_typology")
)

write_csv(
  green_flood_summary,
  "data/results/green_flood_type_summary.csv"
)

write_csv(
  green_flood_dem_summary,
  "data/results/green_flood_dem_type_summary.csv"
)

write_csv(
  final_typology_summary,
  "data/results/final_typology_summary.csv"
)

scale_01 <- function(x) {
  if (all(is.na(x))) return(rep(0, length(x)))
  if (sd(x, na.rm = TRUE) == 0) return(rep(0, length(x)))

  output <- (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

  output[is.na(output)] <- 0
  return(output)
}

if (!"mean_flood_share" %in% names(final_typology_summary)) {
  final_typology_summary$mean_flood_share <- 0
}

if (!"mean_fwei_change_mean" %in% names(final_typology_summary)) {
  final_typology_summary$mean_fwei_change_mean <- 0
}

if (!"mean_green_percent" %in% names(final_typology_summary)) {
  final_typology_summary$mean_green_percent <- 0
}

if (!"mean_np_flood" %in% names(final_typology_summary)) {
  final_typology_summary$mean_np_flood <- 0
}

if (!"mean_elevation_mean" %in% names(final_typology_summary)) {
  final_typology_summary$mean_elevation_mean <- 0
}

problematic_types <- final_typology_summary %>%
  group_by(city) %>%
  mutate(
    flood_score = scale_01(mean_flood_share),
    fwei_score = scale_01(mean_fwei_change_mean),
    low_green_score = 1 - scale_01(mean_green_percent),
    patch_score = scale_01(mean_np_flood),
    low_elevation_score = 1 - scale_01(mean_elevation_mean),
    problematic_score =
      0.35 * flood_score +
      0.25 * fwei_score +
      0.20 * low_green_score +
      0.15 * patch_score +
      0.05 * low_elevation_score
  ) %>%
  ungroup() %>%
  arrange(city, desc(problematic_score)) %>%
  mutate(
    interpretation = case_when(
      mean_green_percent < median(mean_green_percent, na.rm = TRUE) &
        mean_flood_share > median(mean_flood_share, na.rm = TRUE) ~
        "low green and relatively high detected water",
      mean_green_percent > median(mean_green_percent, na.rm = TRUE) &
        mean_flood_share > median(mean_flood_share, na.rm = TRUE) ~
        "green area with detected water, possibly storage or easier detection",
      mean_np_flood > median(mean_np_flood, na.rm = TRUE) ~
        "many separate detected water patches",
      TRUE ~
        "less problematic or unclear"
    ),
    possible_solution_direction = case_when(
      interpretation == "low green and relatively high detected water" ~
        "depaving, rain gardens, permeable surfaces",
      interpretation == "green area with detected water, possibly storage or easier detection" ~
        "floodable parks, retention basins, wetland enhancement",
      interpretation == "many separate detected water patches" ~
        "bioswales, green corridors, connected pocket parks",
      TRUE ~
        "no clear solution from typology alone"
    )
  )

write_csv(
  problematic_types,
  "data/results/problematic_typology_candidates.csv"
)

st_write(
  delft_typology,
  "data/results/delft_typology.gpkg",
  delete_dsn = TRUE
)

st_write(
  xian_typology,
  "data/results/xian_typology.gpkg",
  delete_dsn = TRUE
)

st_write(
  delft_typology,
  "data/results/delft_typology_with_combined_types.gpkg",
  delete_dsn = TRUE
)

st_write(
  xian_typology,
  "data/results/xian_typology_with_combined_types.gpkg",
  delete_dsn = TRUE
)

print(metric_selection)
print(redundancy_check$high_pairs)
print(final_typology_summary)
print(problematic_types)

message("New metric redundancy checks and combined typologies are done.")
