library(terra)
library(sf)
library(landscapemetrics)

dir.create("data/results", recursive = TRUE, showWarnings = FALSE)
green_path <- "data/processed/delft_green_binary.tif"
grid_path <- "data/processed/delft_grid.gpkg"
fwei_before_path <- "data/processed/delft_fwei_before.tif"
fwei_after_path <- "data/processed/delft_fwei_after.tif"
heavy_rain_path <- "data/processed/delft_heavy_rainfall.tif"
dem_path <- "data/processed/delft_dem.tif"
base_output <- "data/results/delft_green_metrics_base.gpkg"
flood_output <- "data/results/delft_flood_metrics_base.csv"
final_output <- "data/results/delft_grid_metrics.gpkg"
flood_threshold <- 0.05

get_green_value <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(NA_real_)
  x <- x[x$class == 1, ]
  if (nrow(x) == 0) return(NA_real_)
  mean(x$value, na.rm = TRUE)
}
get_class_value <- function(x, class_value = 1) {
  if (is.null(x) || nrow(x) == 0) return(NA_real_)
  if ("class" %in% names(x)) {
    x <- x[x$class == class_value, ]
    if (nrow(x) == 0) return(NA_real_)
  }
  mean(x$value, na.rm = TRUE)
}
align_raster_to_template <- function(r, template, method = "bilinear") {
  if (crs(r) != crs(template)) { r <- project(r, template, method = method) } else { r <- resample(r, template, method = method) }
  return(r)
}

required_flood_columns <- c("id","fwei_change_mean","flood_share","pland_flood","np_flood","flood_cohesion","flood_clumpy","flood_lpi","mean_dist_green","min_dist_green")

if (file.exists(base_output)) {
  grid_metrics <- st_read(base_output, quiet = TRUE)
} else {
  green <- rast(green_path)
  grid <- st_read(grid_path, quiet = TRUE)
  fact <- max(1, round(5 / res(green)[1]))
  green_5m <- aggregate(green, fact = fact, fun = "modal")
  grid_v <- vect(grid)
  results <- data.frame(id = grid$id, green_percent = NA_real_, area = NA_real_, gyrate = NA_real_, contig = NA_real_, enn = NA_real_)
  for (i in seq_len(nrow(grid))) {
    tryCatch({
      cell <- grid_v[i, ]
      green_cell <- crop(green_5m, cell)
      green_cell <- mask(green_cell, cell)
      vals <- na.omit(values(green_cell))
      if (length(vals) == 0) next
      results$green_percent[i] <- mean(vals == 1, na.rm = TRUE) * 100
      if (!1 %in% vals) next
      results$area[i] <- get_green_value(lsm_p_area(green_cell))
      results$gyrate[i] <- get_green_value(lsm_p_gyrate(green_cell))
      results$contig[i] <- get_green_value(lsm_p_contig(green_cell))
      results$enn[i] <- get_green_value(lsm_p_enn(green_cell))
    }, error = function(e) NULL)
  }
  grid_metrics <- merge(grid, results, by = "id")
  st_write(grid_metrics, base_output, delete_dsn = TRUE)
}
grid <- st_read(grid_path, quiet = TRUE)
grid_v <- vect(grid)
green <- rast(green_path)
fact <- max(1, round(5 / res(green)[1]))
green_5m <- aggregate(green, fact = fact, fun = "modal")

use_existing_flood_cache <- FALSE
if (file.exists(flood_output)) {
  flood_metrics_cached <- read.csv(flood_output)
  if (all(required_flood_columns %in% names(flood_metrics_cached))) use_existing_flood_cache <- TRUE
}
if (use_existing_flood_cache) {
  flood_metrics_all <- flood_metrics_cached
} else {
  fwei_before <- rast(fwei_before_path)
  fwei_after <- rast(fwei_after_path)
  fwei_change <- fwei_after - fwei_before
  fwei_change <- align_raster_to_template(fwei_change, green_5m, method = "bilinear")
  fwei_mean <- terra::extract(fwei_change, grid_v, fun = mean, na.rm = TRUE)
  names(fwei_mean)[2] <- "fwei_change_mean"
  flood_binary <- classify(fwei_change, rbind(c(-Inf, flood_threshold, 0), c(flood_threshold, Inf, 1)), others = NA)
  flood_share <- terra::extract(flood_binary, grid_v, fun = mean, na.rm = TRUE)
  names(flood_share)[2] <- "flood_share"
  flood_pattern_metrics <- data.frame(id = grid$id, pland_flood = NA_real_, np_flood = NA_real_, flood_cohesion = NA_real_, flood_clumpy = NA_real_, flood_lpi = NA_real_)

  for (i in seq_len(nrow(grid))) {
    tryCatch({
      cell <- grid_v[i, ]
      flood_cell <- crop(flood_binary, cell)
      flood_cell <- mask(flood_cell, cell)
      vals <- na.omit(values(flood_cell))
      if (length(vals) == 0) next
      if (!1 %in% vals) {
        flood_pattern_metrics$pland_flood[i] <- 0
        flood_pattern_metrics$np_flood[i] <- 0
        flood_pattern_metrics$flood_lpi[i] <- 0
        next
      }
      flood_pattern_metrics$pland_flood[i] <- get_class_value(lsm_c_pland(flood_cell), 1)
      flood_pattern_metrics$np_flood[i] <- get_class_value(lsm_c_np(flood_cell), 1)
      flood_pattern_metrics$flood_cohesion[i] <- get_class_value(lsm_c_cohesion(flood_cell), 1)
      flood_pattern_metrics$flood_clumpy[i] <- get_class_value(lsm_c_clumpy(flood_cell), 1)
      flood_pattern_metrics$flood_lpi[i] <- get_class_value(lsm_c_lpi(flood_cell), 1)
    }, error = function(e) NULL)
  }
  green_target <- green_5m
  green_target[green_target != 1] <- NA
  dist_to_green <- distance(green_target)
  flood_dist <- mask(dist_to_green, flood_binary, maskvalues = c(0, NA))
  dist_mean <- terra::extract(flood_dist, grid_v, fun = mean, na.rm = TRUE)
  dist_min <- terra::extract(flood_dist, grid_v, fun = min, na.rm = TRUE)

  names(dist_mean)[2] <- "mean_dist_green"
  names(dist_min)[2] <- "min_dist_green"

  flood_metrics_all <- data.frame(id = grid$id, fwei_change_mean = fwei_mean$fwei_change_mean, flood_share = flood_share$flood_share)
  flood_metrics_all <- merge(flood_metrics_all, flood_pattern_metrics, by = "id", all.x = TRUE)
  flood_metrics_all <- merge(flood_metrics_all, data.frame(id = grid$id, mean_dist_green = dist_mean$mean_dist_green, min_dist_green = dist_min$min_dist_green), by = "id", all.x = TRUE)
  write.csv(flood_metrics_all, flood_output, row.names = FALSE)
}
grid_metrics <- merge(grid_metrics, flood_metrics_all, by = "id", all.x = TRUE)

if (file.exists(heavy_rain_path)) {
  heavy_rain <- rast(heavy_rain_path)
  if (crs(heavy_rain) != crs(grid_v)) heavy_rain <- project(heavy_rain, crs(grid_v), method = "bilinear")
  heavy_rain <- crop(heavy_rain, grid_v)
  heavy_rain <- mask(heavy_rain, grid_v)
  rain_mean <- terra::extract(heavy_rain, grid_v, fun = mean, na.rm = TRUE)
  rain_max <- terra::extract(heavy_rain, grid_v, fun = max, na.rm = TRUE)

  names(rain_mean)[2] <- "heavy_rain_mean"
  names(rain_max)[2] <- "heavy_rain_max"

  grid_metrics$heavy_rain_mean <- rain_mean$heavy_rain_mean
  grid_metrics$heavy_rain_max <- rain_max$heavy_rain_max
}
if (file.exists(dem_path)) {
  dem <- rast(dem_path)
  if (crs(dem) != crs(grid_v)) dem <- project(dem, crs(grid_v), method = "bilinear")
  dem <- crop(dem, grid_v)
  dem <- mask(dem, grid_v)
  slope <- terrain(dem, v = "slope", unit = "degrees")
  dem_mean <- terra::extract(dem, grid_v, fun = mean, na.rm = TRUE)
  dem_min <- terra::extract(dem, grid_v, fun = min, na.rm = TRUE)
  slope_mean <- terra::extract(slope, grid_v, fun = mean, na.rm = TRUE)
  names(dem_mean)[2] <- "elevation_mean"
  names(dem_min)[2] <- "elevation_min"
  names(slope_mean)[2] <- "slope_mean"
  grid_metrics$elevation_mean <- dem_mean$elevation_mean
  grid_metrics$elevation_min <- dem_min$elevation_min
  grid_metrics$slope_mean <- slope_mean$slope_mean
}
st_write(grid_metrics, final_output, delete_dsn = TRUE)
write.csv(st_drop_geometry(grid_metrics), "data/results/delft_grid_metrics.csv", row.names = FALSE)
