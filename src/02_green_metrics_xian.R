library(terra)
library(sf)
library(landscapemetrics)

dir.create("data/results", recursive = TRUE, showWarnings = FALSE)

green_path <- "data/processed/xian_green_binary.tif"
grid_path <- "data/processed/xian_grid.gpkg"
fwei_before_path <- "data/processed/xian_fwei_before.tif"
fwei_after_path <- "data/processed/xian_fwei_after.tif"

dem_path <- "data/processed/xian_dem.tif"

base_output <- "data/results/xian_green_metrics_base.gpkg"
final_output <- "data/results/xian_grid_metrics.gpkg"

get_green_value <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(NA_real_)
  x <- x[x$class == 1, ]
  if (nrow(x) == 0) return(NA_real_)
  mean(x$value, na.rm = TRUE)
}

if (file.exists(base_output)) {
  message("Loading existing Xi'an green metrics...")
  grid_metrics <- st_read(base_output, quiet = TRUE)
} else {
  message("Calculating Xi'an green metrics...")

  green <- rast(green_path)
  grid <- st_read(grid_path, quiet = TRUE)

  fact <- max(1, round(5 / res(green)[1]))
  green_5m <- aggregate(green, fact = fact, fun = "modal")

  grid_v <- vect(grid)

  results <- data.frame(
    id = grid$id,
    green_percent = NA_real_,
    area = NA_real_,
    gyrate = NA_real_,
    contig = NA_real_,
    enn = NA_real_
  )

  for (i in seq_len(nrow(grid))) {
    message("Xi'an green metrics: ", i, " / ", nrow(grid))

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
green <- rast(green_path)

fact <- max(1, round(5 / res(green)[1]))
green_5m <- aggregate(green, fact = fact, fun = "modal")

fwei_before <- rast(fwei_before_path)
fwei_after <- rast(fwei_after_path)

fwei_change <- fwei_after - fwei_before
fwei_change <- crop(fwei_change, ext(vect(grid)))
fwei_change <- resample(fwei_change, green_5m, method = "bilinear")

fwei_mean <- terra::extract(
  fwei_change,
  vect(grid),
  fun = mean,
  na.rm = TRUE
)

names(fwei_mean)[2] <- "fwei_change_mean"
grid_metrics$fwei_change_mean <- fwei_mean$fwei_change_mean

flood_binary <- classify(
  fwei_change,
  rbind(c(-Inf, 0.05, 0),
        c(0.05, Inf, 1)),
  others = NA
)

flood_share <- terra::extract(
  flood_binary,
  vect(grid),
  fun = mean,
  na.rm = TRUE
)

names(flood_share)[2] <- "flood_share"
grid_metrics$flood_share <- flood_share$flood_share

green_target <- green_5m
green_target[green_target != 1] <- NA

dist_to_green <- distance(green_target)
flood_dist <- mask(dist_to_green, flood_binary, maskvalues = c(0, NA))

dist_mean <- terra::extract(
  flood_dist,
  vect(grid),
  fun = mean,
  na.rm = TRUE
)

names(dist_mean)[2] <- "mean_dist_green"
grid_metrics$mean_dist_green <- dist_mean$mean_dist_green

if (file.exists(dem_path)) {
  message("Adding Xi'an DEM variables...")

  dem <- rast(dem_path)
  dem <- project(dem, vect(grid))

  slope <- terrain(dem, v = "slope", unit = "degrees")

  dem_mean <- terra::extract(dem, vect(grid), fun = mean, na.rm = TRUE)
  dem_min <- terra::extract(dem, vect(grid), fun = min, na.rm = TRUE)
  slope_mean <- terra::extract(slope, vect(grid), fun = mean, na.rm = TRUE)

  names(dem_mean)[2] <- "elevation_mean"
  names(dem_min)[2] <- "elevation_min"
  names(slope_mean)[2] <- "slope_mean"

  grid_metrics$elevation_mean <- dem_mean$elevation_mean
  grid_metrics$elevation_min <- dem_min$elevation_min
  grid_metrics$slope_mean <- slope_mean$slope_mean
}

st_write(grid_metrics, final_output, delete_dsn = TRUE)

write.csv(
  st_drop_geometry(grid_metrics),
  "data/results/xian_grid_metrics.csv",
  row.names = FALSE
)

message("Xi'an script complete.")
