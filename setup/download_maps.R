library(tidyverse)
library(osmdata)
library(jsonlite)
library(DBI)
library(RSQLite)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 3) {
    stop(sprintf("%d arguments provided, only provide 3: the database name, the config filepath, and the location to store the downloaded maps",
                 length(args)))
}
db_fn <- args[1]
config_fn <- args[2]
cache_fn <- args[3]

config <- fromJSON(config_fn)$map
xmin <- config$xmin 
xmax <- config$xmax 
ymin <- config$ymin 
ymax <- config$ymax 
padding <- config$padding

# Find the boundary of all activities that start within the specified region
con <- dbConnect(SQLite(), db_fn)
activities_starting_in_region <- tbl(con, "location") |>
    group_by(activity_id) |>
    filter(time == min(time, na.rm=T),
           lon >= xmin,
           lon <= xmax,
           lat >= ymin,
           lat <= ymax
           ) |>
    select(activity_id) |>
    ungroup()
# get bounds of this area
activity_bounds <- activities_starting_in_region |>
    inner_join(tbl(con, "location"), by="activity_id") |>
    summarise(
        xmin = min(lon),
        xmax = max(lon),
        ymin = min(lat),
        ymax = max(lat)
    ) |>
    collect()

# Turn it into format required by osmdata
bounds <- matrix(
    c(
        activity_bounds$xmin,
        activity_bounds$xmax,
        activity_bounds$ymin,
        activity_bounds$ymax
    ),
    nrow=2,
    byrow=TRUE
)
colnames(bounds) <- c("min", "max")
rownames(bounds) <- c("x", "y")

# Add padding to boundary
x_dist <- padding * (bounds[1, 2] - bounds[1, 1])
y_dist <- padding * (bounds[2, 2] - bounds[2, 1])
bounds[1, 1] <- bounds[1, 1] - x_dist
bounds[1, 2] <- bounds[1, 2] + x_dist
bounds[2, 1] <- bounds[2, 1] - y_dist
bounds[2, 2] <- bounds[2, 2] + y_dist

# Download maps and save to disk
download_layer <- function(layer) {
    opq(bounds) |>
    add_osm_feature(key = layer$key, 
                    value = layer$value) |>
    osmdata_sf()
}
layers <- lapply(config$osmlayers, download_layer)

saveRDS(layers, cache_fn)