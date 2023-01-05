library(data.table)
library(ggmap)
library(gganimate)
library(lubridate)
library(tidyverse)
library(hms)
library(osmdata)

MAP_PADDING <- 0.01

# Load all activities and download the largest set of maps

# Restrict to specified activities
all_activities <- fread("data/raw/activities.csv")
all_activities <- all_activities[, .(`Activity Type`, 
                                     Filename = paste0(gsub("\\..+", "", gsub("activities/", "", Filename)), '.csv'), 
                                     date = lubridate::as_date(`Activity Date`, format="%b %d, %Y, %I:%M:%S %p"))]

# Load GPX data
gpx_dir <- "data/clean/from_gpx/"
gpx_fns <- list.files(gpx_dir, full.names = TRUE)
gpx_cols <- c('Date', 'Time', 'Latitude', 'Longitude')
gpx_data <- rbindlist(lapply(setNames(gpx_fns, gpx_fns), fread, select=gpx_cols, fill=TRUE), 
                      fill=TRUE, idcol="filename")

# Load FIT data
fit_dir <- "data/clean/from_fit/"
fit_fns <- list.files(fit_dir, full.names = TRUE)
fit_cols <- c("record.timestamp[s]",
              "record.position_lat[semicircles]",
              "record.position_long[semicircles]")
fit_data <- rbindlist(lapply(setNames(fit_fns, fit_fns), fread, select=fit_cols, fill=TRUE),
                      fill=TRUE, idcol="filename")

# Convert all into single format with 3 columns:
#   'timestamp':  POSIX timestamp
#   'lat': Latitude (deg)
#   'lon': Longitude (deg)

# GPX first
gpx_data[, Date := as_date(Date)]
gpx_data[, Time := as_hms(Time)]
gpx_data[, timestamp := as_datetime(paste0(Date, Time))]
setnames(gpx_data, old=c('Latitude', 'Longitude'), new=c('lat', 'lon'))
gpx_data[, (c('Date', 'Time')) := NULL]
setcolorder(gpx_data, c('timestamp', 'lat', 'lon'))

# FIT next
semicircles_to_degrees <- function(semicircles) {
    semicircles * ( 180 / 2^31 )
}

fit_epoch_offset <- 631065600 
fit_data[, timestamp := as_datetime(`record.timestamp[s]` + fit_epoch_offset)]
fit_data[, lat := semicircles_to_degrees(`record.position_lat[semicircles]`)]
fit_data[, lon := semicircles_to_degrees(`record.position_long[semicircles]`)]
fit_data[, (fit_cols) := NULL]
setcolorder(fit_data, c('timestamp', 'lat', 'lon'))

# Combine into single dataframe
comb <- rbindlist(list(gpx_data, fit_data))
comb[, date := as_date(timestamp)]
comb[, filename := basename(filename)]
comb[, week := floor_date(timestamp, "week")]

# Now to plot on map!
york_bounds <- matrix(c(-1.169865,
                   -0.995778,
                   53.884270,
                   54.005921
               ), nrow=2, byrow = TRUE)
colnames(york_bounds) <- c("min", "max")
rownames(york_bounds) <- c("x", "y")

# Filter dataset to activities within York
starting_points <- comb[ , .SD[which.min(timestamp)], by = filename]
activities_york <- starting_points[ lon > york_bounds[1, 1] & lon < york_bounds[1, 2] & lat > york_bounds[2, 1] & lat < york_bounds[2, 2], filename]
comb <- comb[ filename %in% activities_york]

# Download full maps
bounds_map <- matrix(
    c(
        min(comb$lon, na.rm=T),
        max(comb$lon, na.rm=T),
        min(comb$lat, na.rm=T),
        max(comb$lat, na.rm=T)
    ),
    nrow=2,
    byrow=TRUE
    )
colnames(bounds_map) <- c("min", "max")
rownames(bounds_map) <- c("x", "y")
bounds_map[, 1] <- bounds_map[, 1] - MAP_PADDING
bounds_map[, 2] <- bounds_map[, 2] + MAP_PADDING

land <- bounds_map %>%
    opq()%>%
    add_osm_feature(key = "landuse", 
                    value = c("farmland", "forest", "farmyard", "meadow")) %>%
    osmdata_sf()

big_streets <- bounds_map %>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = c("motorway", "trunk", "primary", "motorway_link", "trunk_link", "primary_link")) %>%
    osmdata_sf()

med_streets <- bounds_map %>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
    osmdata_sf()

small_streets <- bounds_map %>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = c("residential", "living_street",
                              "unclassified",
                              "service", "footway"
                    )) %>%
    osmdata_sf()

river <- bounds_map %>%
    opq()%>%
    add_osm_feature(key = "waterway", value = "river") %>%
    osmdata_sf()

railway <- bounds_map %>%
    opq()%>%
    add_osm_feature(key = "railway", value="rail") %>%
    osmdata_sf()

plot_routes <- function(in_year, 
                        type=c('Run', 'Ride'), 
                        padding_x=0.05,
                        padding_y=0.01,
                        density=FALSE,
                        n_groups=50) {
    type <- match.arg(type)
    sub_fns <- all_activities[ `Activity Type` == type & year(date) == in_year, .(filename=Filename)]
    df <- comb[sub_fns, on="filename"]
    df <- df[!is.na(lat) & !is.na(lon)]
    
    p <- ggplot() +
        geom_sf(data = land$osm_polygons,
                inherit.aes = FALSE,
                color = NA,
                fill = "green",
                size = .1,
                alpha = .3) +
        geom_sf(data = river$osm_lines,
                inherit.aes = FALSE,
                color = "steelblue",
                linewidth = 1.0,
                alpha = 1.0) +
        geom_sf(data = railway$osm_lines,
                inherit.aes = FALSE,
                color = "black",
                size = .2,
                linetype="dotdash",
                alpha = .5) +
        geom_sf(data = med_streets$osm_lines,
                inherit.aes = FALSE,
                color = "black",
                size = .3,
                alpha = .5) +
        geom_sf(data = small_streets$osm_lines,
                inherit.aes = FALSE,
                color = "#666666",
                size = .2,
                alpha = .3) +
        geom_sf(data = big_streets$osm_lines,
                inherit.aes = FALSE,
                color = "black",
                size = .5,
                alpha = .6) +
        coord_sf(xlim = c(min(df$lon, na.rm=T)-padding_x, max(df$lon, na.rm=T)+padding_x),
                 ylim = c(min(df$lat, na.rm=T)-padding_y, max(df$lat, na.rm=T)+padding_y),
                 expand = FALSE)  +
        geom_point(aes(x=lon, y=lat, group=date),
                  data=df,
                  size=0.1,
                  alpha=0.6,
                  na.rm=T,
                  colour='blue'
        ) +
        theme_void() + # get rid of background color, grid lines, etc.
        theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
              plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
        labs(title = sprintf("York %ss - %d", type, in_year))
    if (density) {
        p <- p + 
            stat_density_2d_filled(aes(x = lon, 
                                       y = lat, 
                                       fill=log(as.numeric(..level..)),
                                       alpha=..level..),
                                   contour_var = 'density',
                                   adjust=0.25,
                                   bins=n_groups,
                                   breaks=c(1, seq(1, 5500, length.out=n_groups-1)),
                                   data = df,
                                   show.legend = T) +
            scale_alpha_manual(values=c(0, rep(0.7, n_groups-1))) +
            scale_fill_viridis_c()
    }
    
    p
}


animation_over_time <- function(type=c('Run', 'Ride'), 
                                padding_x=0.05,
                                padding_y=0.01) {
    type <- match.arg(type)
    sub_fns <- all_activities[ `Activity Type` == type, .(filename=Filename)]
    df <- comb[sub_fns, on="filename"]
    df <- df[!is.na(lat) & !is.na(lon)]
    
    # Add dummy weeks in for dates missing with a single point centered on york
    n_weeks <- ceiling(difftime(max(df$week),
                         min(df$week),
                         units = "weeks"))
    
    all_weeks <- data.table(week=seq.POSIXt(df[1, week], by="1 week", length.out = n_weeks))
    df <- merge(all_weeks, df, by="week", all=TRUE)
    df[ is.na(lat), lat := mean(york_bounds[2, ])]
    df[ is.na(lon), lon := mean(york_bounds[1, ])]
    
    # Calculate cumulative monthly bounds
    zoom_bounds <- df[, list("xmin"=min(lon, na.rm=T), 
                             "xmax"=max(lon, na.rm=T),
                             "ymin"=min(lat, na.rm=T),
                             "ymax"=max(lat, na.rm=T)),
                      by=week]
    for (i in 2:nrow(zoom_bounds)) {
        prev_xmin <- zoom_bounds[i-1, xmin]
        prev_xmax <- zoom_bounds[i-1, xmax]
        prev_ymin <- zoom_bounds[i-1, ymin]
        prev_ymax <- zoom_bounds[i-1, ymax]
        
        if (zoom_bounds[i, xmin] > prev_xmin) {
            zoom_bounds[i, xmin := prev_xmin]
        }
        
        if (zoom_bounds[i, ymin] > prev_ymin) {
            zoom_bounds[i, ymin := prev_ymin]
        }
        
        if (zoom_bounds[i, xmax] < prev_xmax) {
            zoom_bounds[i, xmax := prev_xmax]
        }
        
        if (zoom_bounds[i, ymax] < prev_ymax) {
            zoom_bounds[i, ymax := prev_ymax]
        }
    }
    zoom_bounds[, ymin := ymin - padding_y]
    zoom_bounds[, ymax := ymax + padding_y]
    zoom_bounds[, xmin := xmin - padding_x]
    zoom_bounds[, xmax := xmax + padding_x]
    
    p <- ggplot() +
        geom_sf(data = land$osm_polygons,
                inherit.aes = FALSE,
                color = NA,
                fill = "green",
                size = .1,
                alpha = .3) +
        geom_sf(data = river$osm_lines,
                inherit.aes = FALSE,
                color = "steelblue",
                linewidth = 1.0,
                alpha = 1.0) +
        geom_sf(data = railway$osm_lines,
                inherit.aes = FALSE,
                color = "black",
                size = .2,
                linetype="dotdash",
                alpha = .5) +
        geom_sf(data = med_streets$osm_lines,
                inherit.aes = FALSE,
                color = "black",
                size = .3,
                alpha = .5) +
        geom_sf(data = small_streets$osm_lines,
                inherit.aes = FALSE,
                color = "#666666",
                size = .2,
                alpha = .3) +
        geom_sf(data = big_streets$osm_lines,
                inherit.aes = FALSE,
                color = "black",
                size = .5,
                alpha = .6) +
        coord_sf(xlim = c(min(df$lon, na.rm=T)-padding_x, max(df$lon, na.rm=T)+padding_x),
                 ylim = c(min(df$lat, na.rm=T)-padding_y, max(df$lat, na.rm=T)+padding_y),
                 expand = FALSE)  +
        geom_point(aes(x=lon, y=lat, group=date),
                  data=df,
                  size=0.1,
                  alpha=0.6,
                  na.rm=T,
                  colour='blue'
        ) +
        theme_void() + # get rid of background color, grid lines, etc.
        theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
              plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
        labs(title = "Routes ran - {format(frame_time, format='%b %Y')}") +
        transition_time(week) +
        shadow_mark() +
        ease_aes('linear') +
        view_zoom_manual(
            xmin=zoom_bounds$xmin,
            xmax=zoom_bounds$xmax,
            ymin=zoom_bounds$ymin,
            ymax=zoom_bounds$ymax,
            aspect_ratio = 1.5
        )
        # TODO try with view_zoom and use exclude_layer to ignore terrain
        # TODO try lok_ahead
        # Try aspect ratio
    gif <- animate(p,
            nframes=n_weeks,
            render=gifski_renderer(),
            fps=1,
            end_pause=5)
    gif
}

animation_over_routes <- function(type=c('Run', 'Ride'), 
                                  padding_x=0.05,
                                  padding_y=0.01) {
    type <- match.arg(type)
    sub_fns <- all_activities[ `Activity Type` == type, .(filename=Filename)]
    df <- comb[sub_fns, on="filename"]
    df <- df[!is.na(lat) & !is.na(lon)]
    
    # Normalise all timestamps to the same reference point
    df[, delta_s := as.numeric(timestamp - min(timestamp)), by=filename]
    df[, minute := floor(delta_s / 120)]
    n_minutes <- max(df$minute)
    
    # Calculate cumulative minutely bounds
    zoom_bounds <- df[, list("xmin"=min(lon, na.rm=T), 
                             "xmax"=max(lon, na.rm=T),
                             "ymin"=min(lat, na.rm=T),
                             "ymax"=max(lat, na.rm=T)),
                      by=minute]
    for (i in 2:nrow(zoom_bounds)) {
        prev_xmin <- zoom_bounds[i-1, xmin]
        prev_xmax <- zoom_bounds[i-1, xmax]
        prev_ymin <- zoom_bounds[i-1, ymin]
        prev_ymax <- zoom_bounds[i-1, ymax]
        
        if (zoom_bounds[i, xmin] > prev_xmin) {
            zoom_bounds[i, xmin := prev_xmin]
        }
        
        if (zoom_bounds[i, ymin] > prev_ymin) {
            zoom_bounds[i, ymin := prev_ymin]
        }
        
        if (zoom_bounds[i, xmax] < prev_xmax) {
            zoom_bounds[i, xmax := prev_xmax]
        }
        
        if (zoom_bounds[i, ymax] < prev_ymax) {
            zoom_bounds[i, ymax := prev_ymax]
        }
    }
    zoom_bounds[, ymin := ymin - padding_y]
    zoom_bounds[, ymax := ymax + padding_y]
    zoom_bounds[, xmin := xmin - padding_x]
    zoom_bounds[, xmax := xmax + padding_x]
    
    p <- ggplot() +
        geom_sf(data = land$osm_polygons,
                inherit.aes = FALSE,
                color = NA,
                fill = "green",
                size = .1,
                alpha = .3) +
        geom_sf(data = river$osm_lines,
                inherit.aes = FALSE,
                color = "steelblue",
                linewidth = 1.0,
                alpha = 1.0) +
        geom_sf(data = railway$osm_lines,
                inherit.aes = FALSE,
                color = "black",
                size = .2,
                linetype="dotdash",
                alpha = .5) +
        geom_sf(data = med_streets$osm_lines,
                inherit.aes = FALSE,
                color = "black",
                size = .3,
                alpha = .5) +
        geom_sf(data = small_streets$osm_lines,
                inherit.aes = FALSE,
                color = "#666666",
                size = .2,
                alpha = .3) +
        geom_sf(data = big_streets$osm_lines,
                inherit.aes = FALSE,
                color = "black",
                size = .5,
                alpha = .6) +
        coord_sf(xlim = c(min(df$lon, na.rm=T)-padding_x, max(df$lon, na.rm=T)+padding_x),
                 ylim = c(min(df$lat, na.rm=T)-padding_y, max(df$lat, na.rm=T)+padding_y),
                 expand = FALSE)  +
        geom_point(aes(x=lon, y=lat, group=filename),
                  data=df,
                  size=0.1,
                  alpha=0.6,
                  na.rm=T,
                  colour='blue'
        ) +
        theme_void() + # get rid of background color, grid lines, etc.
        theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
              plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
        labs(title = "Routes ran - {format(frame_time, format='%b %Y')}") +
        transition_time(minute) +
        shadow_mark() +
        ease_aes('linear') +
        view_zoom_manual(
            xmin=zoom_bounds$xmin,
            xmax=zoom_bounds$xmax,
            ymin=zoom_bounds$ymin,
            ymax=zoom_bounds$ymax,
            aspect_ratio = 1.5
        )
        # TODO try with view_zoom and use exclude_layer to ignore terrain
        # TODO try lok_ahead
        # Try aspect ratio
    gif <- animate(p,
            nframes=n_minutes,
            render=gifski_renderer(),
            fps=2,
            end_pause=5)
    gif
}

#plot_routes(2019, "Run")
#plot_routes(2020, "Run")
#plot_routes(2021, "Run")
#plot_routes(2022, "Run")
#
#plot_routes(2020, "Ride")
#plot_routes(2021, "Ride")
#plot_routes(2022, "Ride")

#animation_over_time("Run")

foo <- animation_over_routes("Run")
anim_save("routes.gif", foo)

# TODO Make a function to plot running distance from home, i.e. each frame is 1 minute of real running


