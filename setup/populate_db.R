library(data.table)
library(lubridate)
library(tidyverse)
library(hms)
library(DBI)
library(RSQLite)
library(jsonlite)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 5) {
    stop(sprintf("%d arguments provided, please provide 5: DB path, config path, activities.csv path, clean GPX CSV dir, clean Fit CSV dir",
                 length(args)))
}
db_fn <- args[1]
config_fn <- args[2]
activities_fn <- args[3]
gpx_dir <- args[4]
fit_dir <- args[5]

############### Load data
# Load all activities
all_activities <- fread(activities_fn)
all_activities <- all_activities[, .(activity_type=`Activity Type`, 
                                     activity_id = gsub("\\..+", "", basename(Filename)), 
                                     name = `Activity Name`,
                                     start_time = lubridate::as_datetime(`Activity Date`, format="%b %d, %Y, %I:%M:%S %p"),
                                     duration = `Elapsed Time`,
                                     distance = `Distance`,
                                     elevation = `Elevation Gain`)]

# Load GPX data (only has GPS)
gpx_fns <- list.files(gpx_dir, full.names = TRUE)
gpx_cols <- c('Date', 'Time', 'Latitude', 'Longitude')
gpx_data <- rbindlist(lapply(setNames(gpx_fns, gpx_fns), fread, select=gpx_cols, fill=TRUE), 
                      fill=TRUE, idcol="filename")
gpx_data[, activity_id := gsub("\\..+", "", basename(filename))]
gpx_data[, Date := as_date(Date)]
gpx_data[, Time := as_hms(Time)]
gpx_data[, time := as_datetime(paste0(Date, Time))]
gpx_data[, (c('Date', 'Time', 'filename')) := NULL]
setnames(gpx_data, old=c('Latitude', 'Longitude'), new=c('lat', 'lon'))
setcolorder(gpx_data, c('activity_id', 'time', 'lat', 'lon'))

# Load FIT data
fit_fns <- list.files(fit_dir, full.names = TRUE)
fit_cols <- c("record.timestamp[s]",
              "record.heart_rate[bpm]",
              "record.position_lat[semicircles]",
              "record.position_long[semicircles]")
fit_data <- rbindlist(lapply(setNames(fit_fns, fit_fns), fread, select=fit_cols, fill=TRUE), fill=TRUE, idcol="fn")
setnames(fit_data, old=fit_cols, new=c('time', 'heartrate', 'lat', 'lon'))
fit_data[, activity_id := gsub("\\..+", "", basename(fn))]
fit_data[, fn := NULL]

fit_epoch_offset <- 631065600 
fit_data[, time := as_datetime(time + fit_epoch_offset)]

semicircles_to_degrees <- function(semicircles) {
    semicircles * ( 180 / 2^31 )
}

fit_data[, lat := semicircles_to_degrees(lat)]
fit_data[, lon := semicircles_to_degrees(lon)]
setcolorder(fit_data, c('activity_id', 'time', 'lat', 'lon'))

# Athlete settings
config <- fromJSON(config_fn)
athlete <- as.data.frame(config$athlete)

################ Populate DB
con <- dbConnect(SQLite(), db_fn)
dbAppendTable(con, "activities", all_activities)
dbAppendTable(con, "heartrate", fit_data[ !is.na(heartrate), .(activity_id, time, heartrate)])
dbAppendTable(con, "location", fit_data[ !is.na(lon) & !is.na(lat), .(activity_id, time, lon, lat)])
dbAppendTable(con, "location", gpx_data[ !is.na(lon) & !is.na(lat), .(activity_id, time, lon, lat)])
dbAppendTable(con, "athlete", athlete)
dbDisconnect(con)
