calculate_hrss <- function(hr, ts, HRmax, HRrest, LTHR, k=1.92) {
   hrr <- (hr - HRrest) / (HRmax - HRrest) 
   times_diff_min <- c(0, diff(ts)) / 60
   trimp_activity <- sum(times_diff_min * hrr * 0.64 * exp(k * hrr))
   hrr_lthr <- (LTHR - HRrest) / (HRmax - HRrest)
   trimp_lthrhour <- 60 * hrr_lthr * 0.64 * exp(k * hrr_lthr)
   hrss <- trimp_activity / trimp_lthrhour
   hrss * 100
}

get_calendar_dates <- function() {
    # Get all dates from last 52 weeks
    days_til_sunday <- 7 - lubridate::wday(today(), week_start=1)
    first_day <- today() + days(days_til_sunday) - weeks(52) + days(1)
    all_dates <- data.table(date=seq.Date(from=first_day, to=today() + days(days_til_sunday), by="day"))
    all_dates[, c("wday", "week", "year") := .(lubridate::wday(date, week_start=1),
                                               lubridate::isoweek(date),
                                               lubridate::year(date))]
    all_dates[, year_diff := year - min(year)]
    all_dates[, week := week + year_diff * 52]
    # The first days before Monday in the new Year will be off,
    # since technically they'll be week 52 and will add an additional 52
    all_dates[ week == 104, week := 52]
    all_dates[, week := paste0("week_", week - min(week))]
    all_dates[, c('year_diff', 'year') := NULL]

    # Work out the week number with first months
    all_dates[ wday == 1, month := lubridate::month(date)]
    all_dates
}

insert_or_update_default_activity <- function(value, con) {
    default_exists <- nrow(tbl(con, "config") |> collect() |> filter(property == 'default_activity')) > 0
    if (default_exists) {
        q <- "UPDATE config SET value = ? WHERE property = 'default_activity'"
    } else {
        q <- "INSERT INTO config (property, value) VALUES ('default_activity', ?)"
    }
    res <- dbSendStatement(con, q, params=list(value))
    dbClearResult(res)
}

create_activities <- function(df, con) {
    # Creates activity records in the DB
    #
    # Args:
    #  - df (data.table)
    #  - con (Database connection)
    #
    # Returns:
    #  None, updates database as a side effect.

    # Add any new activity types
    act_types <- unique(df$activity_type)
    curr_act_types <- tbl(con, "activity_types") |> pull(activity_type)
    new_types <- setdiff(act_types, curr_act_types)
    if (length(new_types) > 0) {
        dbAppendTable(con, "activity_types", data.frame(activity_type=new_types))
    }

    # Create activity record
    # Duckdb can't convert from integer64 (bit64 R library) into its own long type
    # so cast as float
    df[, activity_id := as.numeric(activity_id) ]
    dbAppendTable(con, "activities", df)
}

upload_activities <- function(df, con) {
    # Uploads new activities
    #
    # Args:
    #   - df (data.table): Columns:
    #     - activity_id (PK)
    #     - time (PK)
    #     - heartrate
    #     - lat
    #     - lon
    #  - con (Database connection)
    #
    # Returns:
    #   None, updates database as a side-effect.

    # Upload heart rate and location data
    hr <- df[ !is.na(heartrate), .(activity_id, time, heartrate)]
    location <- df[ !is.na(lat) & !is.na(lon), .(activity_id, time, lat, lon)]

    # Same issue about duckdb not correctly parsing integer64 so convert to float
    hr[, activity_id := as.numeric(activity_id)]
    location[, activity_id := as.numeric(activity_id)]

    # FITFileR returns 255 for heartrate if missing, given that no one should have this as an actual
    # measurement it seems reasonable to remove these rows
    hr <- hr[ heartrate < 255 ]

    dbAppendTable(con, "heartrate", hr)
    dbAppendTable(con, "location", location)

    # Calculate fitness scores
    if (nrow(hr) > 0) {
        athlete <- tbl(con, "athlete") |> collect()
        hrss <- hr[, .(hrss = calculate_hrss(heartrate,
                                             time,
                                             athlete$maxHR,
                                             athlete$restHR,
                                             athlete$thresholdHR,
                                             )),
                         by=.(activity_id)]
        dbAppendTable(con, "fitness", hrss)
    }
}

handle_export_archive <- function(archive, con) {
    withProgress(
        message="Reading export file",
        detail="Extract raw activity files",
        value=0.0, {

        # Extract archive to a temporary folder
        temp_dir <- tempdir()
        unzip(archive$datapath, exdir=temp_dir)

        # Add activities first!
        all_activities <- fread(file.path(temp_dir, "activities.csv"))
        all_activities <- all_activities[, .(activity_type=`Activity Type`,
                                             activity_id = `Activity ID`,
                                             filename_id = gsub("\\..+", "", basename(Filename)),
                                             name = `Activity Name`,
                                             start_time = lubridate::as_datetime(`Activity Date`, format="%b %d, %Y, %I:%M:%S %p"),
                                             duration = `Elapsed Time`,
                                             distance = `Distance`,
                                             elevation = `Elevation Gain`)]

        # Unzip GPX files - shouldn't ever have any but worth accounting for possibility
        gpx_gz_files <- list.files(file.path(temp_dir, "activities"),
                                pattern="*.gpx.gz$", full.names=TRUE)
        dummy <- lapply(gpx_gz_files, R.utils::gunzip, overwrite=TRUE, remove=FALSE)

        # Count the number of GPX and FIT files
        fit_files <- list.files(file.path(temp_dir, "activities"),
                                pattern="*.fit.gz$", full.names=TRUE)
 
        # Setup values for progress bar updates
        n_fit_files <- length(fit_files)

        fit_files_indices <- 1:n_fit_files
        if (n_fit_files < 20) {
            progress_update_indices_fit <- fit_files_indices
        } else {
            increment <- floor(0.01 * n_fit_files)
            progress_update_indices_fit <- seq(1, n_fit_files, by=increment)
        }
 
        # convert GPX to CSV using gpsbabel
        gpx_files <- list.files(file.path(temp_dir, "activities"),
                                pattern="*.gpx$", full.names=TRUE)

        # Setup values for progress bar updates
        n_gpx_files <- length(gpx_files)
        gpx_files_indices <- 1:n_gpx_files
        if (n_gpx_files < 20) {
            progress_update_indices_gpx <- gpx_files_indices
        } else {
            increment <- floor(0.01 * n_gpx_files)
            progress_update_indices_gpx <- seq(1, n_gpx_files, by=increment)
        }

        n_total_files <- n_fit_files + n_gpx_files

        # Read each file
        for (i in fit_files_indices) {
            fn <- fit_files[i]
            if (i %in% progress_update_indices_fit) {
                fit_pct <- i / n_fit_files
                overall_pct <- i / n_total_files
                setProgress(
                    value=overall_pct,
                    message="Adding FIT activities to database",
                    detail=sprintf("File %d/%d (%.2f%%)", i, n_fit_files, fit_pct*100)
                )
            }

            # Unzip fit file and read into R if doesn't exist in DB
            this_filename_id <- basename(tools::file_path_sans_ext(tools::file_path_sans_ext(fn)))
            this_activity_id <- all_activities |>
                                    filter(filename_id == this_filename_id) |>
                                    pull(activity_id)
            if (tbl(con, "activities") |> filter(activity_id == this_activity_id) |> count() |> pull(n) > 0) next
            R.utils::gunzip(fn, overwrite=TRUE, remove=FALSE)
            bn <- tools::file_path_sans_ext(fn)

            tryCatch({
                fit_raw <- readFitFile(bn)
                fit_raw <- records(fit_raw)

                if ('list' %in% class(fit_raw)) {
                    df_raw <- rbindlist(fit_raw, fill=TRUE, use.names=TRUE)
                } else {
                    df_raw <- as.data.table(fit_raw)
                }

                setnames(df_raw,
                         old=c("timestamp", "heart_rate", "position_lat", "position_long"),
                         new=c("time", "heartrate", "lat", "lon"),
                         skip_absent = TRUE)
                # Add columns if missing
                if (! "lat" %in% names(df_raw)) df_raw[, lat := NA]
                if (! "lon" %in% names(df_raw)) df_raw[, lon := NA]
                if (! "heartrate" %in% names(df_raw)) df_raw[, heartrate := NA]
                df_raw <- df_raw[, .(time, heartrate, lat, lon)]

                # Add activity_id
                df_raw[, filename_id := this_filename_id ]
                df_raw <- all_activities[df_raw, .(heartrate, lat, lon, time, activity_id), on=.(filename_id)]

                # Create activity entry and upload the time-series data if doesn't exist
                create_activities(
                    all_activities[activity_id == this_activity_id, .(activity_type, activity_id, name, start_time, duration, distance, elevation) ],
                    con
                )
                upload_activities(df_raw[, .(activity_id, time, heartrate, lat, lon)], con)
            },
            error=function(e) {}
            )
        }


        for (i in gpx_files_indices) {
            fn <- gpx_files[i]

            if (i %in% progress_update_indices_gpx) {
                gpx_pct <- i / n_gpx_files
                overall_pct <- (i + n_fit_files) / n_total_files
                setProgress(
                    value=overall_pct,
                    message="Adding GPX activities to database",
                    detail=sprintf("File %d/%d (%.2f%%)", i, n_gpx_files, gpx_pct*100)
                )
            }

            # Only read file if doesn't exist in DB
            this_filename_id <- basename(tools::file_path_sans_ext(fn))
            this_activity_id <- all_activities |>
                                    filter(filename_id == this_filename_id) |>
                                    pull(activity_id)
            if (tbl(con, "activities") |> filter(activity_id == this_activity_id) |> count() |> pull(n) > 0) next

            # Read gpx file into R
            tryCatch({
                gpx_raw <- read_gpx(fn)
                gpx_data <- rbindlist(gpx_raw$tracks)
                gpx_data[, filename_id := gsub("\\..+", "", basename(fn))]
                setnames(gpx_data, old=c('Time', 'Latitude', 'Longitude'), new=c('time', 'lat', 'lon'), skip_absent = TRUE)
                # load activity_id and set to first column
                gpx_data <- all_activities[gpx_data, .(lat, lon, time, activity_id), on=.(filename_id)]
                if (! "lat" %in% names(gpx_data)) gpx_data[, lat := NA]
                if (! "lon" %in% names(gpx_data)) gpx_data[, lon := NA]
                if (! "heartrate" %in% names(gpx_data)) gpx_data[, heartrate := NA]
                setcolorder(gpx_data, c('activity_id', 'time', 'heartrate', 'lat', 'lon'))

                create_activities(
                    all_activities |> filter(activity_id == this_activity_id) |> select(-filename_id),
                    con
                )
                upload_activities(gpx_data[, .(activity_id, time, heartrate, lat, lon)], con)
            },
              error=function(e) {}
            )
        }
    })
}

insert_or_update_athlete <- function(settings, con) {
    settings <- mutate_all(settings, as.numeric)
    if (any(is.na(settings)) || any(settings < 0)) {
        return(FALSE)
    }

    athlete_exists <- nrow(tbl(con, "athlete") |> filter(athlete_id==1) |> collect()) > 0
    if (athlete_exists) {
        q <- "UPDATE athlete
              SET height = ?,
                  weight = ?,
                  maxHR = ?,
                  restHR = ?,
                  thresholdHR = ?
              WHERE athlete_id = 1;"
    } else {
        q <- "INSERT INTO athlete (athlete_id, height, weight, maxHR, restHR, thresholdHR)
              VALUES (1, ?, ?, ?, ?, ?)"
    }
    res <- dbSendStatement(con, q, params=as.list(unname(settings)))
    dbClearResult(res)
    TRUE
}

update_fitness <- function(con) {
    fit <- hr <- tbl(con, "heartrate") |> collect() |> as.data.table()
    athlete <- tbl(con, "athlete") |> filter(athlete_id == 1) |> collect()
    hrss <- fit[!is.na(heartrate),
                 .(hrss = calculate_hrss(heartrate,
                                         time,
                                         athlete$maxHR,
                                         athlete$restHR,
                                         athlete$thresholdHR,
                                         )),
                     by=.(activity_id)]
    q <- dbSendStatement(con, "DELETE FROM fitness;")
    dbClearResult(q)
    dbAppendTable(con, "fitness", hrss)
}

update_athlete_modal <- function(default_activity, con, failed=FALSE, first_time=FALSE) {
    if (first_time) {
        title <- "Configure app"
        easy_close <- FALSE
        footer <- tagList(
            modalButton("Cancel"),
            actionButton("insert_settings", "Continue")
        )
    } else {
        title <- "Update settings"
        easy_close <- TRUE
        footer <- tagList(
            modalButton("Cancel"),
            actionButton("update_settings", "Update settings and recalculate training scores")
        )

    }

    curr_settings <- tbl(con, "athlete") |>
                        collect() |>
                        as.list()
    activity_types <- tbl(con, "activity_types") |> pull(activity_type)
    modalDialog(
        title = title,
        h4("Athlete Settings"),
        textInput("update_height", "Height (cm)", value=curr_settings$height),
        textInput("update_weight", "Weight (kg)", value=curr_settings$weight),
        textInput("update_maxHR", "Maximum heartrate (bpm)", value=curr_settings$maxHR),
        textInput("update_restHR", "Resting heartrate (bpm)", value=curr_settings$restHR),
        textInput("update_thresholdHR", "Threshold heartrate (bpm)", value=curr_settings$thresholdHR),
        if (failed) {
            div(tags$b("Values must be positive numbers", style="color: red;"))
        },
        hr(),
        h4("App Settings"),
        radioButtons("update_default_activity", "Default activity",
                     choices=activity_types, selected=default_activity),
        easyClose =easy_close,
        fade=FALSE,
        footer=footer
    )
}

get_logged_in_user <- function(auth) {
    user <- GET("https://www.strava.com/api/v3/athlete", auth)
    if (user$status_code != 200) {
        return(NULL)
    }
    raw <- content(user)
    raw[c('username', 'profile_medium')]
}

get_recent_activities_meta <- function(auth, con) {
    # most recent activity
    most_recent_activity <- tbl(con, "activities") |>
                                slice_max(start_time, n=1) |>
                                pull(start_time) |>
                                as.integer()
    if (length(most_recent_activity) == 0) {
        most_recent_activity <- as.integer(as_datetime("1900-01-01"))
    }
    # Get list of all activities
    page_size <- 30
    i <- 1
    results <- list()
    while (TRUE) {
        all_activities <- GET("https://www.strava.com/api/v3/athlete/activities", auth,
                              query=list(after=most_recent_activity, per_page=page_size,
                                         page=i))
        if (all_activities$status_code != 200) {
            showNotification("Error occurred when trying to retrieve activities, try again later.",
                             type="error", duration=3)
            next
        }
        page_activities <- content(all_activities)
        results <- append(results, page_activities)
        if (length(page_activities) < page_size) break
        i <- i+1
    }
    # Flatten into data.table
    cols <- c(activity_id='id',
              activity_type='type',
              name='name',
              start_time='start_date',
              distance='distance',
              duration='elapsed_time',
              elevation='total_elevation_gain'
              )
    if (length(results) == 0 || "errors" %in% names(results)) {
        return(setNames(data.table(matrix(nrow = 0, ncol = length(cols))), names(cols)))
    }
    dt <- rbindlist(lapply(results, function(x) x[cols]))
    if (nrow(dt) == 0) return(dt)

    setnames(dt, unname(cols), names(cols))
    dt[, c('start_time', 'distance', 'duration') := .(as_datetime(start_time),
                                          distance / 1000,
                                          duration)]
    dt
}

# For each activity, get the associated stream
get_stream <- function(id, auth) {
    raw <- GET(sprintf("https://www.strava.com/api/v3/activities/%s/streams/heartrate,latlng", id),
                  auth, query=list(key_by_type='true', series_type='time'))
    if (raw$status_code != 200) {
        dt <- data.table(activity_id=numeric(), time_offset=numeric(), heartrate=numeric(), lat=numeric(), lon=numeric())
    } else {
        res <- content(raw)
        dt <- data.table(activity_id=id, time_offset = as.numeric(res$time$data))
    }

    if ("heartrate" %in% names(res)) {
        dt[, heartrate := as.numeric(res$heartrate$data)]
    } else {
        dt[, heartrate := NA ]
    }
    if ("latlng" %in% names(res)) {
        dt[, latlng := res$latlng$data]
        dt[, c('lat', 'lon') := .(as.numeric(map(latlng, function(x) x[[1]])),
                                   as.numeric(map(latlng, function(x) x[[2]])))]
        dt[, latlng := NULL]
    } else {
        dt[, c('lat', 'lon') := NA ]
    }

    dt
}

create_db <- function(con) {
    setup_sql <- read_file("setup/schema.sql")
    queries <- trimws(stringr::str_split_1(setup_sql, ";"))
    queries <- purrr::keep(queries, function(x) x != '')
    run_query <- function(q) {
        res <- dbSendStatement(con, q)
        dbClearResult(res)
    }
    map(queries, run_query)
}

load_strava_auth_from_cache <- function(cache_fn, app_name) {
    # Check for cached token
    if (!file.exists(cache_fn)) {
        return(NULL)
    }

    cache <- readRDS(cache_fn)
    cached_creds <- cache[vapply(cache, function(x) x[['app']][['appname']] == app_name, logical(1))]
    # Check have one (and only one!) matching app
    if (length(cached_creds) != 1) {
        return(NULL)
    }

    creds <- cached_creds[[1]]
    # Refresh if expired if possible
    if (creds$credentials$expires_at < now("UTC")) {
        if (creds$can_refresh()) {
            creds$refresh()
        } else {
            return(NULL)
        }
    }
    add_headers(Authorization=sprintf("Bearer %s", creds$credentials$access_token))
}

authenticate_strava <- function(session) {
    # Redirect user to authorise app at Strava's end
    session$sendCustomMessage("redirect_strava", "redirect_strava")
}

showImportModal <- function(x) {
    showModal(
        modalDialog(
            title = "Bulk import",
            h4("Upload an archive containing a Strava export"),
            easyClose = FALSE,
            fade=FALSE,
            footer=tagList(
                modalButton("Cancel"),
                fileInput("archive_upload", "Select file")
            )
        )
    )
}

sync <- function(session, con, cache_fn, auth) {
    has_updates <- TRUE
    # Create a Progress object
    withProgress(message="Syncing activities from Strava",
                 detail="Authenticating...",
                 value=0, {

        if (is.null(auth)) {
            showNotification(sprintf("Please reconnect to Strava, try deleting %s and trying again.", cache_fn),
                             type="error", duration=3)
            has_updates <- FALSE
            return()  # Only returns from withProgress
        }

        setProgress(value=.1, detail="Finding new activities")
        new_activities <- get_recent_activities_meta(auth, con)
        if (nrow(new_activities) == 0) {
            showNotification("No recent activities found!",
                             type="error", duration=3)
            has_updates <- FALSE
            return()  # Only returns from withProgress
        }
        # Does this show, or does it quickly get overwritten by the continuing progress bar?
        showNotification(sprintf("Found %d activities to sync", nrow(new_activities)),
                         type="message", duration=3)

        # Iterate over each activity, adding to DB in turn
        num_added <- 0
        n_activities <- nrow(new_activities)
        if (n_activities < 20) {
            increment <- 1
        } else {
            increment <- floor(0.05 * n_activities)
        }
        progress_update_indices <- seq(1, n_activities, by=increment)

        for (i in 1:n_activities) {
            if (i %in% progress_update_indices) {
                curr_pct <- i / n_activities
                overall_pct = 0.1 + (curr_pct * 0.9)
                setProgress(
                    value=overall_pct,
                    detail=sprintf("Activity %d/%d (%.2f%%)", i, n_activities, curr_pct*100)
                )
            }
            stream <- get_stream(new_activities$activity_id[i], auth)
            stream <- stream[new_activities[i, ],
                             .(activity_id, heartrate, lat, lon, time=start_time + time_offset),
                             on=.(activity_id)]
            create_activities(new_activities[i, ], con)
            upload_activities(stream, con)
            num_added <- num_added + 1
        }
        showNotification(sprintf("Synced %d activities!", num_added),
                         type="message", duration=3)
    })
    has_updates
}

has_auth_code <- function(params) {
    # OAuth in Shiny implementation taken from Hadley's gist here:
    # https://gist.github.com/hadley/144c406871768d0cbe66b0b810160528
    # params is a list object containing the parsed URL parameters. Return TRUE if
    # based on these parameters, it looks like auth codes are present that we can
    # use to get an access token. If not, it means we need to go through the OAuth
    # flow.
    return(!is.null(params$code))
}