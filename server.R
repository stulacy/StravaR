library(data.table)
library(ggmap)
library(gganimate)
library(ggrepel)
library(lubridate)
library(tidyverse)
library(hms)
library(DBI)
library(RSQLite)
library(shinyjs)
library(shiny)
library(DT)
library(zoo)
library(leaflet)
library(sf)
library(plotly)
library(httr)
source("utils.R")

PORT <- 8100
options(shiny.port=PORT)
APP_URL <- sprintf("http://localhost:%d/", PORT)

STRAVA_APP <- oauth_app(appname = "FitViz", 
                        key = Sys.getenv("FITVIZ_CLIENTID"), 
                        redirect_uri = APP_URL,
                        secret = Sys.getenv("FITVIZ_SECRET"))  
STRAVA_END <- oauth_endpoint(
    #request = "https://www.strava.com/oauth/authorize?",
    authorize = "https://www.strava.com/oauth/authorize",
    access = "https://www.strava.com/oauth/token")
STRAVA_SCOPE <- "activity:read_all"
STRAVA_URL <- oauth2.0_authorize_url(STRAVA_END, STRAVA_APP, scope = STRAVA_SCOPE)
REDIRECT_STRAVA_AUTH <- sprintf("Shiny.addCustomMessageHandler('redirect_strava', function(message) {window.location = '%s';});", 
                                STRAVA_URL)
OAUTH_CACHE <- ".httr-oauth"

con <- dbConnect(SQLite(), "data/strava.db")
YEARS <- seq(from=2018, to=2022, by=1)
N_YEARS <- length(YEARS)
FORM_OFFSET <- 40
# Original band definitions
TRAINING_COLOURS <- c("#9E0142", "#FDAE61", "#66C2A5")
TRAINING_BANDS <- data.table(
    type=c('Recovery', 'Neutral', 'Optimal'),
    upper=c(25, 5, -10),
    lower=c(5, -10, -30),
    colour=TRAINING_COLOURS
) 
# Apply scaling to make Form maximizable
TRAINING_BANDS[, c("upper", "lower") := .(FORM_OFFSET - upper,
                                 FORM_OFFSET - lower)]
TRAINING_BANDS[, mid := lower + (upper - lower) / 2 ]

insert_or_update_default_activity <- function(value) {
    default_exists <- nrow(tbl(con, "config") |> collect() |> filter(property == 'default_activity')) > 0
    if (default_exists) {
        q <- "UPDATE config SET value = ? WHERE property = 'default_activity'"
    } else {
        q <- "INSERT INTO config (property, value) VALUES ('default_activity', ?)"
    }
    res <- dbSendStatement(con, q, params=list(value))
    dbClearResult(res)
}

insert_or_update_athlete <- function(settings) {
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
        q <- "INSERT INTO athlete (height, weight, maxHR, restHR, thresholdHR)
              VALUES (?, ?, ?, ?, ?)"
    }
    res <- dbSendStatement(con, q, params=as.list(unname(settings)))
    dbClearResult(res)
    TRUE
}

update_fitness <- function() {
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

update_athlete_modal <- function(default_activity, failed=FALSE, first_time=FALSE) {
    if (first_time) {
        title <- "Configure app"
        easy_close <- FALSE
        footer <- tagList(
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

get_recent_activities_meta <- function(auth) {
    # most recent activity
    most_recent_activity <- tbl(con, "activities") |>
                                slice_max(start_time, n=1) |>
                                pull(start_time)
    if (length(most_recent_activity) == 0) {
        most_recent_activity <- as.numeric(as_datetime("1900-01-01"))
    }
    # Get list of all activities
    page_size <- 30
    i <- 1
    results <- list()
    while (TRUE) {
        all_activities <- GET("https://www.strava.com/api/v3/athlete/activities", auth,
                              query=list(after=most_recent_activity, per_page=page_size,
                                         page=i))
        # TODO error handle non 200!
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
    # TODO error handle non-200!
    raw <- GET(sprintf("https://www.strava.com/api/v3/activities/%s/streams/heartrate,latlng", id), 
                  auth, query=list(key_by_type='true', series_type='time'))
    res <- content(raw)
    dt <- data.table(
        activity_id=id,
        time_offset = as.numeric(res$time$data))
    
    if ("heartrate" %in% names(res)) {
        dt[, heartrate := as.numeric(res$heartrate$data)]
    }
    if ("latlng" %in% names(res)) {
        dt[, latlng := res$latlng$data]
        dt[, c('lat', 'lon') := .(as.numeric(map(latlng, function(x) x[[1]])),
                                   as.numeric(map(latlng, function(x) x[[2]])))]
        dt[, latlng := NULL]
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

auth_strava <- function(session) {
    # Check for cached token
    cache <- if (file.exists(OAUTH_CACHE)) {
        readRDS(OAUTH_CACHE)
    } else {
        list()
    }
    cached_creds <- cache[vapply(cache, function(x) x[['app']][['appname']] == 'FitViz', logical(1))]
    # Shouldn't have multiple credentials for the same app but just in case
    if (length(cached_creds) > 1) {
        return(NULL)
    }
    
    if (length(cached_creds) == 1) {
        creds <- cached_creds[[1]]
        # Refresh if expired if possible
        if (creds$credentials$expires_at < now("UTC")) {
            if (creds$can_refresh()) {
                creds$refresh()
            } else {
                # Redirect user to authorise app at Strava's end
                session$sendCustomMessage("redirect_strava", "redirect_strava")
            }
        }
        return(add_headers(Authorization=sprintf("Bearer %s", creds$credentials$access_token)))
    } else {
        # Redirect user to authorise app at Strava's end
        session$sendCustomMessage("redirect_strava", "redirect_strava")
    }
}

sync <- function(session) {
    has_updates <- TRUE
    # Create a Progress object
    withProgress(message="Syncing activities from Strava",
                 detail="Authenticating...",
                 value=0, {
        
        auth_header <- auth_strava(session)
        # TODO Update progress bar more often when actually downloading
        # I.e. update for each/every 5% streams
        if (is.null(auth_header)) {
            showNotification("Error authenticating. Try deleting .httr-oauth and trying again.",
                             type="error", duration=3)
            has_updates <- FALSE
            return()  # Only returns from withProgress
        }
        
        setProgress(value=.1, detail="Finding new activities")
        new_activities <- get_recent_activities_meta(auth_header)
        if (nrow(new_activities) == 0) {
            showNotification("No recent activities found!",
                             type="error", duration=3)
            has_updates <- FALSE
            return()  # Only returns from withProgress
        }
        # TODO does this show, or does it quickly get overwritten by the continuing progress bar?
        showNotification(sprintf("Found %d activities to sync", nrow(new_activities)),
                         type="success", duration=3)
        
        setProgress(value=.4, detail="Downloading heartrate and gps")
        streams <- rbindlist(lapply(new_activities$activity_id, get_stream, auth_header), fill=TRUE)
        # Add on starting time to go from relative to absolute
        streams <- streams[new_activities, .(activity_id, heartrate, lat, lon, time=start_time + time_offset), on=.(activity_id)]
        # Separate into heartrate and location
        hr <- streams[ !is.na(heartrate), .(activity_id, time, heartrate)]
        location <- streams[ !is.na(lat) & !is.na(lon), .(activity_id, time, lat, lon)]
        
        setProgress(value=.6, detail="Calculating fitness scores")
        # Calculate fitness scores
        athlete <- tbl(con, "athlete") |> collect()
        hrss <- hr[, .(hrss = calculate_hrss(heartrate,
                                             time,
                                             athlete$maxHR,
                                             athlete$restHR,
                                             athlete$thresholdHR,
                                             )), 
                         by=.(activity_id)]
        
        # Update DB!
        setProgress(value=.8, detail="Updating DB")
        # Add any new activity types
        act_types <- unique(new_activities$activity_type)
        curr_act_types <- tbl(con, "activity_types") |> pull(activity_types)
        new_types <- setdiff(act_types, curr_act_types)
        if (length(new_types) > 0) {
            dbAppendTable(con, "activity_types", data.frame(activity_type=new_types))
        }
        dbAppendTable(con, "activities", new_activities)
        dbAppendTable(con, "heartrate", hr)
        dbAppendTable(con, "location", location)
        dbAppendTable(con, "fitness", hrss)
        showNotification(sprintf("Synced %d activities!", nrow(new_activities)),
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

server <- function(input, output, session) {
    output$redirect_js <- renderUI({
        tags$head(tags$script(REDIRECT_STRAVA_AUTH))
    })
    if (length(dbListTables(con)) == 0) {
        create_db(con)
    }
    
    # If we're coming back from an OAuth request, complete the auth process
    params <- parseQueryString(isolate(session$clientData$url_search))
    if (has_auth_code(params)) {
        # Are we coming back from an OAuth request?
        # Manually create a token, cache it
        token <- oauth2.0_token(
            app = STRAVA_APP,
            endpoint = STRAVA_END,
            credentials = oauth2.0_access_token(STRAVA_END, STRAVA_APP, params$code),
            scope=STRAVA_SCOPE,
        )
        token$cache(OAUTH_CACHE)
        
        # Sync will look in cache and find token
        sync(session)
    }
    
    n_activities <- tbl(con, "activities") |> count() |> pull()
    if (n_activities == 0 && !has_auth_code(params)) {
        showModal(
            modalDialog(
                title = "Initial setup",
                h4("Let's get things setup!"),
                easyClose = FALSE,
                fade=FALSE,
                footer=tagList(
                    actionButton("show_setup_athlete", "Continue")
                )
            )
        )
    }
    
    fitness_updated <- reactiveVal(value=FALSE)
    activities_synced <- reactiveVal(value=0)
    initial_default_activity <- tbl(con, "config") |>
                            filter(property == 'default_activity') |>
                            pull(value)
    if (length(initial_default_activity) == 0) {
        initial_default_activity <- 'Run'
    }
    default_activity <- reactiveVal(value=initial_default_activity)
    
    observeEvent(input$show_setup_athlete, {
        showModal(update_athlete_modal(default_activity(), first_time = TRUE))
    })
    
    # Get all dates from last 52 weeks
    days_til_sunday <- 7 - wday(today(), week_start=1)
    first_day <- today() + days(days_til_sunday) - weeks(52) + days(1)
    all_dates <- data.table(date=seq.Date(from=first_day, to=today() + days(days_til_sunday), by="day"))
    all_dates[, c("wday", "week", "year") := .(wday(date, week_start=1),
                                               isoweek(date),
                                               year(date))]
    all_dates[, year_diff := year - min(year)]
    all_dates[, week := week + year_diff * 52]
    # The first days before Monday in the new Year will be off,
    # since technically they'll be week 52 and will add an additional 52
    all_dates[ week == 104, week := 52]
    all_dates[, week := paste0("week_", week - min(week))]
    all_dates[, c('year_diff', 'year') := NULL]
    
    # Work out the week number with first months
    all_dates[ wday == 1, month := month(date)]
    month_breaks <- all_dates[wday == 1, head(.SD, 1L), by=month][, as.integer(gsub("week_", "", week))]
    
    observeEvent(input$settings, {
        showModal(update_athlete_modal(default_activity()))
    })
    
    observeEvent(input$refresh, {
        if (sync(session)) {
            activities_synced(activities_synced() + 1)
        }
    })
    
    observeEvent(input$first_sync, {
        removeModal()
        if (sync(session)) {
            activities_synced(activities_synced() + 1)
        }
    })
    
    observeEvent(input$insert_settings, {
        default_activity(input$update_default_activity)
        insert_or_update_default_activity(input$update_default_activity)
        
        athlete_inputs <- data.frame(
            height=input$update_height,
            weight=input$update_weight,
            maxHR=input$update_maxHR,
            restHR=input$update_restHR,
            thresholdHR=input$update_thresholdHR
        )
        
        validated <- insert_or_update_athlete(athlete_inputs)
        if (!validated) {
            showModal(update_athlete_modal(default_activity(), failed=TRUE, first_time = TRUE))
            return()
        }
        
        # Now prompt to sync data
        showModal(
            modalDialog(
                title = "Sync with Strava",
                p("Great, now let's get some data! Press the button below and follow the prompts to authorise FitViz to access your Strava data."),
                easyClose = FALSE,
                fade=FALSE,
                footer=tagList(
                    actionButton("first_sync", "Sync data")
                )
            )
        )
    })
    
    observeEvent(input$update_settings, {
        # Update default activity
        default_activity(input$update_default_activity)
        insert_or_update_default_activity(input$update_default_activity)
        
        # Update athlete
        athlete_inputs <- data.frame(
            height=input$update_height,
            weight=input$update_weight,
            maxHR=input$update_maxHR,
            restHR=input$update_restHR,
            thresholdHR=input$update_thresholdHR
        )
        validated <- insert_or_update_athlete(athlete_inputs)
        if (!validated) {
            showModal(update_athlete_modal(default_activity(), failed=TRUE))
            return()
        }
        
        # Update fitness
        update_fitness()
        fitness_updated(TRUE)  # Triggers a redraw of the training plot
        removeModal()
    })
    
    output$activity_type_select_wrapper <- renderUI({
        activity_types <- tbl(con, "activity_types") |> pull(activity_type)
        checkboxGroupInput("activity_type_select",
                           "Activity Type",
                           activity_types,
                           selected=default_activity())
    })
    
    activities_summary <- eventReactive(c(input$activity_type_select, activities_synced()), {
        types <- input$activity_type_select
        df <- tbl(con, "activities") |>
            filter(activity_type %in% types) |>
            collect() |>
            mutate(start_time = as_datetime(start_time),
                   name = sprintf("<a href='https://strava.com/activities/%s'>%s</a>", 
                                  activity_id, 
                                  name),
                   date = as_date(start_time),
                   distance = round(distance, 1),
                   elevation = round(elevation),
                   duration = as.period(duration(duration, units="secs")),
                   dur_fmt = sprintf("%dh %dm %ds", hour(duration),
                                     minute(duration),
                                     round(second(duration))),
                   dur_fmt = gsub("0h ", "", dur_fmt)) |>
            arrange(desc(start_time)) |>
            select(
                date,
                activity_type,
                name,
                distance,
                dur_fmt,
                elevation
           )
        validate(need(nrow(df) > 0, "No activities found"))
        df
    }, ignoreInit = TRUE)
    
    output$activities <- renderDT({
        activities_summary() |>
            rename(
                Date=date,
                Type=activity_type,
                Name=name,
                `Distance (km)`=distance,
                Duration=dur_fmt,
                `Elevation (m)`=elevation
               )
    }, escape=FALSE)
    
    activities_calendar <- eventReactive(activities_summary(), {
        df_raw <- activities_summary() |>
            filter(date >= first_day) |>
            setDT()
        df <- df_raw[, .(distance = sum(distance)), by=.(date)]
        
        # Add all possible dates
        df <- df[all_dates, on='date']
        
        # Form wide and add 0s for days with no activities
        df_wide <- dcast(df, wday ~ week, value.var="distance")
        setorder(df_wide, -wday)
        setcolorder(df_wide, c("wday", paste("week", 0:51, sep="_")))
        setnafill(df_wide, fill=0)
        df_wide
    }, ignoreInit = FALSE)
    
    output$calendar <- renderPlotly({
        df_raw <- activities_summary() |> filter(date >= first_day) |> as.data.table()
        df_wide <- activities_calendar()
        
        week_avg <- df_raw[ date > (today() - days(7)), .(mu=round(mean(distance)))]$mu
        if (length(week_avg) == 0) week_avg <- 0
        
        colour_granularity <- 9
        plot_ly(
            z = as.matrix(df_wide[, -c("wday")]),
            type = "heatmap",
            text=apply(matrix(as.character(all_dates$date), nrow=7, ncol=52), 2, rev),
            colors=c("white", RColorBrewer::brewer.pal(colour_granularity, "Oranges")[2:colour_granularity]),
            hovertemplate="%{z}km on %{text}<extra></extra>",
            xgap=2,
            ygap=2,
            showscale=FALSE
        ) |>
            layout(yaxis=list(scaleanchor='x', scaleratio=1,
                              zeroline=FALSE,
                              constrain="domain",
                              ticks="",
                              tickmode="array",
                              tickvals=seq(0, 6),
                              ticktext=c("Sun", "Sat", "Fri", "Thurs", "Weds", "Tues", "Mon")),
                   xaxis=list(zeroline=FALSE,
                              side="top",
                              ticks="",
                              tickmode="array",
                              tickvals=month_breaks,
                              ticktext=c("Jan", "Feb", "Mar",
                                         "Apr", "May", "Jun",
                                         "Jul", "Aug", "Sep",
                                         "Oct", "Nov", "Dec")),
                   title=sprintf("%d %ss in the last year with an average of %dkm\n%d in the last week with an average of %dkm", 
                                 nrow(df_raw),
                                 tolower(paste(unique(df_raw$activity_type), collapse='+')),
                                 round(mean(df_raw$distance)),
                                 sum(df_raw$date > (today() - days(7))),
                                 week_avg
                                 )
                   ) |>
            plotly::config(displayModeBar=FALSE) 
    })
    
    mileage_df <- eventReactive(c(input$activity_type_select, activities_synced()), {
        types <- input$activity_type_select
        dt <- tbl(con, "activities") |>
            filter(activity_type %in% types) |>
            select(start_time, distance) |>
            collect() |>
            setDT()
        dt[, date := as_date(as_datetime(start_time))]
        dt[, .(distance = sum(distance)), by=date]
        validate(need(nrow(dt) > 0, "No activities found"))
        dt
    })
    
    hrss <- eventReactive({
            input$activity_type_select
            fitness_updated()
            activities_synced()
        }, 
        {
        types <- input$activity_type_select
        hrss <- tbl(con, "fitness") |>
                        inner_join(tbl(con, "activities"), by="activity_id") |>
                        filter(activity_type %in% types) |>
                        collect() |>
                        mutate(date = as_date(as_datetime(start_time))) |>
                        setDT()
        validate(need(nrow(hrss) > 0, "No activities found"))
        # Summarise per date
        hrss <- hrss[, .(hrss = sum(hrss)), by=date][order(date)]
        # Make entry for every day as need to run equation everyday as time isn't
        # parameterised in it
        all_dates <- data.table(date=seq.Date(from=min(hrss$date), to=max(hrss$date), by=1))
        hrss <- merge(hrss, all_dates, on='date', all.y=TRUE)
        hrss[ is.na(hrss), hrss := 0 ]
        
        hrss[, c('Fitness', 'Fatigue', 'Form') := 0]
        
        # Now calculate fitness trends!
        for (i in seq(nrow(hrss))) {
            if (i == 1) {
                prev_fitness <- 0
                prev_fatigue <- 0
            } else {
                prev_fitness <- hrss$Fitness[i - 1]
                prev_fatigue <- hrss$Fatigue[i - 1]
            }
            this_hrss <- hrss$hrss[i]
            
            hrss$Fitness[i] <- prev_fitness + (this_hrss - prev_fitness)*(1-exp(-1/42))
            hrss$Fatigue[i] <- prev_fatigue + (this_hrss - prev_fatigue)*(1-exp(-1/7))
            hrss$Form[i] <- prev_fitness - prev_fatigue
        }
        fitness_updated(FALSE)
    })
    
    routes_df <- eventReactive(c(input$activity_type_select, activities_synced()), {
        types <- input$activity_type_select
        dt <- tbl(con, "activities") |>
            filter(activity_type %in% types) |>
            inner_join(tbl(con, "location"), by="activity_id") |>
            select(activity_id, name, time, lat, lon) |>
            collect() |>
            setDT()
        
        dt[, time := as_datetime(time)]
        dt[, date := as_date(time)]
        setorder(dt, time)
        validate(need(nrow(dt) > 0, "No activities found"))
        dt
    })
    
    output$mileage_cumulative <- renderPlotly({
        df2 <- mileage_df()
        df2[, c('Year', 'Date') := .(as.factor(year(date)), 
                                     as_datetime(sprintf("2000-%d-%d", 
                                                         month(date),
                                                         day(date))))]
        setorder(df2, Date)
        df2[, c('Distance', 'label') := .(cumsum(distance),
                                          ifelse(Date == max(Date),
                                                 as.character(Year),
                                                 NA_character_)),
            by=Year]
        
        p <- plot_ly(x=~Date)
        for (group in unique(df2$Year)) {
            p <- p |> add_lines(y=~Distance, 
                                name=group, 
                                text=group,
                                data=df2 |> filter(Year == group),
                                showlegend=F,
                                hovertemplate=paste("Year: %{text}<br>",
                                                    "Distance: %{y:.0f}km<br>",
                                                    "Date: %{x|%b %d}<extra></extra>"))# |>
        }
        
        # Get coordinates of last point for each line and add text
        foo <- jsonlite::parse_json(plotly_json(p, jsonedit=FALSE))
        for (i in 1:length(foo$data)) {
            n_points <- length(foo$data[[i]]$x)
            sub_df <- tibble(
                last_y = foo$data[[i]]$y[[n_points]],
                Date = as_datetime(foo$data[[i]]$x[[n_points]]) + days(12)
            )
            col <- foo$data[[i]]$line$color
            this_year <- foo$data[[i]]$text[[1]]
            p <- p |> add_trace(y=~last_y,
                                name=this_year,
                                text=this_year,
                                type="scatter",
                                textfont=list(color=col, size=14),
                                mode="marker+text",
                                data=sub_df,
                                showlegend=F,
                                hoverinfo='none')
        }
        
        ggplotly(p, tooltip=c('colour', 'x', 'y')) |>
            layout(yaxis = list(hoverformat = '.5f'),
                   xaxis = list(title="", tickformat="%b"))
    })
    
    output$mileage_weekly <- renderPlotly({
        df <- mileage_df()
        all_dates <- data.table(date=seq.Date(from=min(df$date), to=max(df$date), by=1))
        df <- merge(df, all_dates, on='date', all.y=TRUE)
        df[, distance := ifelse(is.na(df$distance), 0, df$distance)]
        df[, weeklyrate := rollsum(zoo(distance, date), 7, fill=NA)]
        df[, mileage := rollmean(zoo(weeklyrate, date), 7, fill=NA)]
        
        # Generate alternate shaded years
        years <- unique(year(df$date))
        start <- as_datetime(sapply(years, function(x) sprintf("%d-01-01", x)))
        end <- as_datetime(sapply(years, function(x) sprintf("%d-12-31", x)))
        ribbons <- tibble(year=years, start=start, end=end,
                          ymin=0, ymax=1.10*max(df$mileage, na.rm=T)) |>
            mutate(shaded = year %% 2 == 0)
        # Replace the first date with the first actual date I ran
        ribbons$start[1] <- min(df$date)
        
        p <- plot_ly(x=~date,
                     y=~mileage,
                     data=df,
                     type="scatter",
                     mode="lines",
                     showlegend=F,
                     hovertemplate=paste("Date: %{x|%b %d %Y}<extra></extra><br>",
                                         "Weekly mileage: %{y:.0f}km"))
        
        # Add alternate year shading
        rectangles <- vector(mode="list", length=nrow(ribbons)) 
        for (i in 1:nrow(ribbons)) {
            rectangles[[i]] <- list(
                type="rect",
                fillcolor="#d2d2d2",
                line=list(color="#d2d2d2"),
                opacity=if (i %% 2 == 0) 0 else 0.3,
                x0=ribbons$start[i],
                x1=ribbons$end[i],
                y0=ribbons$ymin[i],
                y1=ribbons$ymax[i]
            )
        }
        ggplotly(p, tooltip=c('x', 'y')) |>
            layout( yaxis = list(title="7-day rolling average of weekly distance", 
                                 hoverformat = '.0f'),
                    xaxis = list(title=""),
                    shapes=rectangles)
    })
    
    output$training <- renderPlotly({
        df <- hrss()
        X_BUFFER_DAYS <- 40
        
        df[, Form := FORM_OFFSET - Form]
        
        df <- TRAINING_BANDS[df, 
              .(date, Form, Fitness, Fatigue, type, lower, upper), 
              on=.(lower > Form, upper < Form)]
        
        curr_status <- tail(df, 1)$type
        curr_status_colour <- TRAINING_BANDS[tail(df, 1), on=.(type)]$colour
        
         p1 <- plot_ly(x=~date, y=~Form, data=df,
                       type="scatter", mode="lines",
                       text=~type,
                       name="Form",
                       showlegend=FALSE,
                       hoverlabel=list(bgcolor=df$colour)) |>
                        layout(title=list(text=sprintf("Current training status: %s", curr_status),
                                          font=list(color=curr_status_colour)))
         
         rectangles <- vector(mode="list", length=3)
         for (i in 1:nrow(TRAINING_BANDS)) {
             rectangles[[i]] <- list(
                 type="rect",
                 fillcolor=TRAINING_BANDS$colour[i],
                 line=list(color=TRAINING_BANDS$colour[i]),
                 opacity=0.2,
                 x0=min(df$date),
                 hoverlabel=list(bgcolor=TRAINING_COLOURS[i]),
                 x1=today() + days(X_BUFFER_DAYS),
                 y0=TRAINING_BANDS$lower[i],
                 y1=TRAINING_BANDS$upper[i]
             )
         }
         p1 <- p1 |> 
                 layout(shapes=rectangles,
                        yaxis=list(title="Form",
                                   hoverformat=".0f")) 
         for (i in 1:nrow(TRAINING_BANDS)) {
             p1 <- p1 |>
                 add_annotations(
                     text=TRAINING_BANDS$type[i],
                     x=today() + days(X_BUFFER_DAYS/2),
                     y=TRAINING_BANDS$mid[i],
                     font_color=TRAINING_BANDS$colour[i],
                     showarrow=FALSE
                 ) 
         }
         
         p2 <- plot_ly(x=~date, y=~Fitness, data=df,
                       type="scatter", mode="lines",
                       name="Fitness",
                       showlegend=FALSE) |>
                    layout(
                        yaxis=list(
                            title="Fitness",
                            hoverformat=".0f"
                            )
                        )
         p3 <- plot_ly(x=~date, y=~Fatigue, data=df,
                       type="scatter", mode="lines",
                       name="Fatigue",
                       showlegend=FALSE) |>
                    layout(
                        yaxis=list(
                            title="Fatigue",
                            hoverformat=".0f"
                        )
                    )
         subplot(p1, p2, p3, shareX = TRUE, nrows=3,
                      titleY=TRUE) |>
            layout(hovermode="x unified",
                   xaxis=list(title=""))
    })
    
    output$routes <- renderLeaflet({
        df <- routes_df()
        df_sf <- df |> 
            arrange(time) |>
            group_by(activity_id, name, date) |>
            nest() |>
            mutate(line = map(data, function(x) st_linestring(as.matrix(x |> select(lon, lat))))) |>
            select(-data) |>
            ungroup() |>
            st_sf(crs = "EPSG:4326") |>
            mutate(label = sprintf("%s - %s", name, date))
        
        df_sf |>
            leaflet() |>
            addTiles() |>
            addPolylines(label=df_sf$label,
                         color="steelblue")
    })
}

