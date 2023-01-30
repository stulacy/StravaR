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

con <- dbConnect(SQLite(), "data/strava.db")
ACTIVITY_TYPES <- tbl(con, "activity_types") |>
                    pull(activity_type)
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

strava_auth <- function() {
    strava_app <- oauth_app(appname = "FitViz", key = Sys.getenv("FITVIZ_CLIENTID"), 
                            secret = Sys.getenv("FITVIZ_SECRET"))  
    
    strava_end <- oauth_endpoint(
        #request = "https://www.strava.com/oauth/authorize?",
        authorize = "https://www.strava.com/oauth/authorize",
        access = "https://www.strava.com/oauth/token")
    
    res <- oauth2.0_token(endpoint = strava_end, 
                   app = strava_app, 
                   scope = "activity:read_all", 
                   cache = TRUE)
    auth_header <- add_headers(Authorization=sprintf("Bearer %s", res$credentials$access_token))
}

get_recent_activities_meta <- function(auth) {
    # most recent activity
    most_recent_activity <- tbl(con, "activities") |>
                                slice_max(start_time, n=1) |>
                                pull(start_time)
    # Get list of all activities
    page_size <- 30
    i <- 1
    results <- list()
    while (TRUE) {
        all_activities <- GET("https://www.strava.com/api/v3/athlete/activities", auth,
                              query=list(after=most_recent_activity, per_page=page_size,
                                         page=i))
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


function(input, output, session) {
    
    fitness_updated <- reactiveVal(value=FALSE)
    activities_synced <- reactiveVal(value=0)
    initial_default_activity <- tbl(con, "config") |>
                            filter(property == 'default_activity') |>
                            pull(value)
    default_activity <- reactiveVal(value=initial_default_activity)
    
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
    
    update_athlete_modal <- function(failed=FALSE) {
        curr_settings <- tbl(con, "athlete") |>
                            collect() |>
                            as.list()
        modalDialog(
            title = "Update settings",
            h4("Athlete Settings"),
            textInput("update_height", "Height (cm)", value=curr_settings$height),
            textInput("update_weight", "Weight (cm)", value=curr_settings$weight),
            textInput("update_maxHR", "Maximum heartrate (bpm)", value=curr_settings$maxHR),
            textInput("update_restHR", "Resting heartrate (bpm)", value=curr_settings$restHR),
            textInput("update_thresholdHR", "Threshold heartrate (bpm)", value=curr_settings$thresholdHR),
            if (failed) {
                div(tags$b("Values must be positive numbers", style="color: red;"))
            },
            hr(),
            h4("App Settings"),
            radioButtons("update_default_activity", "Default activity",
                         choices=ACTIVITY_TYPES, selected=default_activity()),
            easyClose = TRUE,
            fade=FALSE,
            footer=tagList(
                modalButton("Cancel"),
                actionButton("update_settings", "Update settings and recalculate training scores")
            )
        )
    }
    
    sync <- function() {
        # Create a Progress object
        withProgress(message="Syncing activities from Strava",
                     detail="Authenticating...",
                     value=0, {
            creds <- strava_auth()
            
            setProgress(value=.2, detail="Finding new activities")
            new_activities <- get_recent_activities_meta(creds)
            if (nrow(new_activities) == 0) {
                showNotification("No recent activities found!",
                                 type="error", duration=3)
                return()
            }
            setProgress(value=.4, detail="Downloading heartrate and gps")
            streams <- rbindlist(lapply(new_activities$activity_id, get_stream, creds), fill=TRUE)
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
            dbAppendTable(con, "activities", new_activities)
            dbAppendTable(con, "heartrate", hr)
            dbAppendTable(con, "location", location)
            dbAppendTable(con, "fitness", hrss)
            activities_synced(activities_synced() + 1)
            showNotification(sprintf("Synced %d activities!", nrow(new_activities)),
                             type="message", duration=3)
        })
    }
    
    observeEvent(input$settings, {
        showModal(update_athlete_modal())
    })
    
    observeEvent(input$refresh, {
        sync()
    })
    
    observeEvent(input$update_settings, {
        # Update default activity if changed, needing to update:
        # this session (UI) and BD
        if (input$update_default_activity != default_activity()) {
            default_activity(input$update_default_activity)
            q <- dbSendStatement(con, "UPDATE config SET 
                                    value = ?
                                  WHERE property = 'default_activity';",
                              params=list(input$update_default_activity))
            dbClearResult(q)
        }
        
        # Updating athlete values is more complex
        # First want to validate they are positive numbers
        new_vals <- data.frame(
            height=input$update_height,
            weight=input$update_weight,
            maxHR=input$update_maxHR,
            restHR=input$update_restHR,
            thresholdHR=input$update_thresholdHR
        )
        validated <- TRUE
        new_vals <- mutate_all(new_vals, as.numeric)
        if (any(is.na(new_vals)) || any(new_vals < 0)) validated <- FALSE
        
        if (validated) {
            # Update DB with the new values, update fitness scores,
            # and trigger replot of fitness plot
            q <- dbSendStatement(con, "UPDATE athlete SET 
                                    height = ?,
                                    weight = ?,
                                    maxHR = ?,
                                    restHR = ?,
                                    thresholdHR = ?
                                  WHERE athlete_id = 1;",
                              params=as.list(unname(new_vals)))
            dbClearResult(q)
            # TODO should refactor into function
            fit <- hr <- tbl(con, "heartrate") |> collect() |> as.data.table()
            hrss <- fit[!is.na(heartrate), 
                         .(hrss = calculate_hrss(heartrate,
                                                 time,
                                                 new_vals$maxHR,
                                                 new_vals$restHR,
                                                 new_vals$thresholdHR,
                                                 )), 
                             by=.(activity_id)]
            q <- dbSendStatement(con, "DELETE FROM fitness;")
            dbClearResult(q)
            dbAppendTable(con, "fitness", hrss)
            fitness_updated(TRUE)  # Triggers a redraw of the training plot
            removeModal()
        } else {
            showModal(update_athlete_modal(failed=TRUE))
        }
    })
    
    output$activity_type_select_wrapper <- renderUI({
        checkboxGroupInput("activity_type_select",
                           "Activity Type",
                           ACTIVITY_TYPES,
                           selected=default_activity())
    })
    
    activities_summary <- eventReactive(c(input$activity_type_select, activities_synced()), {
        types <- input$activity_type_select
        tbl(con, "activities") |>
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
        hrss
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