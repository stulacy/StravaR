library(data.table)
library(tidyverse)
library(DBI)
library(shinyjs)
library(shiny)
library(DT)
library(zoo)
library(leaflet)
library(FITfileR)
library(duckdb)
library(gpx)
library(sf)
library(plotly)
library(httr)
source("utils.R")

PORT <- 8100
options(shiny.port=PORT)
options(shiny.maxRequestSize=200*1024^2)
APP_URL <- sprintf("http://localhost:%d/", PORT)
DB_FN <- "data.db"

STRAVA_APP_NAME <- 'FitViz'
STRAVA_APP <- oauth_app(appname = STRAVA_APP_NAME,
                        key = Sys.getenv("FITVIZ_CLIENTID"), 
                        redirect_uri = APP_URL,
                        secret = Sys.getenv("FITVIZ_SECRET"))  
STRAVA_END <- oauth_endpoint(
    authorize = "https://www.strava.com/oauth/authorize",
    access = "https://www.strava.com/oauth/token")
STRAVA_SCOPE <- "activity:read_all"
STRAVA_URL <- oauth2.0_authorize_url(STRAVA_END, STRAVA_APP, scope = STRAVA_SCOPE)
REDIRECT_STRAVA_AUTH <- sprintf("Shiny.addCustomMessageHandler('redirect_strava', function(message) {window.location = '%s';});", 
                                STRAVA_URL)
OAUTH_CACHE <- ".httr-oauth"
AUTH_HEADER <- NULL

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

CONNECT_TO_STRAVA_BUTTON <- tags$li(
                actionLink(
                    "connectStrava",
                    label=img(src="resources/btn_strava_connectwith_orange.png"),
                ),
                class = "dropdown")

server <- function(input, output, session) {
    # Javascript to add redirect to the Strava auth page
    output$redirect_js <- renderUI({
        tags$head(tags$script(REDIRECT_STRAVA_AUTH))
    })
    
    # Connect to and create DB if doesn't exist
    con <- dbConnect(duckdb(), DB_FN)
    if (length(dbListTables(con)) == 0) {
        create_db(con)
    }
    
    # If we're coming back from an OAuth request, complete the auth process
    params <- parseQueryString(isolate(session$clientData$url_search))
    if (has_auth_code(params)) {
        # Are we coming back from an OAuth request?
        # Manually create a token, cache it, about to load it next
        token <- oauth2.0_token(
            app = STRAVA_APP,
            endpoint = STRAVA_END,
            credentials = oauth2.0_access_token(STRAVA_END, STRAVA_APP, params$code),
            scope=STRAVA_SCOPE,
        )
        token$cache(OAUTH_CACHE)
    }
    AUTH_HEADER <<- load_strava_auth_from_cache(OAUTH_CACHE, STRAVA_APP_NAME)
    
    # Prompt the user to configure settings on first use
    n_activities <- tbl(con, "activities") |> count() |> pull()
    if (n_activities == 0 && !has_auth_code(params)) {
        showModal(
            modalDialog(
                title = "Initial setup",
                h4("Let's get things setup!"),
                easyClose = FALSE,
                fade=FALSE,
                footer=tagList(
                    modalButton("Some other time"),
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
        showModal(update_athlete_modal(default_activity(), con, first_time = TRUE))
    })
    
    observeEvent(input$settings, {
        showModal(update_athlete_modal(default_activity(), con))
    })
    
    observeEvent(input$refresh, {
        has_data <- tbl(con, "activities") |> count() |> pull(n) > 0
        tags <- list(
            modalButton("Cancel"),
            actionButton("import", "Bulk import Strava export")
        )
        if (has_data) {
            text <- "New activities can either be downloaded directly from the Strava API or through an exported Strava archive."
            tags[[3]] <- actionButton("sync", "Sync data")
        } else {
            text <- "The first data upload must be from a Strava export, subsequent ones can 1-click sync directly with Strava."
        }
        showModal(
            modalDialog(
                title = "Download recent activities",
                p(text),
                easyClose = TRUE,
                fade=FALSE,
                footer=do.call(tagList, tags)
            )
        )
    })
    
    observeEvent(input$connectStrava, {
        authenticate_strava(session)
    })
    
    
    observeEvent(input$sync, {
        removeModal()
        if (sync(session, con, OAUTH_CACHE, AUTH_HEADER)) {
            activities_synced(activities_synced() + 1)
        }
    })
    
    observeEvent(input$import, {
        removeModal()
        showImportModal()
    })
    
    
    observeEvent(input$archive_upload, {
        handle_export_archive(input$archive_upload, con)
        activities_synced(activities_synced() + 1)
        removeModal()
    })
    
    
    observeEvent(input$insert_settings, {
        default_activity(input$update_default_activity)
        insert_or_update_default_activity(input$update_default_activity, con)
        
        athlete_inputs <- data.frame(
            height=input$update_height,
            weight=input$update_weight,
            maxHR=input$update_maxHR,
            restHR=input$update_restHR,
            thresholdHR=input$update_thresholdHR
        )
        
        validated <- insert_or_update_athlete(athlete_inputs, con)
        if (!validated) {
            showModal(update_athlete_modal(default_activity(), con, failed=TRUE, first_time = TRUE))
            return()
        }
        
        # Now prompt to sync data
        showModal(
            modalDialog(
                title = "Add activities",
                p("Great, now let's get some data! Your first upload must be from a Strava export, afterwards you can 1-click sync."),
                easyClose = FALSE,
                fade=FALSE,
                footer=tagList(
                    modalButton("Cancel"),
                    actionButton("import", "Bulk import Strava export")
                )
            )
        )
    })
    
    observeEvent(input$update_settings, {
        # Update default activity
        default_activity(input$update_default_activity)
        insert_or_update_default_activity(input$update_default_activity, con)
        
        # Update athlete
        athlete_inputs <- data.frame(
            height=input$update_height,
            weight=input$update_weight,
            maxHR=input$update_maxHR,
            restHR=input$update_restHR,
            thresholdHR=input$update_thresholdHR
        )
        validated <- insert_or_update_athlete(athlete_inputs, con)
        if (!validated) {
            showModal(update_athlete_modal(default_activity(), con, failed=TRUE))
            return()
        }
        
        # Update fitness
        update_fitness(con)
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
            mutate(start_time = start_time,
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
        # Calculate daily total distance for each day of last year
        all_dates <- get_calendar_dates()
        df_raw <- activities_summary() |>
            filter(date >= min(all_dates$date)) |>
            setDT()
        df <- df_raw[, .(distance = sum(distance)), by=.(date)]
        
        # Add all possible dates
        df <- df[all_dates, on='date']
        
        # Form wide and add 0s for days with no activities
        df_wide <- dcast(df, wday ~ week, value.var="distance")
        setorder(df_wide, -wday)
        setcolorder(df_wide, c("wday", paste("week", 0:51, sep="_")))
        setnafill(df_wide, fill=0)
        list(calendar=df_wide, all_dates=all_dates)
    }, ignoreInit = FALSE)
    
    output$calendar <- renderPlotly({
        calendar_raw <- activities_calendar()
        df_wide <- calendar_raw$calendar
        all_dates <- calendar_raw$all_dates
        month_breaks <- all_dates[wday == 1, head(.SD, 1L), by=month][, as.integer(gsub("week_", "", week))]
        
        # TODO can't this be calculated from df_wide?
        df_raw <- activities_summary() |> 
            filter(date >= min(all_dates$date)) |> 
            as.data.table()
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
            mutate(date = as_date(start_time)) |>
            setDT()
        dt <- dt[, .(distance = sum(distance)), by=date]
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
                        mutate(date = as_date(start_time)) |>
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
        hrss
    })
    
    routes_df <- eventReactive(c(input$activity_type_select, activities_synced()), {
        types <- input$activity_type_select
        dt <- tbl(con, "activities") |>
            filter(activity_type %in% types) |>
            inner_join(tbl(con, "location"), by="activity_id") |>
            select(activity_id, name, time, lat, lon) |>
            collect() |>
            mutate(date = as_date(time)) |>
            setDT()
        
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
        df <- df[, .(distance = sum(distance, na.rm=T)), by=.(date)]
        # Have date, start_time, distance, Year, Date, Distance, label
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
                   xaxis=list(title="", 
                              range=c(today() - months(1), today())))
    })
    
    output$stravaConnectPlaceholder <- renderUI({
        if (is.null(AUTH_HEADER)) {
            CONNECT_TO_STRAVA_BUTTON
        } else {
            user <- get_logged_in_user(AUTH_HEADER)
            if (is.null(user)) {
                showNotification(sprintf("Unable to identify logged in user, try deleting %s and trying again.", OAUTH_CACHE),
                                 type="error", duration=3)
                CONNECT_TO_STRAVA_BUTTON
            } else {
                tags$li(
                  div(
                      a(href="https://strava.com",
                        span(user$username, class="strava-profile-text")),
                      img(src=user$profile_medium, class="strava-profile-image"),
                      class="strava-profile-container"
                  )
                )  
            }
        }
    })
    
    output$routes <- renderLeaflet({
        df <- routes_df()
        df_sf <- df |> 
            group_by(activity_id, name, date) |>
            arrange(time) |>
            nest() |>
            mutate(line = map(data, function(x) st_linestring(as.matrix(x |> select(lon, lat))))) |>
            select(-data) |>
            ungroup() |>
            st_sf(crs = "EPSG:4326") |>
            mutate(label = sprintf("%s - %s", name, date))
        
        df_sf |>
            leaflet() |>
            addTiles() |>
            addPolylines(label=~label,
                         color="steelblue")
    })
    
    onStop(function() {
        dbDisconnect(con, shutdown=TRUE)
    })
}