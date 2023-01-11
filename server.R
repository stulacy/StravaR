library(data.table)
library(ggmap)
library(gganimate)
library(ggrepel)
library(lubridate)
library(tidyverse)
library(hms)
library(osmdata)
library(DBI)
library(RSQLite)
library(shinyjs)
library(shiny)
library(DT)
library(zoo)
library(leaflet)
library(sf)
library(plotly)

con <- dbConnect(SQLite(), "data/strava.db")
ACTIVITY_TYPES <- tbl(con, "activity_types") |>
                    pull(activity_type)
YEARS <- seq(from=2018, to=2022, by=1)
N_YEARS <- length(YEARS)

calculate_hrss <- function(hr, ts, HRmax, HRrest, LTHR, k=1.92) {
   hrr <- (hr - HRrest) / (HRmax - HRrest) 
   times_diff_min <- c(0, diff(ts)) / 60
   trimp_activity <- sum(times_diff_min * hrr * 0.64 * exp(k * hrr))
   hrr_lthr <- (LTHR - HRrest) / (HRmax - HRrest)
   trimp_lthrhour <- 60 * hrr_lthr * 0.64 * exp(k * hrr_lthr)
   hrss <- trimp_activity / trimp_lthrhour
   hrss * 100
}

# Creates an activity_type_select_<keyword> checkboxgroup
create_activity_type_checkbox <- function(keyword) {
    checkboxGroupInput(sprintf("activity_type_select_%s", keyword),
                       "Activity Type",
                       ACTIVITY_TYPES,
                       selected="Run")
}

function(input, output, session) {
    
    output$activity_type_table <- renderUI({
        create_activity_type_checkbox("table")
    })
    
    output$activity_type_mileage <- renderUI({
        create_activity_type_checkbox("mileage")
    })

    output$activity_type_fitness <- renderUI({
        create_activity_type_checkbox("fitness")
    })
    
    output$activity_type_routes <- renderUI({
        create_activity_type_checkbox("routes")
    })
    
    output$activities <- renderDT({
        req(input$activity_type_select_table)
        types <- input$activity_type_select_table
        tbl(con, "activities") |>
            filter(activity_type %in% types) |>
            collect() |>
            mutate(start_time = as_datetime(start_time),
                   Date = as_date(start_time),
                   distance = round(distance, 1),
                   elevation = round(elevation),
                   duration = as.period(duration(duration, units="secs")),
                   dur_fmt = sprintf("%dh %dm %ds", hour(duration),
                                     minute(duration),
                                     second(duration)),
                   dur_fmt = gsub("0h ", "", dur_fmt)) |>
            arrange(desc(start_time)) |>
            
            select(
                Date,
                Type=activity_type,
                Name=name,
                `Distance (km)`=distance,
                Duration=dur_fmt,
                `Elevation (m)`=elevation
           )
    })
    
    mileage_df <- eventReactive(input$activity_type_select_mileage, {
        types <- input$activity_type_select_mileage
        dt <- tbl(con, "activities") |>
            filter(activity_type %in% types) |>
            select(start_time, distance) |>
            collect() |>
            setDT()
        dt[, date := as_date(as_datetime(start_time))]
        dt[, .(distance = sum(distance)), by=date]
    })
    
    hrss <- eventReactive(input$activity_type_select_fitness, {
        types <- input$activity_type_select_fitness
        fit_data <- tbl(con, "heartrate") |>
                        inner_join(tbl(con, "activities"), by="activity_id") |>
                        filter(activity_type %in% types) |>
                        collect() |>
                        setDT()
        fit_data[, time := as_datetime(time)]
        fit_data[, date := as_date(time)]
        
        # Calculate HRSS per activity
        # Assume only one athlete
        athlete_hr <- tbl(con, "athlete") |>
            select(maxHR, restHR, thresholdHR) |>
            head(1) |>
            collect()
        hrss <- fit_data[, .(hrss = calculate_hrss(heartrate,
                                                   time,
                                                   athlete_hr$maxHR,
                                                   athlete_hr$restHR,
                                                   athlete_hr$thresholdHR,
                                                   )), by=.(date, activity_id)]
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
        hrss
    })
    
    routes_df <- eventReactive(input$activity_type_select_routes, {
        types <- input$activity_type_select_routes
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
                                showlegend=F)
        }
        
        ggplotly(p, tooltip=c('colour', 'x', 'y')) |>
            layout(yaxis = list(hoverformat = '.5f'),
                   xaxis = list(title=""))
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
        FORM_TRANSLATION <- 40
        x_buffer_days <- 80
        
        # Original band definitions
        bands <- data.table(
            type=c('Freshness', 'Neutral', 'Optimal'),
            upper=c(25, 5, -10),
            lower=c(5, -10, -30),
            colour=c("#FC8D59", "#FFFFBF", "#99D594")
        ) 
        
        # Apply scaling to make Form maximizable
        bands[, c("upper", "lower") := .(FORM_TRANSLATION - upper,
                                         FORM_TRANSLATION - lower)]
        df[, Form := FORM_TRANSLATION - Form]
        
        df <- bands[df, 
              .(date, Form, Fitness, Fatigue, type, lower, upper), 
              on=.(lower > Form, upper < Form)]
        
         p1 <- plot_ly(x=~date, y=~Form, data=df,
                       type="scatter", mode="lines",
                       text=~type,
                       name="Form",
                       showlegend=FALSE)
         
         rectangles <- vector(mode="list", length=3)
         for (i in 1:nrow(bands)) {
             rectangles[[i]] <- list(
                 type="rect",
                 fillcolor=bands$colour[i],
                 line=list(color=bands$colour[i]),
                 opacity=0.2,
                 x0=min(df$date),
                 x1=max(df$date),
                 y0=bands$lower[i],
                 y1=bands$upper[i]
             )
         }
         p1 <- p1 |> 
                 layout(shapes=rectangles,
                        yaxis=list(title="Form",
                                   hoverformat=".0f"))
         
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

# TODO plotly
#  - Fitness plot
# TODO add badges (i.e. xKm in last week + month + year), or current training status
# TODO pbs?
# TODO clean up
