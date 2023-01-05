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

con <- dbConnect(SQLite(), "data/strava.db")
ACTIVITY_TYPES <- tbl(con, "activity_types") |>
                    pull(activity_type)
YEARS <- seq(from=2018, to=2022, by=1)
N_YEARS <- length(YEARS)

calculate_hrss <- function(hr, ts, HRmax=190, HRrest=55, LTHR=170, k=1.92) {
   hrr <- (hr - HRrest) / (HRmax - HRrest) 
   times_diff_min <- c(0, diff(ts)) / 60
   trimp_activity <- sum(times_diff_min * hrr * 0.64 * exp(k * hrr))
   hrr_lthr <- (LTHR - HRrest) / (HRmax - HRrest)
   trimp_lthrhour <- 60 * hrr_lthr * 0.64 * exp(k * hrr_lthr)
   hrss <- trimp_activity / trimp_lthrhour
   hrss * 100
}

function(input, output, session) {
    
    output$activity_type_table <- renderUI({
        checkboxGroupInput("activity_table_type_select",
                           "Activity Type",
                           ACTIVITY_TYPES,
                           selected="Run")
    })
    
    output$activity_type_mileage <- renderUI({
        checkboxGroupInput("activity_mileage_type_select",
                           "Activity Type",
                           ACTIVITY_TYPES,
                           selected="Run")
    })

    output$activity_type_fitness <- renderUI({
        checkboxGroupInput("activity_fitness_type_select",
                           "Activity Type",
                           ACTIVITY_TYPES,
                           selected="Run")
    })
    
    output$activities <- renderDT({
        req(input$activity_table_type_select)
        types <- input$activity_table_type_select
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
    
    mileage_df <- reactive({
        req(input$activity_mileage_type_select)
        types <- input$activity_mileage_type_select
        tbl(con, "activities") |>
            filter(activity_type %in% types) |>
            collect() |>
            mutate(time = as_datetime(start_time),
                   date = as_date(time)) |>
            group_by(date) |>
            summarise(distance = sum(distance)) |>
            ungroup()
    })
    
    hrss <- reactive({
        req(input$activity_fitness_type_select)
        types <- input$activity_fitness_type_select
        fit_data <- tbl(con, "heartrate") |>
                        inner_join(tbl(con, "activities"), by="activity_id") |>
                        filter(activity_type %in% types) |>
                        collect() |>
                        mutate(time = as_datetime(time),
                               date = as_date(time)) |>
                        setDT()
        # Calculate HRSS per activity
        # TODO use athlete's stats
        hrss <- fit_data[, .(hrss = calculate_hrss(heartrate, time)), by=.(date, activity_id)]
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
    
    output$mileage_cumulative <- renderPlot({
        req(mileage_df())
        mileage_df() |>
            mutate(year = year(date),
                   date = as_datetime(sprintf("2000-%d-%d", month(date), day(date)))) %>%
            group_by(year) %>%
            arrange(date) %>%
            mutate(cumdist = cumsum(distance),
                   label = ifelse(date == max(date), as.character(year), NA_character_)) %>%
            ungroup() |>
            ggplot(aes(x=date, y=cumdist)) +
                geom_line(aes(colour=as.factor(year))) +
                geom_text_repel(aes(label = label,
                                     colour=as.factor(year)),
                                 nudge_x = 60*60*24*10,
                                 nudge_y=0,
                                 na.rm = TRUE) +
                theme_bw() +
                scale_colour_manual("", values=RColorBrewer::brewer.pal(N_YEARS, 'Set1')) +
                guides(shape="none", colour="none") +
                scale_x_datetime(date_labels="%d %b") +
                labs(x="", y="Cumulative distance (km)") +
                theme(legend.position="bottom",
                      axis.text = element_text(size=10),
                      legend.text = element_text(size=12),
                      axis.title = element_text(size=14),
                      plot.title = element_text(size=16),
                      panel.grid.minor.x = element_blank()
                )
    })
    
    output$mileage_weekly <- renderPlot({
        req(mileage_df)
        df <- mileage_df()
        all_dates <- tibble(date=seq.Date(from=min(df$date), to=max(df$date), by=1))
        df <- merge(df, all_dates, on='date', all.y=TRUE)
        df$distance <- ifelse(is.na(df$distance), 0, df$distance)
        weeklyrate <- zoo::rollsum(zoo(df$distance, df$date), 7, fill=NA)
        value <- zoo::rollmean(zoo(weeklyrate, df$date), 7, fill=NA)
        
        df <- as.data.frame(value)
        df$date <- as_datetime(row.names(df))
        
        years <- unique(year(df$date))
        start <- as_datetime(sapply(years, function(x) sprintf("%d-01-01", x)))
        end <- as_datetime(sapply(years, function(x) sprintf("%d-12-31", x)))
        ribbons <- tibble(year=years, start=start, end=end,
                          ymin=0, ymax=125) |>
            mutate(shaded = year %% 2 == 0)
        # Replace the first date with the first actual date I ran
        ribbons$start[1] <- min(df$date)
        
        df %>%
            ggplot(aes(x=date, y=value)) +
                geom_line(na.rm=T) +
                theme_bw() +
                geom_rect(aes(x=start, y=ymax, xmin=start, xmax=end, ymin=ymin, ymax=ymax,
                              alpha=as.factor(shaded)),
                          data=ribbons) +
                scale_x_datetime(date_labels="%b %y",
                                 date_breaks = "3 months") + 
                scale_alpha_manual(values=c(0, 0.2)) +
                guides(alpha="none") +
                labs(x="", y="7-day rolling average distance (km)")
        
    })
    
    output$training <- renderPlot({
        req(hrss())
        df <- hrss()
        x_buffer_days <- 80
        bands <- data.frame(
            type=c('Freshness', 'Neutral', 'Optimal'),
            upper=c(25, 5, -10),
            lower=c(5, -10, -30)
        )
        bands_plot <- bands[rep(1:nrow(bands), nrow(df)+x_buffer_days), ]
        bands_plot$date <- rep(c(df$date, 
                                 seq.Date(max(df$date)+1,
                                          max(df$date)+x_buffer_days,
                                          by=1)), 
                               each=nrow(bands))
        bands_plot$name <- factor('Form',
                                  levels=c('Form', 'Fitness', 'Fatigue'))
        
        df_long <- df %>%
            pivot_longer(-(c(date, hrss))) |>
            mutate(name = factor(name,
                                 levels=c('Form', 'Fitness', 'Fatigue')))
        df_long |>
            ggplot() +
                geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill=type), 
                            data=bands_plot, alpha=0.3) +
                geom_label(aes(x=date, y=mid, label=type, colour=type), 
                          data=bands_plot |> 
                              group_by(type) |>
                              summarise(mid = lower + (upper - lower)/2,
                                        date=max(date) - x_buffer_days /2) |> 
                              ungroup() |> 
                              distinct(type, mid, date) |>
                              mutate(name = factor('Form',
                                                   levels=c('Form', 'Fitness', 'Fatigue')))) +
                geom_line(aes(x=date, y=value)) + 
                theme_bw() +
                facet_wrap(~name, ncol=1,
                           scales="free_y") +
                labs(x="", y="") +
                scale_x_date(date_breaks = "2 month",
                             date_labels = "%b %Y") +
                guides(fill="none", colour="none") +
                scale_fill_viridis_d("") +
                scale_colour_viridis_d("") +
                theme(legend.position = "bottom")
        
    })
}

# TODO add route plots
# TODO add route gifs
# TODO plotly
# TODO icons
# TODO clean up
