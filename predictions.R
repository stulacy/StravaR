# Race predictions tab: rendering only. The model itself is kept up to date by
# fitness_update.R, which the server runs at start-up and after each sync.
RACE_DISTANCES <- c("5k", "10k", "10 mile", "Half marathon", "Marathon")

# Minutes -> "m:ss" or "h:mm:ss"
format_race_time <- function(minutes) {
    s <- round(minutes * 60)
    h <- s %/% 3600; m <- (s %% 3600) %/% 60; sec <- s %% 60
    ifelse(h > 0, sprintf("%d:%02d:%02d", h, m, sec), sprintf("%d:%02d", m, sec))
}

# Plotly shows a clock-time axis when y is a time of day, so race times are
# plotted as seconds past midnight on an arbitrary day.
as_clock <- function(minutes) as.POSIXct("2000-01-01", tz = "UTC") + minutes * 60

# Tick positions for a race-time axis: a round step giving about six ticks,
# labelled with format_race_time() ("22:00", "3:40:00"). Plotly's own
# tickformat cannot drop the leading zero from hours ("%-H" breaks it).
race_time_ticks <- function(minutes) {
    lo <- min(minutes); hi <- max(minutes)
    steps <- c(0.5, 1, 2, 5, 10, 15, 20, 30, 60)
    step <- steps[which.max(steps >= (hi - lo) / 6)]
    v <- seq(floor(lo / step) * step, ceiling(hi / step) * step, by = step)
    list(tickvals = as_clock(v), ticktext = format_race_time(v))
}

missing_model_message <- function() {
    div(class = "pred-missing",
        p(strong("No fitness model installed."),
          sprintf("Expected %s.", normalizePath(BUNDLE_PATH, mustWork = FALSE))),
        p("Create one in the running-analysis repo with",
          code("Rscript update_fitness.R bundle v10 ../StravaR/model/fitness_model.rds"),
          "and restart the app.")
    )
}

render_predictions_now <- function(model) {
    if (is.null(model)) return(missing_model_message())
    now <- current_race_predictions(model)
    now <- now[match(RACE_DISTANCES, now$race), ]
    x <- fitness_model_status(model)
    tiles <- lapply(seq_len(nrow(now)), function(i) {
        div(class = "pred-tile",
            div(class = "pred-dist", now$race[i]),
            div(class = "pred-time", format_race_time(now$pred_q50[i])),
            div(class = "pred-range", sprintf("80%%: %s – %s",
                                              format_race_time(now$pred_q10[i]),
                                              format_race_time(now$pred_q90[i]))))
    })
    tagList(
        div(class = "pred-row", tiles),
        p(class = "pred-caption",
          sprintf("As of %s; latest run %s.",
                  format(x$as_of, "%d %b %Y"), format(x$last_run, "%d %b %Y")),
          if (x$ess < x$n_draws / 2)
              strong(" The model has drifted from its fit: refit it in running-analysis and install a new bundle.")
          else NULL)
    )
}

RANGE_BUTTONS <- list(buttons = list(
    list(count = 6, label = "6m", step = "month", stepmode = "backward"),
    list(count = 1, label = "1y", step = "year", stepmode = "backward"),
    list(count = 2, label = "2y", step = "year", stepmode = "backward"),
    list(step = "all", label = "All")))

# The Training chart opens on the last month, so it also gets a button for that
TRAINING_RANGE_BUTTONS <- list(buttons = c(
    list(list(count = 1, label = "1m", step = "month", stepmode = "backward")),
    RANGE_BUTTONS$buttons))

# The history is smoothed (every run up to the model's fit) followed by the
# filter's real-time estimates for runs since. The real-time part starts from
# the last smoothed point, where the two coincide, and is drawn dotted.
split_history <- function(h) {
    sm <- h[h$source == "smoothed", ]; rt <- h[h$source == "real time", ]
    if (nrow(rt)) rt <- bind_rows(tail(sm, 1), rt)
    list(smoothed = sm, real_time = rt)
}

plot_fitness_model <- function(model) {
    f <- model$st$history$fitness |>
        mutate(hover = sprintf("%s<br>Fitness %.1f (80%%: %.1f–%.1f)<br>Equivalent maximal 5k %s%s",
                               format(date, "%d %b %Y"), index, index_q10, index_q90,
                               format_race_time(max5k_min),
                               ifelse(source == "real time", "<br><i>real-time estimate</i>", "")))
    h <- split_history(f)
    p <- plot_ly(f, x = ~date) |>
        add_ribbons(ymin = ~index_q10, ymax = ~index_q90, name = "80% range",
                    line = list(width = 0), fillcolor = "rgba(0,166,90,0.2)",
                    hoverinfo = "skip") |>
        add_lines(data = h$smoothed, y = ~index, name = "Fitness",
                  line = list(color = "#00a65a", width = 2), text = ~hover, hoverinfo = "text")
    if (nrow(h$real_time))
        p <- p |> add_lines(data = h$real_time, y = ~index, name = "Fitness (real time)",
                            line = list(color = "#00a65a", width = 2, dash = "dot"),
                            text = ~hover, hoverinfo = "text")
    p |>
        layout(showlegend = FALSE,
               xaxis = list(title = "", rangeselector = RANGE_BUTTONS,
                            range = c(today() - years(2), today())),
               yaxis = list(title = "Fitness (100 = long-run average)"),
               shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                                  y0 = 100, y1 = 100,
                                  line = list(color = "grey", dash = "dot", width = 1))))
}

plot_race_times <- function(model, race_name) {
    r <- model$st$history$races |>
        filter(race == race_name) |>
        mutate(hover = sprintf("%s<br>Expected %s (80%%: %s – %s)%s",
                               format(date, "%d %b %Y"), format_race_time(q50),
                               format_race_time(q10), format_race_time(q90),
                               ifelse(source == "real time", "<br><i>real-time estimate</i>", "")))
    h <- split_history(r)
    a <- model$actual |>
        filter(race == race_name) |>
        mutate(hover = sprintf("%s<br>%s<br>Actual %s", format(date, "%d %b %Y"), name,
                               format_race_time(time_min)))
    ticks <- race_time_ticks(c(r$q10, r$q90, a$time_min))
    p <- plot_ly(r, x = ~date) |>
        add_ribbons(ymin = ~as_clock(q10), ymax = ~as_clock(q90), name = "80% range",
                    line = list(width = 0), fillcolor = "rgba(243,156,18,0.25)",
                    hoverinfo = "skip") |>
        add_lines(data = h$smoothed, y = ~as_clock(q50), name = "Expected",
                  line = list(color = "#e08e0b", width = 2), text = ~hover, hoverinfo = "text")
    if (nrow(h$real_time))
        p <- p |> add_lines(data = h$real_time, y = ~as_clock(q50), name = "Expected (real time)",
                            line = list(color = "#e08e0b", width = 2, dash = "dot"),
                            text = ~hover, hoverinfo = "text")
    if (nrow(a) > 0)
        p <- p |> add_markers(data = a, x = ~date, y = ~as_clock(time_min), name = "Races",
                              marker = list(color = "#333333", size = 8),
                              text = ~hover, hoverinfo = "text")
    p |> layout(showlegend = FALSE,
                xaxis = list(title = "", rangeselector = RANGE_BUTTONS,
                             range = c(today() - years(2), today())),
                yaxis = list(title = sprintf("%s time (faster is higher)", race_name),
                             type = "date", tickmode = "array", tickvals = ticks$tickvals,
                             ticktext = ticks$ticktext, autorange = "reversed"))
}
