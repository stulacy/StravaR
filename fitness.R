library(tidyverse)
library(lubridate)
library(data.table)
library(RColorBrewer)

fit_dir <- "data/clean/from_fit/"
fit_fns <- paste0(fit_dir, "/", list.files(fit_dir))
fit_cols <- c("record.timestamp[s]",
              "record.heart_rate[bpm]")
fit_data <- rbindlist(lapply(setNames(fit_fns, fit_fns), fread, select=fit_cols, fill=TRUE), fill=TRUE, idcol="fn")
setnames(fit_data, new=c('fn', 'timestamp', 'hr'))

fit_epoch_offset <- 631065600 
fit_data[, timestamp := as_datetime(timestamp + fit_epoch_offset)]

# Remove timepoints where don't have HR
fit_data <- fit_data[ !is.na(hr) ]
fit_data[, date := as_date(min(timestamp)), by=fn ]

# Restrict to running dates, NB: assuming that never have day
# where both run and cycle
running_dates <- read_csv("data/raw/activities.csv") |>
            filter(`Activity Type` == 'Run') |>
            mutate(timestamp=parse_date_time(`Activity Date`, "%b %d, %Y %I:%M:%S %p"),
                   is_run=TRUE) |>
            select(timestamp, is_run) |>
            setDT()
# Unsure how to do this group by filter in data.table syntax 
fit_data <- running_dates[fit_data, on='timestamp'] |>
    group_by(fn) |>
    filter(sum(!is.na(is_run)) > 0) |>
    ungroup() |>
    setDT()

# Calculate activity HRSS
calculate_hrss <- function(hr, ts, HRmax=190, HRrest=55, LTHR=170, k=1.92) {
   hrr <- (hr - HRrest) / (HRmax - HRrest) 
   times_diff_min <- c(0, diff(ts)) / 60
   trimp_activity <- sum(times_diff_min * hrr * 0.64 * exp(k * hrr))
   hrr_lthr <- (LTHR - HRrest) / (HRmax - HRrest)
   trimp_lthrhour <- 60 * hrr_lthr * 0.64 * exp(k * hrr_lthr)
   hrss <- trimp_activity / trimp_lthrhour
   hrss * 100
}

# Calculate HRSS per activity
hrss <- fit_data[, .(hrss = calculate_hrss(hr, timestamp)), by=list(fn, date)]
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

x_buffer_days <- 80
bands <- data.frame(
    type=c('Freshness', 'Neutral', 'Optimal'),
    upper=c(25, 5, -10),
    lower=c(5, -10, -30)
)
bands_plot <- bands[rep(1:nrow(bands), nrow(hrss)+x_buffer_days), ]
bands_plot$date <- rep(c(hrss$date, 
                         seq.Date(max(hrss$date)+1,
                                  max(hrss$date)+x_buffer_days,
                                  by=1)), 
                       each=nrow(bands))
bands_plot$name <- factor('Form',
                          levels=c('Form', 'Fitness', 'Fatigue'))

df_long <- hrss %>%
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
