library(tidyverse) 
library(lubridate) 
library(FITfileR)
library(pracma)
library(ggrepel)

df <- read_csv("data/raw/activities.csv")
comb_df <- df %>%
            filter(`Activity Type` == 'Run') %>%
            mutate(dt=parse_date_time(`Activity Date`, "%b %d, %Y %I:%M:%S %p"),
                   date=as_date(dt)) %>%
            select(date, distance=`Distance...7`) %>%
            filter(!is.na(date)) %>%
            group_by(date) %>%
            summarise(distance = sum(distance)) %>%
            ungroup()

###########################################################
# Plot yearly cumulative distance
###########################################################
# Hold points of interest that will highlight on plot
pois <- data.frame(
    year=c(2018, 2019, 2020, 2021, 2022),
    date=as_datetime(c("2000-10-14", "2000-07-21", "2000-08-09", "2000-09-24", "2000-10-16")),
    cumdist=c(230, 707, 1717, 2250, 1742)
)

# Add in text labels
label_offset <- 100
labels <- pois
labels$text <- c("First ten mile", "First half marathon", "First self-marathon", "Ada born",
                 "First marathon race")
labels$cumdist <- labels$cumdist + label_offset

comb_df %>%
    rbind(data.frame(date=as_date("2018-01-01"), distance=0)) %>%
    rbind(data.frame(date=as_date("2018-12-31"), distance=0)) %>%
    rbind(data.frame(date=as_date("2020-12-31"), distance=0)) %>%
    rbind(data.frame(date=as_date("2019-01-01"), distance=0)) %>%
    filter(date < as_date("2023-01-01")) %>%
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
        geom_point(data=pois, shape=8, size=3, colour='black') +
        geom_text(aes(label=text, colour=as.factor(year)), data=labels, show.legend = FALSE) +
        theme_bw() +
        scale_colour_manual("", values=RColorBrewer::brewer.pal(5, 'Set1')) +
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

###########################################################
# Plot 7-day moving average against time
###########################################################
# Will need to add in all possible days with 0 distance first
# Make entry for every day as need to run equation everyday as time isn't
# parameterised in it
library(zoo)
all_dates <- tibble(date=seq.Date(from=min(comb_df$date), to=max(comb_df$date), by=1))
comb_df_all <- merge(comb_df, all_dates, on='date', all.y=TRUE)
comb_df_all$distance <- ifelse(is.na(comb_df_all$distance), 0, comb_df_all$distance)
weeklyrate <- zoo::rollsum(zoo(comb_df_all$distance, comb_df_all$date), 7, fill=NA)
value <- zoo::rollmean(zoo(weeklyrate, comb_df_all$date), 7, fill=NA)

df <- as.data.frame(value)
df$date <- as_datetime(row.names(df))

pois3 <- data.frame(
    date=as_datetime(c("2018-10-14", "2019-07-21", "2020-02-16", "2020-08-09", "2021-09-24",
                       "2022-10-16")),
    value=c(100, 100, 100, 100, 100, 100)
)
label_offset <- 10
labels3 <- pois3
labels3$text <- c("Ten mile", "Half marathon", "Verona 10k", "Marathon", "Ada born", "Marathon race")
labels3$value <- labels3$value + label_offset

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
        geom_vline(aes(xintercept=date, colour=as.factor(date)), data=pois3, linetype=2, show.legend=FALSE) +
        geom_text(aes(label=text, colour=as.factor(date)), data=labels3, show.legend = FALSE) +
        scale_x_datetime(date_labels="%b %y",
                         date_breaks = "3 months") + 
        scale_alpha_manual(values=c(0, 0.2)) +
        guides(alpha="none") +
        labs(x="", y="7-day rolling average distance (km)")