# =============================================================================
# fitness_update.R — keep the fitness model current as runs are synced
# -----------------------------------------------------------------------------
# model/fitness_model.rds   the model bundle, made offline after each refit by
#                           `Rscript update_fitness.R bundle` in running-analysis
# model/fitness_state.rds   this app's filter state and dashboard history
#
# update_fitness_model() finds runs the filter has not seen yet, turns each
# into the model's observations, and steps the filter (fitness_filter.R). It
# runs when the app starts and after every sync, on the app's own database
# connection.
#
# New runs must be summarised exactly as the runs the model was fitted on, so
# every constant here -- heart-rate reserve, HRSS threshold, the best-effort
# distance grid, the GPS spike threshold, known sensor faults -- comes from
# the bundle, not from this app's athlete settings. The functions mirror
# running-analysis/R/00_common.R and R/41_online_ingest.R.
# =============================================================================

MODEL_DIR   <- "model"
BUNDLE_PATH <- file.path(MODEL_DIR, "fitness_model.rds")
STATE_PATH  <- file.path(MODEL_DIR, "fitness_state.rds")

# ── Run summaries ────────────────────────────────────────────────────────────

fm_hrss <- function(hr, ts_sec, hr_max, hr_rest, lthr, k = 1.92) {
    hrr <- pmax((hr - hr_rest) / (hr_max - hr_rest), 0)
    dt_min <- c(0, diff(ts_sec)) / 60
    dt_min[dt_min > 5] <- 0                        # ignore long recording gaps
    trimp <- sum(dt_min * hrr * 0.64 * exp(k * hrr))
    hrr_lthr <- (lthr - hr_rest) / (hr_max - hr_rest)
    100 * trimp / (60 * hrr_lthr * 0.64 * exp(k * hrr_lthr))
}

# Known sensor faults (the bundle's data_flags): add `value` bpm to samples
# whose date falls in a flagged range.
fm_apply_hr_flags <- function(hr, flags) {
    f <- flags[flags$issue == "hr_offset", ]
    if (!nrow(f) || !nrow(hr)) return(hr)
    d <- as.Date(hr$time)
    for (k in seq_len(nrow(f))) {
        hit <- d >= f$start_date[k] & d <= f$end_date[k]
        if (!is.na(f$activity_id[k])) hit <- hit & as.character(hr$activity_id) == f$activity_id[k]
        hr$heartrate[hit] <- hr$heartrate[hit] + f$value[k]
    }
    hr
}

# ── GPS track ────────────────────────────────────────────────────────────────

fm_haversine <- function(lat1, lon1, lat2, lon2) {
    r <- pi / 180
    a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
    2 * 6371000 * asin(sqrt(pmin(a, 1)))
}

# A point implying an impossible speed both into and out of it is a bad fix.
fm_gps_spikes <- function(t, lat, lon, max_speed) {
    keep <- !is.na(lat) & !is.na(lon)
    repeat {
        i <- which(keep); n <- length(i)
        if (n < 3) break
        v <- fm_haversine(lat[i[-n]], lon[i[-n]], lat[i[-1]], lon[i[-1]]) / pmax(diff(t[i]), 1)
        vin <- c(FALSE, v > max_speed); vout <- c(v > max_speed, FALSE)
        spike <- (vin & vout) | (vin & seq_len(n) == n) | (vout & seq_len(n) == 1)
        if (!any(spike)) break
        keep[i[spike]] <- FALSE
    }
    !keep
}

# Cumulative time and distance, GPS distance rescaled to the device total
fm_track <- function(con, id, const) {
    pts <- dbGetQuery(con, "
        SELECT l.time, l.lat, l.lon, a.distance * 1000 AS device_m
        FROM location l JOIN activities a USING (activity_id)
        WHERE l.activity_id = ? ORDER BY l.time", params = list(id))
    if (nrow(pts) < 2) return(NULL)
    t <- as.numeric(pts$time)
    drop <- fm_gps_spikes(t, pts$lat, pts$lon, const$max_gps_speed)
    pts <- pts[!drop, ]; t <- t[!drop]
    if (length(t) < 2) return(NULL)
    step <- c(0, fm_haversine(head(pts$lat, -1), head(pts$lon, -1), pts$lat[-1], pts$lon[-1]))
    cd <- cumsum(step); tot <- max(cd)
    sf <- if (tot > 100 && !is.na(pts$device_m[1])) pts$device_m[1] / tot else 1
    tibble(time = pts$time, cum_time = t - t[1], cum_distance = cd * min(max(sf, 0.7), 1.3))
}

# Fastest window covering D_m, allowed to end tol_m past the last point
fm_best_effort <- function(cd, ct, D_m, tol_m) {
    na <- list(t = NA_real_, t0 = NA_real_, t1 = NA_real_)
    if (length(cd) < 5 || max(cd) + tol_m < D_m) return(na)
    tgt <- cd + D_m
    ok <- which(tgt <= max(cd) + tol_m)
    if (!length(ok)) return(na)
    t_end <- approx(cd, ct, xout = tgt[ok], ties = "ordered", rule = 2)$y
    dur <- t_end - ct[ok]
    j <- which.min(dur)
    if (!length(j) || !is.finite(dur[j])) return(na)
    list(t = dur[j], t0 = ct[ok][j], t1 = t_end[j])
}

# ── Races ────────────────────────────────────────────────────────────────────

FM_RACE_PATTERN <- regex(paste0(
    "parkrun|park run|\\brace\\b|time.?trial|\\btt\\b|",
    "half.?marathon|\\bmarathon\\b|\\bpb\\b|\\bpr\\b|",
    "championship|league|\\b\\d+ ?k(m)? ?(pb|tt|-)|",
    "york 10k|castle howard|vienna|verona|divonne"), ignore_case = TRUE)
FM_NOT_RACE_PATTERN <- regex(paste0(
    "pre.?race|shakeout|leg stretch|warm.?up|substitute|",
    "with ada|with jenny|with ryan|pushchair|buggy|easy|recovery|jubilee"), ignore_case = TRUE)
FM_MP_PATTERN <- regex("@ ?mp|marathon pace|@ ?race pace|tempo", ignore_case = TRUE)
FM_ENCUMBERED_PATTERN <- regex(paste0(
    "pushchair|buggy|pram|stroller|with ada|with robin|",
    "ada run|ada\\u2019s|nursery|nap time"), ignore_case = TRUE)
FM_MIN_RACE_EFFORT <- 0.78

fm_embedded_time <- function(nm) {
    m <- str_match(nm, "(?<!\\d)(?:(\\d{1,2}):)?([0-5]?\\d):([0-5]\\d)(?!\\d)")
    h <- suppressWarnings(as.numeric(m[, 2])); h[is.na(h)] <- 0
    out <- h * 60 + suppressWarnings(as.numeric(m[, 3])) + suppressWarnings(as.numeric(m[, 4])) / 60
    out
}

fm_snap_to_standard <- function(km) {
    std <- c(5, 10, 21.0975, 42.195)
    i <- which.min(abs(log(km) - log(std)))
    if (abs(log(km) - log(std[i])) < log(1.12)) std[i] else NA_real_
}

# ── Temperature at the start of the run ──────────────────────────────────────
# The archive API lags a few days, so recent runs fall back to the forecast
# API's recent past, and failing both, to the reference (no temperature effect).
fm_run_temp <- function(lat, lon, date, hour) {
    get <- function(base) {
        url <- sprintf(paste0("%s?latitude=%.4f&longitude=%.4f&start_date=%s&end_date=%s",
                              "&hourly=temperature_2m&timezone=Europe%%2FLondon"),
                       base, lat, lon, format(date), format(date))
        js <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
        if (is.null(js$hourly)) return(NA_real_)
        js$hourly$temperature_2m[match(hour, as.integer(substr(js$hourly$time, 12, 13)))]
    }
    tc <- get("https://archive-api.open-meteo.com/v1/archive")
    if (is.na(tc)) tc <- get("https://api.open-meteo.com/v1/forecast")
    tc
}

# ── One activity -> the filter's inputs ──────────────────────────────────────

fm_ingest <- function(con, id, const) {
    a <- dbGetQuery(con, "SELECT activity_id, name, start_time, distance AS km, elevation
                          FROM activities WHERE activity_id = ?", params = list(id))
    date <- as.Date(a$start_time); hour <- as.integer(format(a$start_time, "%H"))
    nm <- ifelse(is.na(a$name), "", a$name)
    hr <- dbGetQuery(con, "SELECT activity_id, time, heartrate FROM heartrate
                           WHERE activity_id = ? ORDER BY time", params = list(id)) |>
        fm_apply_hr_flags(const$data_flags)
    reserve <- const$hr_max - const$hr_rest
    avg_hr <- if (nrow(hr)) mean(hr$heartrate, na.rm = TRUE) else NA_real_
    hrss <- if (nrow(hr)) fm_hrss(hr$heartrate, as.numeric(hr$time - min(hr$time), units = "secs"),
                                  const$hr_max, const$hr_rest, const$lthr) else NA_real_
    effort_hr <- (avg_hr - const$hr_rest) / reserve

    nominal <- fm_snap_to_standard(a$km)
    is_race <- str_detect(nm, FM_RACE_PATTERN) && !str_detect(nm, FM_NOT_RACE_PATTERN) &&
               !str_detect(nm, FM_MP_PATTERN) &&
               (ifelse(is.na(effort_hr), 0, effort_hr) >= FM_MIN_RACE_EFFORT ||
                !is.na(fm_embedded_time(nm)) || is.na(effort_hr)) && !is.na(nominal)
    enc <- str_detect(nm, FM_ENCUMBERED_PATTERN)
    run <- list(activity_id = as.character(id), name = nm, date = date,
                day = as.numeric(date - const$date_min),
                load_std = if (is.na(hrss)) 0 else hrss / const$load_mean, is_race = is_race)

    tr <- fm_track(con, id, const)
    if (is.null(tr) || nrow(tr) < 5) return(list(run = run, obs = NULL))
    s2 <- tr[order(tr$cum_time), ]
    s2 <- s2[s2$cum_distance == cummax(s2$cum_distance), ]
    be <- map(const$best_effort_dists,
              ~fm_best_effort(s2$cum_distance, s2$cum_time, .x * 1000, const$be_tol_m))
    be <- tibble(be_dist_km = const$best_effort_dists, be_time_s = map_dbl(be, "t"),
                 be_t0 = map_dbl(be, "t0"), be_t1 = map_dbl(be, "t1")) |>
        filter(!is.na(be_time_s), be_time_s > 0) |>
        mutate(be_speed_kmh = be_dist_km / (be_time_s / 3600))
    hrt <- inner_join(tr[, c("time", "cum_time")], hr, by = "time") |>
        filter(!is.na(heartrate)) |> arrange(cum_time)
    be$seg_hr <- map2_dbl(be$be_t0, be$be_t1, function(t0, t1) {
        k <- hrt$cum_time >= t0 & hrt$cum_time <= t1
        if (!any(k)) NA_real_ else mean(hrt$heartrate[k], na.rm = TRUE)
    })

    tc <- fm_run_temp(const$lat, const$lon, date, hour)
    if (is.na(tc)) tc <- const$temp_ref
    elev_per_km <- ifelse(is.na(a$elevation), 0, a$elevation) / max(a$km, 0.5)

    obs <- be |>
        filter(be_time_s > 60, be_dist_km <= a$km * 1.02, be_speed_kmh > 4, be_speed_kmh < 25) |>
        mutate(activity_id = as.character(id),
               effort_seg = if_else(!is.na(seg_hr), (seg_hr - const$hr_rest) / reserve, effort_hr),
               has_hr = as.integer(!is.na(effort_seg)),
               effort = ifelse(is.na(effort_seg), const$effort_ref, effort_seg),
               log_speed = log(be_speed_kmh), log_dist_c = log(be_dist_km / 5),
               log_frac = log(pmin(be_dist_km / a$km, 1)),
               temp_ctr = tc - const$temp_ref, elev_ctr = elev_per_km - const$elev_ref,
               encumbered = as.numeric(enc),
               # a race observation is the best effort at the race's own distance
               is_race = as.integer(is_race & !is.na(nominal) & abs(be_dist_km - nominal) < 1e-6),
               dist_idx = match(be_dist_km, const$dgrid)) |>
        filter(!is.na(dist_idx))
    list(run = run, obs = obs)
}

# ── The model: load, update, predict ─────────────────────────────────────────

# NULL when no bundle is installed. A bundle newer than the saved state (a
# refit) replaces the state, since its parameters supersede the old ones.
load_fitness_model <- function() {
    if (!file.exists(BUNDLE_PATH)) return(NULL)
    b <- readRDS(BUNDLE_PATH)
    st <- if (file.exists(STATE_PATH)) readRDS(STATE_PATH) else NULL
    if (is.null(st) || !identical(st$bundle_created_at, b$created_at))
        st <- list(bundle_created_at = b$created_at, filter = b$state, history = b$history)
    list(par = b$par, st = st, actual = b$actual, tag = b$tag, fitted_to = b$fitted_to)
}

# Dashboard rows for the filter's current estimate (real time, not smoothed)
fm_history_rows <- function(model, date) {
    par <- model$par; st <- model$st$filter
    f <- online_fitness(st, par, st$t)
    ref <- par$const$fitness_ref
    fit <- tibble(date = date, index = 100 * exp(f$phi - ref),
                  index_q10 = 100 * exp(f$phi - 1.2816 * f$phi_sd - ref),
                  index_q90 = 100 * exp(f$phi + 1.2816 * f$phi_sd - ref),
                  max5k_min = f$t5_min, source = "real time")
    d <- par$const$race_distances
    pr <- withr::with_seed(1, online_predict_race(st, par, unname(d), t = st$t))
    races <- tibble(date = date, race = names(d), distance_km = unname(d),
                    q10 = pr$pred_q10, q50 = pr$pred_q50, q90 = pr$pred_q90, source = "real time")
    list(fitness = fit, races = races)
}

# Steps the filter through every run it has not seen, in time order, and saves
# the state. Returns the model and how many runs were processed or skipped.
update_fitness_model <- function(model, con, progress = NULL) {
    if (is.null(model)) return(list(model = NULL, processed = 0, skipped = 0))
    par <- model$par; st <- model$st
    acts <- dbGetQuery(con, "SELECT activity_id, start_time FROM activities
                             WHERE activity_type = 'Run' ORDER BY start_time")
    new <- acts[!as.character(acts$activity_id) %in% st$filter$seen, ]
    processed <- 0; skipped <- 0
    for (i in seq_len(nrow(new))) {
        if (!is.null(progress)) progress(i, nrow(new))
        ing <- fm_ingest(con, new$activity_id[i], par$const)
        if (ing$run$day < floor(st$filter$t)) {
            # The filter cannot go back in time; a late-synced old run waits
            # for the next refit, which will see it.
            skipped <- skipped + 1
            st$filter$seen <- c(st$filter$seen, ing$run$activity_id)
            next
        }
        st$filter <- online_step(st$filter, par, ing$run, ing$obs, reweight = TRUE)
        model$st <- st
        rows <- fm_history_rows(model, ing$run$date)
        # one real-time row per day: a later run the same day replaces it
        keep <- function(h) h[!(h$date == ing$run$date & h$source == "real time"), ]
        st$history$fitness <- bind_rows(keep(st$history$fitness), rows$fitness)
        st$history$races <- bind_rows(keep(st$history$races), rows$races)
        processed <- processed + 1
    }
    model$st <- st
    if (processed + skipped > 0) saveRDS(st, STATE_PATH)
    list(model = model, processed = processed, skipped = skipped)
}

# Predictions for today (or `date`), projected forward from the last run
current_race_predictions <- function(model, date = Sys.Date()) {
    par <- model$par; st <- model$st$filter
    t <- max(as.numeric(date - par$const$date_min), st$t)
    d <- par$const$race_distances
    withr::with_seed(1, online_predict_race(st, par, unname(d), t = t)) |>
        mutate(race = names(d), .before = 1)
}

fitness_model_status <- function(model, date = Sys.Date()) {
    st <- model$st$filter
    list(as_of = date, last_run = model$par$const$date_min + floor(st$t),
         ess = weights_ess(st), n_draws = model$par$S, tag = model$tag,
         fitted_to = model$fitted_to)
}
