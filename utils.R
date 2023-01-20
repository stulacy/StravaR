calculate_hrss <- function(hr, ts, HRmax, HRrest, LTHR, k=1.92) {
   hrr <- (hr - HRrest) / (HRmax - HRrest) 
   times_diff_min <- c(0, diff(ts)) / 60
   trimp_activity <- sum(times_diff_min * hrr * 0.64 * exp(k * hrr))
   hrr_lthr <- (LTHR - HRrest) / (HRmax - HRrest)
   trimp_lthrhour <- 60 * hrr_lthr * 0.64 * exp(k * hrr_lthr)
   hrss <- trimp_activity / trimp_lthrhour
   hrss * 100
}
