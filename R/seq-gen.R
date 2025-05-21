cal_gregorian <- function(period = "week", week_start = 1) {
  function(x) {
    if(any(!is.finite(x))) return(x)

    # Convert to date, should not needed later?
    out <- convert_time(x)

    # out <- as.Date(x, origin = "1970-01-01")
    # Only designed to work for period = "week" for now.
    # wday <- 1 + (as.numeric(out) + (6 - week_start))%%7

    out <- seq(
      from = timechange::time_floor(out[1], period, week_start),
      to = timechange::time_ceiling(out[2], period, week_start),
      by = period
    )
    out
    # vctrs::vec_cast(out, x)
  }
}
