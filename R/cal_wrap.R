calendar_wrap <- function(x, wrap_points = cal_gregorian("week")) {
  time <- convert_time(x)

  if(is.function(wrap_points)) {
    wrap_points <- wrap_points(range(time, na.rm = TRUE))
  }
  i <- as.integer(cut(time, wrap_points))
  x <- x - as.numeric(wrap_points)[i]
  x
}
