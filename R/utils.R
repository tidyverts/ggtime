convert_time <- function(x) {
  if (max(x) > 1e5) {
    structure(x, class = c("POSIXct", "POSIXt"))
  } else {
    structure(x, class = "Date")
  }
}
