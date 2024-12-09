convert_time <- function(x) {
  if(!is.numeric(x)) return(x)

  if(all(is.na(x))) return(x)

  if (max(x) > 1e5) {
    structure(x, class = c("POSIXct", "POSIXt"))
  } else {
    structure(x, class = "Date")
  }
}
