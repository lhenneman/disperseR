get_yearmon <- function(start.year = NULL,
  start.month = NULL,
  end.year = NULL,
  end.month = NULL) {

  if (!is.character(start.year) |
      !is.character(start.month) |
      !is.character(end.year) |
      !is.character(end.month)) {
    stop("start.month, start.year, end.month, end.year should all be provided as characters. Please refer to the main vignette for an example")
  }

  startdate <- paste0(start.year, "/", start.month, "/01")
  enddate <- paste0(end.year, "/", end.month, "/01")
  vector <- as.character(seq(as.Date(startdate), as.Date(enddate), "months"))

  getstring <- function(date) {
    year <- as.numeric(substr(date, 1, 4))
    month <- as.numeric(substr(date, 6, 7))
    out <- paste0(year, month)
    return(out)
  }

  return(unlist(lapply(FUN = getstring, vector)))
}
