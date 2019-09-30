

#' create a set of directories to run disperseR
#'
#' \code{get_yearmon}
#'
#' @description `get_yearmon()` outputs a vector of months that you can loop over. For Jan-March 2005, you just have to use the `get_yearmon()` `start.year = "2005"`, `start.month = "01"`, `end.year = "2005"`, and `end.month = "03"`.
#'
#'
#' @param start.year what year do you want your vector to start with? eg. start.year = "2005"
#' @param start.month what month of `start.year` eg. start.month = "01"
#' @param end.year what year do you want your vector to end with? This has to be a character eg. end.year = "2005"
#' @param end.month what month of that year? eg. end.month = "03"
#'
#' @return vector of months that you can loop over


#' @export get_yearmon

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
