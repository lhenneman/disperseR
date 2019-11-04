#' @export read_ziplinks_subfun
read_ziplinks_subfun <- function(i, files) {
  d <- fread(files[i], drop = 'V1', keepLeadingZeros = TRUE)
  d[, `:=` (ZIP = as(ZIP, 'character'),
            month = as(month, 'character'))]
  d <- d[N > 0]
  return(d)
}

#' @export read_gridlinks_subfun
read_gridlinks_subfun <- function(i, files) {
  d <- fread( files[i], drop = 'V1', keepLeadingZeros = TRUE)
  d[, month := as( month, 'character')]
  d <- d[N > 0]
  return(d)
}

#' @export read_countylinks_subfun
read_countylinks_subfun <- function(i, files) {
  d <- fread( files[i], drop = 'V1', keepLeadingZeros = TRUE)
  d[, month := as( month, 'character')]
  d <- d[ N > 0]
  return(d)
}
