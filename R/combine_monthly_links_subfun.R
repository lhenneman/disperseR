read_ziplinks_subfun <- function(i, files) {
  names.use <- c('ZIP', 'hyads', 'month', 'uID')
  d <- fread(files[i], drop = 'V1', col.names = names.use)
  d[, `:=` (ZIP = as(d$ZIP, 'character'))]
  d <- d[hyads > 0]
  return(d)
}

read_gridlinks_subfun <- function(i, files) {
  d <- fread( files[i], drop = 'V1')
  setnames( d, 'hyspdisp', names(files)[i])
  r <- rasterFromXYZ( d)
  return(r)
}

read_countylinks_subfun <- function(i, files) {
  d <- fread( files[i], drop = 'V1')
  d[, `:=` (uID = names(files)[i] )]
  d <- d[ N > 0]
  return(d)
}
