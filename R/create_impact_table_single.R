#' @export create_impact_table_single
create_impact_table_single <- function(data.linked,
                                       data.units,
                                       link.to = 'zips',
                                       zcta.dataset = NULL,
                                       counties. = NULL,
                                       map.month,
                                       map.unitID,
                                       metric = 'N') {

  year.use <- as( substr( map.month, 1, 4), 'integer')
  datareduced <-
    data.linked[month == map.month & ID == map.unitID]
  dataunits <- data.units[ID == map.unitID & year == year.use]

  if( link.to == 'zips'){
    dataset_sf <- data.table(dataunits,
                             merge(
                               zcta.dataset,
                               datareduced,
                               by = c('ZIP'),
                               all.y = T
                             ))
    setnames(dataset_sf, metric, 'metric')
    myVector <-
      c(
        "ID",
        "month",
        "Latitude",
        "Longitude",
        "metric",
        "ZIP",
        "ZCTA",
        "CITY",
        "STATE",
        "TOTALESTIMATE",
        "MARGINOFERROR",
        "geometry"
      )

  } else if( link.to == 'counties'){
    dataset_sf <- data.table(dataunits,
                             merge(
                               counties.[, c( "statefp",
                                              "countyfp",
                                              "state_name",
                                              "name",
                                              "geoid",
                                              "geometry")],
                               datareduced,
                               by = c( "statefp",
                                       "countyfp",
                                       "state_name",
                                       "name",
                                       "geoid"),
                               all.y = T
                             ))
    setnames(dataset_sf, metric, 'metric')
    myVector <-
      c(
        "ID",
        "month",
        "Latitude",
        "Longitude",
        "metric",
        "statefp",
        "countyfp",
        "state_name",
        "name",
        "geometry"
      )
  } else if( link.to == 'grids'){
    dataset_r <- suppressWarnings( rasterFromXYZ( datareduced))
    dataset_sp <- as( dataset_r, 'SpatialPolygonsDataFrame')
    dataset_sf <- st_as_sf( dataset_sp)
    suppressWarnings(
      st_crs( dataset_sf$geometry) <-  "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
    )
    setnames(dataset_sf, metric, 'metric')
    dataset_sf$ID <- datareduced$ID
    dataset_sf$comb <- datareduced$comb
    myVector <- names( dataset_sf)
  }

  out <- data.table( dataset_sf[, myVector, with = FALSE])
  return(out)
}
