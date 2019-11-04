#' \code{create_impact_table_single}
#'
#'
#' @description `create_impact_table_single()` takes output from `link_all_units()` and creates a table of spatial impacts
#'
#'
#' @param data.linked data for plotting as output from disperseR::link_all_units()
#'
#' @param data.units information on unit locations as output from disperseR::units()
#'
#' @param link.to spatial scale for plotting. One of 'zips', 'counties', or 'grids' that should match original input to disperseR::link_all_units()
#'
#' @param zcta.dataset ZIP code/ZCTA sptial dataset as imported by disperseR::get_data() or subset thereof
#'
#' @param counties. US counties dataset as imported using USAboundaries::us_counties( ) or subset thereof
#'
#' @param metric plotting metric in `data.linked` input. Defaults to 'hyads', the default output of disperseR::calculate_exposure().
#'
#' @param map.month specific month to map in form YYYYMM if time.agg = 'month'. Can be created using disperseR::get_yearmon()
#'
#' @param map.unitID specific unit to map as string. Must match 'ID' column in data.units.
#'
#'
#' @return An sf data.table of impacts for a single unit and month suitable for plotting
#'
#'
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
