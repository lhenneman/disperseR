#' create a set of directories to run disperseR
#'
#' \code{link_all_units}
#'
#' @description with `link_all_units()` users can link all air parcels to relevant spatial scales by month for specified units with combinations of years and months. `link_all_units()` reads in all the relevant HYSPLIT files (i.e., those that correspond to the provided units) produced by the `run_disperser_parallel()` function and saves them. Then it links them to relevant spatial scales.
#'
#'
#'
#' @param units.run A data.table with the following columns (and column classes): ID (character)a unique ID assigned to the particles emmission source. uID (character) same as ID. Latitude (numeric) and Longitude (numeric) the emmission source's latitude and longitude. year (integer) in which the emission events occur. Be mindful of the default value of the parameter stringsAsFactors when using the function data.table() to convert a data.frame to a data.table. It can take the data.table in disperseR::units() if `run_disperser_parallel()` used the information on unit locations in disperseR::units().
#'
#' @param start.date this argument is not necessary, but can be used if the user is interested in specifying a specific date to start the analysis with as opposed to using months. For example `start.date="2005-01-02"` for 2 January 2005. This argument are set to `NULL` by default and the function computes the start and the end dates using the `year.mons` provided.
#'
#' @param start.end this argument is not necessary, but can be used if the user is interested in specifying a specific date to end the analysis with as opposed to using months. For example `start.date="2005-01-02"` for 2 January 2005.This argument are set to `NULL` by default and the function computes the start and the end dates using the `year.mons` provided.
#'
#' @param link.to one of 'zips', 'counties', or 'grids' to denote spatial linkage scale. zips and counties are only for the USA. If another spatial unit of aggregation is required (census tracts, census block groups, etc), an sf object with POLYGONS can be provided to the `counties.` parameter. These sf object provided to the `counties.` parameter must have the following columns: statefp (character) the fips id of the state. countyfp (character) the fips id of the county. state_name (character) the name of the state. geoid (character) the id of the aggregation unit. name (character) the name of the aggregation unit. geometry (sfc_POLYGON or sfc_MULTIPOLYGON).
#'
#' @param year.mons these are the months for which we would like to do the linking. You can use the get_yearmon() function to create a vector that can be an input here.
#'
#' @param pbl_trim logical. Trim parcel locations under monthly PBL heights and take concentration under PBL layer?
#'
#' @param pbl.height monthly boundary layer heights. required if pbl_trim = TRUE
#'
#' @param crosswalk. `crosswalk. = crosswalk` by default but you can change it. See the vignette
#'
#' @param mc.cores `link_all_units()` enables the parallel run by default splitting different months on different cores, but you can make it serial just by setting the `mc.cores` argument to `1`.As mentioned before  `link_all_units()` enables the parallel run by default splitting different months on different cores, but you can make it serial just by setting the `mc.cores` argument to `1`.
#'
#' @param duration.run.hours `duration.run.hours = 240` by default which equals 10 days. 10 days is the maximum (approximately) that sulfur stays in the atmosphere before it deposits to the ground.
#'
#' @param overwrite `overwrite = FALSE` by default. Would you like to overwrite files that already exist?
#'
#' @param res.link Defines the grid resolution (in meters---defaults to 12000m = 12km) for linking. This is important for all link.to values, since parcel locations are first put on this grid before spatially allocating to other spatial domains.
#'
#' @param crop.usa Logical. For grid links, crop the output to only over the lower 48 US states? Ignored for county and ZIP code links.
#'
#' @param crop.usa Logical. For grid links, crop the output to only over the lower 48 US states? Ignored for county and ZIP code links.
#'
#' @return vector of months that you can loop over


#' @export link_all_units


link_all_units<- function(units.run,
                          link.to = 'zips',
                          mc.cores = detectCores(),
                          year.mons = NULL,
                          start.date = NULL,
                          end.date = NULL,
                          pbl_trim = TRUE,
                          pbl.height = NULL,
                          crosswalk. = NULL,
                          counties. = NULL,
                          duration.run.hours = 240,
                          res.link = 12000,
                          overwrite = FALSE,
                          pbl.trim = FALSE,
                          crop.usa = FALSE,
                          return.linked.data = TRUE) {

  if ((is.null(start.date) |
       is.null(end.date)) & is.null(year.mons)) {
    stop("Define either a start.date and an end.date OR a year.mons")
  }
  if ( length( link.to) != 1 | !(link.to %in% c( 'zips', 'counties', 'grids')) )
    stop( "link.to should be one of 'zips', 'counties', 'or 'grids'")
  if (link.to == 'zips' & is.null(crosswalk.))
    stop( "crosswalk. must be provided if link.to == 'zips'")
  if (link.to == 'counties' & is.null(counties.))
    stop( "counties. must be provided if link.to == 'counties'")
  if( pbl_trim & is.null( pbl.height))
    stop( "pbl.height must be provided if pbl_trim == TRUE")

  zips_link_parallel <- function(unit) {
    linked_zips <- parallel::mclapply(
      year.mons,
      disperseR::disperser_link_zips,
      unit = unit,
      pbl.height = pbl.height,
      crosswalk. = crosswalk.,
      duration.run.hours = duration.run.hours,
      overwrite = overwrite,
      res.link. = res.link,
      mc.cores = mc.cores,
      pbl. = pbl.trim,
      return.linked.data. = return.linked.data
    )

    linked_zips <- data.table::rbindlist(Filter(is.data.table, linked_zips))
    message(paste("processed unit", unit$ID, ""))

    linked_zips[, month := as( month, 'character')]
    return(linked_zips)
  }

  counties_link_parallel <- function(unit) {
    linked_counties <- parallel::mclapply(
      year.mons,
      disperseR::disperser_link_counties,
      unit = unit,
      pbl.height = pbl.height,
      counties = counties.,
      duration.run.hours = duration.run.hours,
      overwrite = overwrite,
      res.link. = res.link,
      mc.cores = mc.cores,
      pbl. = pbl.trim,
      return.linked.data. = return.linked.data
    )

    linked_counties <- data.table::rbindlist(Filter(is.data.table, linked_counties))
    message(paste("processed unit", unit$ID, ""))

    linked_counties[, month := as( month, 'character')]
    return(linked_counties)
  }

  grids_link_parallel <- function(unit) {
    linked_grids <- parallel::mclapply(
      year.mons,
      disperseR::disperser_link_grids,
      unit = unit,
      pbl.height = pbl.height,
      duration.run.hours = duration.run.hours,
      overwrite = overwrite,
      res.link. = res.link,
      mc.cores = mc.cores,
      pbl. = pbl.trim,
      crop.usa = crop.usa,
      return.linked.data. = return.linked.data
    )

    linked_grids <- data.table::rbindlist(Filter(is.data.table, linked_grids))
    message(paste("processed unit", unit$ID, ""))

    linked_grids[, month := as( month, 'character')]
    return(linked_grids)
  }

  units.run <- unique( units.run[, .( uID, ID)])

  if( link.to == 'zips')
    out <- units.run[, zips_link_parallel(.SD), by = seq_len(nrow(units.run))]
  if( link.to == 'counties')
    out <- units.run[, counties_link_parallel(.SD), by = seq_len(nrow(units.run))]
  if( link.to == 'grids')
    out <- units.run[, grids_link_parallel(.SD), by = seq_len(nrow(units.run))]

  out[, comb := paste("month: ", out[, month], " unitID :", out[, ID], sep = "")]
  out[, seq_len := NULL]
  return(out)
}
