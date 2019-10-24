#' create a set of directories to run disperseR
#'
#' \code{link_all_units}
#'
#' @description with `link_all_units()` users can link all air parcels to ZIP codes by month for specified units. with combinations of years and months. `link_all_units_zip()` uses another function called `disperser_link_zip()` that runs in the background and reads in all the relevant files (i.e., those that correspond to the provided units) produced by the `run_disperser_parallel()` function and saves them. Then it links them to ZIP codes.
#'
#'
#'
#' @param link.to which spatial scale would you like to link parcel locations to? Current options are 'zips', 'counties', or 'grids'.
#' @param start.date this argument is not necessary, but can be used if the user is interested in specifying a specific date to start the analysis with as opposed to using months. For example `start.date="2005-01-02"` for 2 January 2005. This argument are set to `NULL` by default and the function computes the start and the end dates using the `year.mons` provided.
#'
#' @param start.end this argument is not necessary, but can be used if the user is interested in specifying a specific date to end the analysis with as opposed to using months. For example `start.date="2005-01-02"` for 2 January 2005.This argument are set to `NULL` by default and the function computes the start and the end dates using the `year.mons` provided.
#'
#' @param year.mons these are the months for which we would like to do the linking. You can use the get_yearmon() function to create a vector that can be an input here.
#' @param pbl.height `pbl.height = pblheight` by default but you can change it. See the vignette
#' @param crosswalk. `crosswalk. = crosswalk` by default but you can change it. See the vignette
#' @param counties. required if link.to = counties. Should be the result of the call USAboundaries::us_counties( ), subsetted as desired to a smaller list of counties.
#' @param mc.cores `link_all_units_zip()` enables the parallel run by default splitting different months on different cores, but you can make it serial just by setting the `mc.cores` argument to `1`.As mentioned before  `link_all_units_zip()` enables the parallel run by default splitting different months on different cores, but you can make it serial just by setting the `mc.cores` argument to `1`.
#' @duration.run.hours `duration.run.hours = 240` by default which equals 10 days. 10 days is the maximum (approximately) that sulfur stays in the atmosphere before it deposits to the ground.
#' overwrite `overwrite = FALSE` by default. Would you like to overwrite files that already exist?
#' @return vector of months that you can loop over



link_all_units<- function(units.run,
                          link.to = 'zips',
                          mc.cores = detectCores(),
                          year.mons = NULL,
                          start.date = NULL,
                          end.date = NULL,
                          pbl.height = pblheight,
                          crosswalk. = NULL,
                          counties. = NULL,
                          duration.run.hours = 240,
                          overwrite = FALSE) {


  if ((is.null(start.date) |
       is.null(end.date)) & is.null(year.mons)) {
    stop("Define either a start.date and an end.date OR a year.mons")
  }
  if ( length( link.to) != 1 | !(link.to %in% c( 'zips', 'counties', 'grids')) )
    stop( "link.to should be one of 'zips', 'counties', 'or 'grids'")
  if (link.to == 'zips' & is.null(crosswalk.))
    stop( "crosswalk. must be provided if link.to == 'zips'")
  if (link.to == 'counties' & is.null(crosswalk.))
    stop( "counties. must be provided if link.to == 'counties'")

  zip_link_parallel <- function(unit) {
    linked_zips <- parallel::mclapply(
      yearmons,
      disperseR::disperser_link_zip,
      unit = unit,
      pbl.height = pbl.height,
      crosswalk. = crosswalk.,
      duration.run.hours = duration.run.hours,
      overwrite = overwrite,
      mc.cores = mc.cores
    )

    message(paste("processed unit", unit, ""))

    linked_zips <- data.table::rbindlist(Filter(is.data.table, linked_zips))
    return(linked_zips)
  }

  county_link_parallel <- function(unit) {
    linked_zips <- parallel::mclapply(
      yearmons,
      disperseR::disperser_link_county,
      unit = unit,
      pbl.height = pbl.height,
      counties. = counties.,
      duration.run.hours = duration.run.hours,
      overwrite = overwrite,
      mc.cores = mc.cores
    )

    message(paste("processed unit", unit, ""))

    linked_zips <- data.table::rbindlist(Filter(is.data.table, linked_zips))
    return(linked_zips)
  }

  grid_link_parallel <- function(unit) {
    linked_zips <- parallel::mclapply(
      yearmons,
      disperseR::disperser_link_grid,
      unit = unit,
      pbl.height = pbl.height,
      duration.run.hours = duration.run.hours,
      overwrite = overwrite,
      mc.cores = mc.cores
    )

    message(paste("processed unit", unit, ""))

    linked_zips <- data.table::rbindlist(Filter(is.data.table, linked_zips))
    return(linked_zips)
  }

  if( link.to == 'zips')
    out <- unitsrun[, zip_link_parallel(.SD), by = seq_len(nrow(unitsrun))]
  if( link.to == 'counties')
    out <- unitsrun[, county_link_parallel(.SD), by = seq_len(nrow(unitsrun))]
  if( link.to == 'grids')
    out <- unitsrun[, grid_link_parallel(.SD), by = seq_len(nrow(unitsrun))]

    out <- out[, comb := paste("month: ", out[, month], " unitID :", out[, unitID], sep = "")]
  return(out)
}
