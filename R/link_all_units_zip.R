#' create a set of directories to run disperseR
#'
#' \code{link_all_units_zip}
#'
#' @description with `link_all_units_zip()` users can link all air parcels to ZIP codes by month for specified units. with combinations of years and months. `link_all_units_zip()` uses another function called `disperser_link_zip()` that runs in the background and reads in all the relevant files (i.e., those that correspond to the provided units) produced by the `run_disperser_parallel()` function and saves them. Then it links them to ZIP codes.
#'
#'
#'
#' @param start.date this argument is not necessary, but can be used if the user is interested in specifying a specific date to start the analysis with as opposed to using months. For example `start.date="2005-01-02"` for 2 January 2005. This argument are set to `NULL` by default and the function computes the start and the end dates using the `year.mons` provided.
#'
#' @param start.end this argument is not necessary, but can be used if the user is interested in specifying a specific date to end the analysis with as opposed to using months. For example `start.date="2005-01-02"` for 2 January 2005.This argument are set to `NULL` by default and the function computes the start and the end dates using the `year.mons` provided.
#'
#' @param year.mons these are the months for which we would like to do the linking. You can use the get_yearmon() function to create a vector that can be an input here.
#' @param pbl.height `pbl.height = pblheight` by default but you can change it. See the vignette
#' @param crosswalk. `crosswalk. = crosswalk` by default but you can change it. See the vignette
#' @param mc.cores `link_all_units_zip()` enables the parallel run by default splitting different months on different cores, but you can make it serial just by setting the `mc.cores` argument to `1`.As mentioned before  `link_all_units_zip()` enables the parallel run by default splitting different months on different cores, but you can make it serial just by setting the `mc.cores` argument to `1`.
#' @duration.run.hours `duration.run.hours = 240` by default which equals 10 days. 10 days is the maximum (approximately) that sulfur stays in the atmosphere before it deposits to the ground.
#' overwrite `overwrite = FALSE` by default. Would you like to overwrite files that already exist?
#' @return vector of months that you can loop over


#' @export link_all_units_zip

link_all_units_zip <- function(units.run,
  mc.cores = detectCores(),
  year.mons = NULL,
  start.date = NULL,
  end.date = NULL,
  pbl.height = pblheight,
  crosswalk. = crosswalk,
  duration.run.hours = 240,
  overwrite = FALSE) {


  if ((is.null(start.date) |
      is.null(end.date)) & is.null(year.mons)) {
    stop("Define either a start.date and an end.date OR a year.mons")
  }

  zip_link_parallel <- function(unit) {
    linked_zips <- parallel::mclapply(
      yearmons,
      disperseR::disperser_zip_link,
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

  out <-
    unitsrun[, zip_link_parallel(.SD), by = seq_len(nrow(unitsrun))]
  out <- out[, comb := paste("month: ", out[, month], " unitID :", out[, unitID], sep = "")]
  return(out)
}
