

#' Define species parameters
#'
#' \code{define_input}  takes as input a species name and
#' provides a table of HYSPLIT species parameters accepted by `run_disperser_parallel()`
#'
#' @description Once HYSPLIT has been run for each emissions event, the simulated parcel locations are aggregated by source, time, and location. The functions in `disperseR` are written to enable runs of many emissions events. o define an object that includes all emission events in a given time period, we can use the helper function `define_inputs()`. This takes as inputs a starting and ending day, and outputs a table of values whose rows will later correspond to inputs into the main `disperseR` functions. The following command combines the units defined above with four times a day for January-July in 2005
#'
#' @param startday starting day for HyADS runs, must be in standard YYY-MM-DD format
#' @param endday starting day for HyADS runs, must be in standard YYY-MM-DD format
#' @param start.hours vector of starting hours (integers between 0 and 23) for each HYSPLIT run. Defaults to c(0, 6, 12, 18), i.e., HYSPLIT runs initiated at 12:00am, 6:00am, 12:00pm, and 6:00pm.
#' @param duration `duration = 240` denotes that the emitted air parcels are tracked for 240 hours (10 days). 10 days is the maximum (approximately) that sulfur stays in the atmosphere before it deposits to the ground.
#' @return This function returns a data table of run parameters accepted by `run_disperser_parallel()`
#'
#'Once HYSPLIT has been run for each emissions event, the simulated parcel locations are aggregated by source, time, and location. The functions below are written to enable runs of many emissions events.



#' @export define_inputs
## define species parameters
define_inputs <-
  function(units,
    startday,
    endday,
    start.hours =  c(0, 6, 12, 18),
    duration = 240) {

    startday.date <- as.Date(startday)
    endday.date   <- as.Date(endday)

    out <- data.table(
      expand.grid(
        ID = unique( units$ID),
        year = unique( units$year),
        start_hour = start.hours,
        start_day = seq.Date(
          from = as.Date(startday.date),
          to =   as.Date(endday.date),
          by = '1 day'
        ),
        duration_emiss_hours = 1,
        duration_run_hours = duration
      )
    )

    # get only the input year for which we get units data
    out <- out[year==year( start_day)]
    out <- unique(merge(out, units, by = c('ID', 'year')))

    return(out)
  }
