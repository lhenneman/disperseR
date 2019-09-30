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
        ID = units$ID,
        year = units$year,
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

    out <- out[as.character(year)==format(as.Date(out$start_day, format="%d/%m/%Y"),"%Y")]
    out <- unique(merge(out, units, by = c('ID', 'year')))
    # get only the input year for which we get units data

    return(out)
  }
