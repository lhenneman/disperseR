#' @export calculate_exposure
calculate_exposure <- function(year.E,
                               year.D,
                               link.to = 'zips',
                               pollutant = 'SO2..tons.',
                               units.mo,
                               rda_file = 'loaded',
                               exp_dir = NULL,
                               source.agg = c('total', 'facility', 'unit'),
                               time.agg = c('year', 'month'),
                               return.monthly.data = F) {
  `%ni%` <- Negate(`%in%`)

  #define defaults if none provided
  if (length(source.agg) > 1) {
    print('Multiple source.agg provided, deaulting to "total".')
    source.agg <- 'total'
  }
  if (source.agg %ni% c('total', 'facility', 'unit')) {
    stop('source.agg not recognized, please provide one of c("total", "facility", "unit").')
  }
  if (length(time.agg) > 1) {
    print('Multiple time.agg provided, deaulting to "year".')
    time.agg <- 'year'
  }
  if (time.agg %ni% c('year', 'month')) {
    stop('time.agg not recognized, please provide one of c("year", "month").')
  }

  #define defaults if none provided
  if (rda_file != 'loaded')
    load(rda_file, envir = environment())

  # start with the first map available in the given year
  # map.names <- paste0("MAP", 1:12, ".", year.D)
  # for( m in 1:12){
  #   test <- map.names[m] %in% ls(envir = globalenv()) | map.names[m] %in% ls()
  #   if( test)
  #     break
  # }

  # Create directory to store output files if it does not exist
  if (is.null(exp_dir)) {
    exp_dir <- file.path(getwd(), 'rdata_hyspdisp')
    print(paste('No exp_dir provided. Defaulting to', exp_dir))
  }
  dir.create(exp_dir, recursive = TRUE, showWarnings = F)

  #initiate exposure data.table
  exposures <-  data.table()

  #initiate list of monthly files
  monthly.filelist <- c()

  #Iterate over months of the year
  print(
    paste0(
      "Calculating ", link.to, " exposures for HYSPLIT year ",
      year.D,
      " and emissions year ",
      year.E,
      "!"
    )
  )
  for (i in 1:12) {
    PP.units_monthly <- subset(units.mo, month == i & year == year.E)
    setnames(PP.units_monthly, pollutant, 'pollutant')

    #Aggregate unit power plant emissions to unit level
    PP_monthly <- PP.units_monthly[!duplicated(uID)]
    PP_monthly <- PP_monthly[is.na(pollutant), pollutant := 0]

    #get HYPSPLIT mappings
    map.name <- paste0("MAP", i, ".", year.D)
    if (map.name %ni% ls(envir = globalenv())
        & map.name %ni% ls()) {
      warning(
        paste(
          map.name,
          'not loaded in environment. If you want it linked, either load RData file before or specify rda_file'
        )
      )
      next
    }
    if (map.name %in% ls()) {
      month_mapping <-
        data.table(eval(parse(text = map.name)))
    } else{
      month_mapping <- data.table(eval(parse(text = map.name),
                                       envir = globalenv()))
    }

     #melt them to long format
    if( link.to == 'zips'){
      id.v <- 'ZIP'
      month_mapping <- month_mapping[ZIP != 'ZIP']
    } else if( link.to == 'counties'){
      id.v <- c("statefp", "countyfp", "state.name", "name", "geoid")
    } else if( link.to == 'grids')
      id.v <- c('x', 'y')

    month_mapping[is.na(month_mapping)] <- 0
    names(month_mapping) <-
      gsub('_|-|\\*', '.', names(month_mapping))

    month_mapping_long <- melt(
      month_mapping,
      id.vars = id.v,
      variable.factor = FALSE,
      variable.name = "uID",
      value.name = "N"
    )
    if (sapply(month_mapping_long, class)['N'] == 'character')
      month_mapping_long[, `:=`(N = as.double(N))]

    #This is what I want - pollutant-weighted emissions trajectories
    PP.linkage <-
      merge(month_mapping_long,
            PP_monthly,
            by = 'uID',
            all.y = T)

    #  clean house
    rm(list = c('month_mapping_long', 'PP_monthly', 'month_mapping'))

    # Sum by ZIP and uID if calculating annual
    if (time.agg == 'year') {
      # define aggregation strings
      if (source.agg == 'total')
        sum.by <- id.v
      if (source.agg == 'facility')
        sum.by <- c(id.v, 'FacID')
      if (source.agg == 'unit')
        sum.by <- c(id.v, 'uID')

      # calculate exposure, label year/month
      PP.linkage[, `:=` (Exposure  = pollutant * N)]

      # Append running data frame
      exposures <- data.table(rbind(exposures,
                                    PP.linkage[, list(Exposure = sum(Exposure)),
                                               by = sum.by]))

      # sum over the year so far
      exposures <- exposures[, list(Exposure = sum(Exposure)),
                             by = sum.by]
    } else {
      # define aggregation strings
      if (source.agg == 'total')
        sum.by <- c(id.v, 'yearmonth')
      if (source.agg == 'facility')
        sum.by <- c(id.v, 'FacID', 'yearmonth')
      if (source.agg == 'unit')
        sum.by <- c(id.v, 'uID', 'yearmonth')

      # add month
      PP.linkage[, `:=` (
        Exposure  = pollutant * N,
        yearmonth = paste0(year.E, i)
      )]

      # Append running data frame
      exposures <- data.table(rbind(exposures,
                                    PP.linkage[, list(hyads = sum(Exposure)),
                                               by = sum.by]))[hyads > 0]

      # write to file, add monthly file to list if not empty data.table
      file.mo <- file.path(exp_dir,
                           paste0(
                             link.to,
                             '_exposures_byunit_',
                             paste0(year.E, '_', formatC(
                               i, width = 2, flag = '0'
                             )),
                             '.csv'
                           ))

      if( link.to == 'zips')
        exposures <- exposures[ZIP != '   NA']

      if (nrow(exposures) != 0) {
        write.csv(exposures,
                  file = file.mo)
        monthly.filelist[i] <- file.mo
      }
      #re-initiate ZIP exposure data.table
      exposures <-  data.frame()
    }

  }

  if (time.agg == 'year') {
    setnames(exposures,
             c('Exposure'),
             c('hyads'))
    exposures[,  `:=` (
      year.E = year.E,
      year.D = year.D
    )]
    #convert 3-digit zip code to 5, add emissions and hysplit years
    if( link.to == 'zips'){
      exposures[,  `:=` (
        ZIP = formatC(
          as.integer(ZIP),
          width = 5,
          flag = "0",
          format = "d"
        ))]
      exposures <- exposures[ZIP != '   NA']
    }
    return(exposures)
  } else {
    if (return.monthly.data) {
      out <- rbindlist(lapply(na.omit(monthly.filelist),
                              fread,
                              keepLeadingZeros = T,
                              drop = 'V1'))
      out[,  `:=` (
        hyads = as(hyads, 'numeric')
      )]

      if( link.to == 'zips')
        out <- out[ZIP != '   NA']

      return(out)
    } else
      return(monthly.filelist)
  }
}
