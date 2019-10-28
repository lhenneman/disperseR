#' @export combine_monthly_ziplinks

combine_monthly_links <- function( month_YYYYMMs,
                                   link.to = 'zips',
                                   filename = NULL) {

  names.map <- c()

  for (ym in month_YYYYMMs) {

    year.h <- substr(ym, 1, 4)
    month.m <- as.integer(substr(ym, 5, 6))
    month.h <- formatC(month.m, width = 2, format = "d", flag = "0")

    if( link.to == 'zips'){
      pattern <- paste0('ziplinks.*', year.h, '-', month.h)
    } else if( link.to == 'grids'){
      pattern <- paste0('gridlinks.*', year.h, '-', month.h)
    } else if( link.to == 'counties'){
      pattern <- paste0('countylinks.*', year.h, '-', month.h)
    }

    files.month <-
      list.files(path = ziplink_dir,
                 pattern = pattern,
                 full.names = T)

    if (length(files.month) == 0) {
      print(paste("No data files for month_YYYYMMs", ym))
    } else {
      print(paste('Reading and merging month', month.h, 'in year', year.h))

      unitnames <-
        gsub(paste0('.*links_|_', year.h, '-', month.h , '.*csv$'),
             '',
             files.month)
      names(files.month) <- unitnames


      if( link.to == 'zips'){
        data.h <- lapply(seq_along(files.month),
                         disperseR::read_ziplinks_subfun,
                         files.month)

        MergedDT  <- rbindlist(data.h)
        Merged_cast <-
          dcast(MergedDT,
                ZIP ~ ID,
                fun.aggregate = sum,
                value.var = "N")
      } else if( link.to == 'grids'){
        data.h <- lapply(seq_along(files.month),
                         disperseR::read_gridlinks_subfun,
                         files.month)

        MergedDT  <- rbindlist(data.h)
        Merged_cast <- dcast(MergedDT,
                             x + y ~ ID,
                             fun.aggregate = sum,
                             value.var = "N")

        } else if( link.to == 'counties'){
        data.h <- lapply(seq_along(files.month),
                         disperseR::read_countylinks_subfun,
                         files.month)

        MergedDT  <- rbindlist( data.h)
        Merged_cast <- dcast(MergedDT,
                             statefp + countyfp + state_name + name + geoid ~ ID,
                             fun.aggregate = sum,
                             value.var = "N")
      }

      # assign to mappings
      name.map <- paste0("MAP", month.m, ".", year.h)
      names.map <- append(names.map, name.map)
      assign(name.map, Merged_cast)
      rm("MergedDT", "Merged_cast")
    }
  }

  # put all grid links on consistent extent
  if( link.to == 'grid'){
    # gather output
    out.d <- mget(names.map)
    out.r <- lapply( out.d, rasterFromXYZ)
    out.ids <- lapply( out.d, function( dt) names( dt))

    #calculate consistent extent
    out.e <- extent( Reduce( extend, out.r)) #lapply( out.r, extent)

    #apply extent to all rasters
    out.b <- lapply( out.r, extend, out.e)

    #convert to data.table
    out.dt <- lapply( out.b, function( x) data.table( rasterToPoints( x)))

    #round to nearest meter to ease re-rasterizing later
    out.dt <- lapply( out.dt,
                      function( dt)
                        dt[, `:=`( x = round( x),
                                   y = round( y))])

    #extract from list
    lapply( names( out.dt),
            function( x, l, n){
              names( l[[x]]) <- out.ids[[x]]
              assign( x, l[[x]], envir = parent.env( environment()))},
            out.dt, out.ids)
  }

  if( is.null( filename))
    filename <- paste0( 'hyads_unwgted_', link.to, '.RData')
  rda.filename <- file.path(rdata_dir, filename)
  save(list = names.map, file = rda.filename)

  print(paste("Monthly RData file written to", rda.filename))
  return(mget(names.map))
}


