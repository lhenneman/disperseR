#########################################################
################# link_zip
#' @export link_to
link_to <- function(d,
                    link.to = 'zips',
                    p4string,
                    zc = NULL,
                    cw = NULL,
                    county.sp = NULL,
                    rasterin = NULL,
                    res.link. = 12000,
                    pbl. = TRUE,
                    crop.usa = FALSE) {

  xy <- d[, .(lon, lat)]
  spdf.in <- SpatialPointsDataFrame( coords = xy,
                                     data = d,
                                     proj4string = CRS( "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  spdf <- spTransform( spdf.in, p4string)

  # create raster with resolution res.link.
  e <- extent(spdf)
  e@xmin <- floor(  e@xmin / res.link.) * res.link.
  e@ymin <- floor(  e@ymin / res.link.) * res.link.
  e@xmax <- ceiling(e@xmax / res.link.) * res.link.
  e@ymax <- ceiling(e@ymax / res.link.) * res.link.
  r <- raster( ext = e, resolution = res.link., crs = CRS( proj4string( spdf)))
  values( r) <- NA

  # count number of particles in each cell,
  # find original raster cells, divide number by pbl
  cells <- cellFromXY( r, spdf)
  tab <- table( cells)

  # concentration - divide by pbl or not
  if( pbl.){
    # reproject pbl's to raster
    pbl_layer <- subset_nc_date( hpbl_brick = rasterin,
                                 vardate = d$Pdate[1])
    pbl_layer.d <- projectRaster( pbl_layer, r)
    pbls <- pbl_layer.d[as.numeric( names( tab))]

    r[as.numeric( names( tab))] <- tab / pbls
  } else
    r[as.numeric( names( tab))] <- tab

  # crop around point locations for faster extracting
  r2 <- crop( trim(r,
                   padding = 1),
              e)

  # mask around USA for smaller files
  if( crop.usa){
    usa <- rnaturalearth::ne_countries(scale = 110, type = "countries", country = "United States of America",
                                       geounit = NULL, sovereignty = NULL,
                                       returnclass = c("sp"))
    usa.sub <- disaggregate(usa)[6,]
    crop.extent <- usa.sub
    crop.extent.proj <- projectExtent( crop.extent, p4string)
    r2 <- crop( r2, crop.extent.proj)
  }

  # if return.grid, return xyz object
  if( link.to == 'grids'){
    xyz <- data.table( rasterToPoints(r2))
    names(xyz)[3] <- 'N'

    return( xyz)
  }

  #  convert to polygons for faster extracting
  r3 <- rasterToPolygons(r2)

  # if county.so, return xyz object
  if( link.to == 'counties'){
    print( 'Linking counties!')
    county.o <- over( county.sp,
                      r3,
                      fn = mean)
    county.dt <- data.table( county.o)
    county.dt <- cbind( as.data.table( county.sp[, c( 'statefp', 'countyfp', 'state_name',
                                                      'name', 'geoid')]),
                        county.dt)
    setnames( county.dt, names( r3), 'N')

    # if "over" returned no matches, need a vector of NA's
    if( nrow( county.dt) == 1 & is.na( county.dt[1, 1])){
      county.dt <- cbind( as.data.table( county.sp[, c( 'statefp', 'countyfp', 'state_name',
                                                        'name', 'geoid')]),
                          data.table( X = as.numeric( rep( NA, length( county.sp)))))
      setnames( county.dt, "X", 'N')
    }

    return( county.dt)
  }

  #crop zip codes to only use ones over the extent
  zc_trim <- crop( zc,
                   snap = 'out',
                   e)

  zc_groups <- ceiling(seq_along(zc_trim) / 1000)

  #extract average concentrations over zip codes
  #name column as 'N', combine with zip codes
  #define function to not run out of memory
  over_fn <- function( group,
                       zc_dt,
                       groups,
                       raster_obj) {

    dt <- data.table( over( zc_dt[ groups %in% group,],
                            raster_obj,
                            fn = mean))

    # if "over" returned no matches, need a vector of NA's
    if( nrow( dt) == 1 & is.na( dt[1])){
      dt <- data.table( X = as.numeric( rep( NA, length( zc_dt[ groups %in% group,]))))
      setnames( dt, "X", names( raster_obj))
    }

    return( dt)
  }

  or <- rbindlist( lapply( unique( zc_groups),
                           over_fn,
                           zc_dt = zc_trim,
                           groups = zc_groups,
                           raster_obj = r3))

  names(or) <- 'N'
  D <- data.table( cbind( zc_trim@data,
                          or))

  setnames( D, 'ZCTA5CE10', 'ZCTA')
  cw$ZCTA <- formatC( cw$ZCTA,
                      width = 5,
                      format = "d",
                      flag = "0") # to merge on zcta ID
  M <- merge( D, cw, by = "ZCTA", all = F, allow.cartesian = TRUE) # all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)
  M[, ZIP:= formatC( ZIP,
                     width = 5,
                     format = "d",
                     flag = "0")]
  M$ZIP <- as(M$ZIP, 'character')
  M <- na.omit( M)
  return(M)
}

#########################################################
################# trim_zero

#' @export trim_zero
trim_zero <- function(Min) {
  M <- copy(Min)

  p_zero_df <- M[height == 0,]
  particles <- unique(p_zero_df$particle_no)

  for (p in particles) {
    h_zero <- p_zero_df[particle_no == p, hour]
    M[particle_no == p & hour >= h_zero,] <- NA
  }
  M <- na.omit(M)
  return(M)
}

#########################################################
################# trim_pbl
#' @export trim_pbl
trim_pbl <- function(Min,
                     rasterin) {
  Sys.setenv(TZ = 'UTC')
  M <- copy(Min)
  M[, ref := 1:nrow(M)]

  #Find unique month-year combinations
  M[, Pmonth := formatC(month(Pdate),
                        width = 2,
                        format = "d",
                        flag = "0")]
  M[, Pyear  := formatC(year(Pdate),
                        width = 2,
                        format = "d",
                        flag = "0")]
  my <-
    data.table(expand.grid(data.table(mo = unique(M[, Pmonth]),
                                      yr = unique(M[, Pyear]))))

  #Convert M to spatial points data frame
  xy <- M[, .(lon, lat)]
  spdf <- SpatialPointsDataFrame(
    coords = xy,
    data = M,
    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  )

  # identify cells for each parcel location
  spdf$rastercell <- cellFromXY(rasterin, spdf)
  spdf.dt <- na.omit(data.table(spdf@data))

  for (m in 1:nrow(my)) {
    mon <- my[m, mo]
    yer <- my[m, yr]
    day <- paste(yer, mon, '01', sep = '-')

    pbl_layer <- subset_nc_date(hpbl_brick = rasterin,
                                varname = 'hpbl',
                                vardate = day)

    spdf.dt[Pmonth %in% mon & Pyear %in% yer,
            pbl := pbl_layer[spdf.dt[Pmonth %in% mon &
                                       Pyear %in% yer, rastercell]]]
  }
  spdf.dt <- spdf.dt[height < pbl]
  return(M[spdf.dt$ref,
           .(lon, lat, height, Pdate, hour)])
}


#' @export disperser_link_grids
disperser_link_grids <- function(  month_YYYYMM = NULL,
                                   start.date = NULL,
                                   end.date = NULL,
                                   unit,
                                   duration.run.hours = duration.run.hours,
                                   pbl.height,
                                   res.link.,
                                   overwrite = F,
                                   pbl. = TRUE,
                                   crop.usa = FALSE,
                                   return.linked.data.){
  unitID <- unit$ID

  if( (is.null( start.date) | is.null( end.date)) & is.null( month_YYYYMM))
    stop( "Define either a start.date and an end.date OR a month_YYYYMM")
  if( dim( unit)[1] > 1)
    stop( "Please supply a single unit (not multiple)")

  ## create start.date and end.date if month_YYYYMM is provided
  if( is.null( start.date) | is.null( end.date)){
    start.date <- as.Date( paste( substr( month_YYYYMM, 1, 4),
                                  substr( month_YYYYMM, 5, 6),
                                  '01', sep = '-'))

    end.date <- seq( start.date,
                     by = paste (1, "months"),
                     length = 2)[2] - 1
  }

  if( is.null( month_YYYYMM))
    month_YYYYMM <- paste( start.date, end.date, sep = '_')

  month_YYYYMM <- as( month_YYYYMM, 'character')

  ## name the eventual output file
  output_file <- file.path( ziplink_dir,
                            paste0("gridlinks_",
                                   unit$ID, "_",
                                   start.date, "_",
                                   end.date,
                                   ".fst"))

  ## Run the zip linkages
  if( !file.exists( output_file) | overwrite == T){

    ## identify dates for hyspdisp averages and dates for files to read in
    vec_dates <-
      as(
        seq.Date(
          as.Date(start.date),
          as.Date(end.date),
          by = '1 day'),
        'character')
    vec_filedates <-
      seq.Date(
        from = as.Date( start.date) - ceiling( duration.run.hours / 24),
        to = as.Date( end.date),
        by = '1 day'
      )

    ## list the files
    pattern.file <-
      paste0( '_',
              gsub( '[*]', '[*]', unit$ID),
              '_(',
              paste(vec_filedates, collapse = '|'),
              ').*\\.fst$'
      )
    hysp_dir.path <-
      file.path( hysp_dir,
                 unique( paste( year( vec_filedates),
                                formatC( month( vec_filedates), width = 2, flag = '0'),
                                sep = '/')))
    files.read <-
      list.files( path = hysp_dir.path,
                  pattern = pattern.file,
                  recursive = F,
                  full.names = T)

    ## read in the files
    l <- lapply( files.read, read.fst, as.data.table = TRUE)

    ## Combine all parcels into single data table
    d <- rbindlist(l)
    if( length( d) == 0)
      return( paste( "No files available to link in", month_YYYYMM))
    print(  paste( Sys.time(), "Files read and combined"))

    ## Trim dates & first hour
    d <- d[ as( Pdate, 'character') %in% vec_dates & hour > 1, ]

    ## Trim PBL's
    if( pbl.){
      #Check if extent matches the hpbl raster
      d_xmin <- min( d$lon)
      e_xmin <- extent( pbl.height)[1]
      if( d_xmin < e_xmin - 5)
        pbl.height <- rotate( pbl.height)

      d_trim <- trim_pbl( d,
                          rasterin = pbl.height)
      print( paste( Sys.time(), "PBLs trimmed"))
    } else
      d_trim <- d

    ## Link to grid
    p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
    disp_df_link <- link_to( d = d_trim,
                             link.to = 'grids',
                             p4string = p4s,
                             rasterin = pbl.height,
                             res.link. = res.link.,
                             pbl. = pbl.,
                             crop.usa = crop.usa)
    print(  paste( Sys.time(), "Grids linked"))
    out <- disp_df_link
    out$month <- as( month_YYYYMM, 'character')
    out$ID <- unitID

    if( nrow( out) != 0){
      ## write to file
      write.fst( out,output_file)
      print( paste( Sys.time(), "Linked grids and saved to", output_file))
    }
  } else {
    print( paste("File", output_file, "already exists! Use overwrite = TRUE to over write"))
    if( return.linked.data.)
      out <- read.fst( output_file, as.data.table = TRUE)
  }

  if( !return.linked.data.)
    out <- data.table( x = numeric(), y = numeric(), N = numeric())

  out$month <- as( month_YYYYMM, 'character')
  out$ID <- unitID
  suppressWarnings( out[, V1 := NULL])
  return( out)
}


#' @export disperser_link_counties
disperser_link_counties <- function( month_YYYYMM = NULL,
                                     start.date = NULL,
                                     end.date = NULL,
                                     counties,
                                     unit,
                                     duration.run.hours = duration.run.hours,
                                     pbl.height,
                                     res.link.,
                                     overwrite = F,
                                     pbl. = TRUE,
                                     return.linked.data.){

  unitID <- unit$ID

  if ((is.null(start.date) | is.null(end.date)) & is.null(month_YYYYMM))
    stop("Define either a start.date and an end.date OR a month_YYYYMM")
  if (dim(unit)[1] > 1)
    stop("Please supply a single unit (not multiple)")

  ## create start.date and end.date if month_YYYYMM is provided

  if (is.null(start.date) | is.null(end.date)) {
    start.date <-
      as.Date(paste(
        substr(month_YYYYMM, 1, 4),
        substr(month_YYYYMM, 5, 6),
        '01',
        sep = '-'
      ))
    end.date <-
      seq(start.date, by = paste (1, "months"), length = 2)[2] - 1
  }

  ## name the eventual output file
  output_file <-
    file.path( ziplink_dir,
               paste0("countylinks_", unit$ID, "_", start.date, "_", end.date, ".fst"))

  ## Run the zip linkages
  if( !file.exists( output_file) | overwrite == T){

    ## identify dates for hyspdisp averages and dates for files to read in
    vec_dates <-
      as(
        seq.Date(
          as.Date(start.date),
          as.Date(end.date),
          by = '1 day'),
        'character')
    vec_filedates <-
      seq.Date(
        from = as.Date( start.date), #previously defines as: as.Date( start.date) - ceiling( duration.run.hours / 24),
        to = as.Date( end.date),
        by = '1 day'
      )

    ## list the files
    pattern.file <-
      paste0( '_',
              gsub( '[*]', '[*]', unit$ID),
              '_(',
              paste(vec_filedates, collapse = '|'),
              ').*\\.fst$'
      )
    hysp_dir.path <-
      file.path( hysp_dir,
                 unique( paste( year( vec_filedates),
                                formatC( month( vec_filedates), width = 2, flag = '0'),
                                sep = '/')))
    files.read <-
      list.files( path = hysp_dir.path,
                  pattern = pattern.file,
                  recursive = F,
                  full.names = T)

    ## read in the files
    l <- lapply(files.read, read.fst, as.data.table = TRUE)

    ## Combine all parcels into single data table
    d <- rbindlist(l)
    if( length( d) == 0)
      return( paste( "No files available to link in", month_YYYYMM))
    print(  paste( Sys.time(), "Files read and combined"))

    ## Trim dates & first hour
    d <- d[ as( Pdate, 'character') %in% vec_dates & hour > 1, ]

    ## Trim PBL's
    if( pbl.){
      #Check if extent matches the hpbl raster
      d_xmin <- min( d$lon)
      e_xmin <- extent( pbl.height)[1]
      if( d_xmin < e_xmin - 5)
        pbl.height <- rotate( pbl.height)

      d_trim <- trim_pbl( d,
                          rasterin = pbl.height)
      print( paste( Sys.time(), "PBLs trimmed"))
    } else
      d_trim <- d

    counties.sp <- sf::as_Spatial( counties)
    p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
    counties.sp <- spTransform(counties.sp, p4s)

    disp_df_link <- link_to( d = d_trim,
                             link.to = 'counties',
                             county.sp = counties.sp,
                             p4string = proj4string( counties.sp),
                             rasterin = pbl.height,
                             res.link. = res.link.,
                             pbl. = pbl.)

    print(  paste( Sys.time(), "Counties linked"))

    out <- as.data.table( disp_df_link)
    out$month <- as( month_YYYYMM, 'character')
    out$ID <- unitID

    if( nrow( out) != 0){
      ## write to file
      write.fst( out, output_file)

      print( paste( Sys.time(), "Linked counties and saved to", output_file))
    }
  } else {
    print( paste("File", output_file, "already exists! Use overwrite = TRUE to over write"))

    if( return.linked.data.)
      out <- read.fst( output_file, as.data.table = TRUE)
  }

  if( !return.linked.data.)
    out <- data.table( statefp = character(),
                       countyfp = character(),
                       state_name = character(),
                       name = character(),
                       geoid = character(),
                       N = numeric())

  out$month <- as( month_YYYYMM, 'character')
  out$ID <- unitID
  suppressWarnings( out[, V1 := NULL])
  return( out)
}

#' @export disperser_link_zips
disperser_link_zips <- function(month_YYYYMM = NULL,
                                start.date = NULL,
                                end.date = NULL,
                                unit,
                                duration.run.hours = duration.run.hours,
                                pbl.height=NULL,
                                crosswalk.,
                                res.link.,
                                overwrite = F,
                                pbl. = TRUE,
                                return.linked.data.) {
  unitID <- unit$ID

  if ((is.null(start.date) | is.null(end.date)) & is.null(month_YYYYMM))
    stop("Define either a start.date and an end.date OR a month_YYYYMM")
  if (dim(unit)[1] > 1)
    stop("Please supply a single unit (not multiple)")

  ## create start.date and end.date if month_YYYYMM is provided

  if (is.null(start.date) | is.null(end.date)) {
    start.date <-
      as.Date(paste(
        substr(month_YYYYMM, 1, 4),
        substr(month_YYYYMM, 5, 6),
        '01',
        sep = '-'
      ))
    end.date <-
      seq(start.date, by = paste (1, "months"), length = 2)[2] - 1
  }

  if( is.null( month_YYYYMM))
    month_YYYYMM <- paste( start.date, end.date, sep = '_')

  month_YYYYMM <- as( month_YYYYMM, 'character')

  ## name the eventual output file
  zip_output_file <-
    file.path(ziplink_dir,
              paste0("ziplinks_", unit$ID, "_", start.date, "_", end.date, ".fst"))


  ## Run the zip linkages
  if (!file.exists(zip_output_file) | overwrite == T) {
    ## identify dates for hyspdisp averages and dates for files to read in
    vec_dates <-
      as(
        seq.Date(
          as.Date(start.date),
          as.Date(end.date),
          by = '1 day'),
        'character')
    vec_filedates <-
      seq.Date(
        from = as.Date( start.date) - ceiling( duration.run.hours / 24),
        to = as.Date( end.date),
        by = '1 day'
      )

    ## list the files
    pattern.file <-
      paste0( '_',
              gsub( '[*]', '[*]', unit$ID),
              '_(',
              paste(vec_filedates, collapse = '|'),
              ').*\\.fst$'
      )
    hysp_dir.path <-
      file.path( hysp_dir,
                 unique( paste( year( vec_filedates),
                                formatC( month( vec_filedates), width = 2, flag = '0'),
                                sep = '/')))
    files.read <-
      list.files( path = hysp_dir.path,
                  pattern = pattern.file,
                  recursive = F,
                  full.names = T)


    ## read in the files
    l <- lapply(files.read, read.fst, as.data.table = TRUE)

    ## Combine all parcels into single data table
    d <- rbindlist(l)

    if (length(d) == 0) {
      return(paste("No files available to link in", month_YYYYMM))
    }
    print(paste(Sys.time(), "Files read and combined"))

    ## Trim dates & first hour
    d <- d[ as( Pdate, 'character') %in% vec_dates & hour > 1, ]

    ## Trim PBL's
    if( pbl.){
      #Check if extent matches the hpbl raster
      d_xmin <- min(d$lon)
      e_xmin <- extent(pbl.height)[1]
      if (d_xmin < e_xmin - 5) {
        pbl.height <- rotate(pbl.height)
      }

      d_trim <- trim_pbl( d,
                          rasterin = pbl.height)
      print( paste( Sys.time(), "PBLs trimmed"))
    } else
      d_trim <- d

    ## Link zips
    p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
    disp_df_link <-
      link_to(
        d = d_trim,
        link.to = 'zips',
        zc = zcta,
        cw = crosswalk.,
        p4string = p4s,
        rasterin = pbl.height,
        res.link. = res.link.,
        pbl. = pbl.
      )
    print(paste(Sys.time(), "ZIPs linked"))

    out <- disp_df_link[, .(ZIP, N)]
    out$ZIP <- formatC(out$ZIP, width = 5, format = "d", flag = "0")
    out$month <- as( month_YYYYMM, 'character')
    out$ID <- unitID

    ## write to file
    if (nrow(out) != 0) {
      write.fst(out, zip_output_file)
      print(paste(Sys.time(), "Linked ZIPs and saved to", zip_output_file))
    }
  } else {
    print(paste(
      "File",
      zip_output_file,
      "already exists! Use overwrite = TRUE to over write"
    ))
    if( return.linked.data.)
      out <- read.fst(zip_output_file, as.data.table = TRUE)

  }

  if( !return.linked.data.)
    out <- data.table( ZIP = character(), N = numeric())

  out$month <- as( month_YYYYMM, 'character')
  out$ID <- unitID
  out <- out[, .(ZIP, N, month, ID)]
  return(out)
}
