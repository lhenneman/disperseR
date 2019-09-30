disperser_grid_link <- function(  month_YYYYMM = NULL,
                                  start_date = NULL,
                                  end_date = NULL,
                                  unit,
                                  duration.run.hours = duration.run.hours,
                                  pbl.height,
                                  overwrite = F){

  if( (is.null( start_date) | is.null( end_date)) & is.null( month_YYYYMM))
    stop( "Define either a start_date and an end_date OR a month_YYYYMM")
  if( dim( unit)[1] > 1)
    stop( "Please supply a single unit (not multiple)")

  ## create start_date and end_date if month_YYYYMM is provided
  if( is.null( start_date) | is.null( end_date)){
    start_date <- as.Date( paste( substr( month_YYYYMM, 1, 4),
                                  substr( month_YYYYMM, 5, 6),
                                  '01', sep = '-'))

    end_date <- seq( start_date,
                     by = paste (1, "months"),
                     length = 2)[2] - 1
  }

  ## name the eventual output file
  output_file <- file.path( zpc_dir,
                                 paste0("gridlinks_",
                                        unit$ID, "_",
                                        start_date, "_",
                                        end_date,
                                        ".csv"))

  ## Run the zip linkages
  if( !file.exists( output_file) | overwrite == T){

    ## identify dates for hyspdisp averages and dates for files to read in
    vec_dates <- seq.Date( as.Date( start_date),
                           as.Date( end_date),
                           by = '1 day')
    vec_filedates <- seq.Date( from = as.Date( start_date) - ceiling( duration_run_hours / 24),
                               to = as.Date( end_date),
                               by = '1 day')

    ## list the files
    pattern.file <- paste0( '_', gsub( '[*]', '[*]', unit$ID), '_(', paste(vec_filedates, collapse = '|'), ')')
    files.read <- list.files(path = hyo_dir,
                             pattern = pattern.file,
                             full.names = T)

    ## if hyo_dir2 povided, check for files there too
    if( !is.null( hyo_dir2))
      files.read <- append( files.read, list.files(path = hyo_dir2,
                                                   pattern = pattern.file,
                                                   full.names = T))

    ## read in the files
    l <- lapply(files.read, fread, keepLeadingZeros = TRUE)

    ## Combine all parcels into single data table
    d <- rbindlist(l)
    if( length( d) == 0)
      return( paste( "No files available to link in", month_YYYYMM))
    print(  paste( Sys.time(), "Files read and combined"))

    ## Trim dates & first hour
    d <- d[d$Pdate %in% as( c( vec_dates), "character") &
             hour > 1,]

    #Check if extent matches the hpbl raster
    d_xmin <- min( d$lon)
    e_xmin <- extent( hpbl_raster)[1]
    if( d_xmin < e_xmin - 5)
      hpbl_raster <- rotate( hpbl_raster)

    ## Trim PBL's
    d_trim <- trim_pbl( d,
                        rasterin = hpbl_raster)
    print( paste( Sys.time(), "PBLs trimmed"))

    ## Link to grid
    disp_df_link <- link_zip( d = d_trim,
                              p4string = p4s,
                              rasterin = hpbl_raster,
                              return.grid = T)
    print(  paste( Sys.time(), "Grids linked"))
    out <- disp_df_link

    if( nrow( out) != 0){
      ## write to file
      write.csv( out,output_file)
      print( paste( Sys.time(), "Linked ZIPs  and saved to", output_file))
    }
  } else {
    print( paste("File", output_file, "already exists! Use overwrite = TRUE to over write"))
    out <- fread( output_file, keepLeadingZeros = TRUE)
  }

  return( out)
}
