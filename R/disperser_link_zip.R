disperser_zip_link <- function(month_YYYYMM = NULL,
  start.date = NULL,
  end.date = NULL,
  unit,
  duration.run.hours = duration.run.hours,
  pbl.height=NULL,
  crosswalk.,
  overwrite = F) {
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
  zip_output_file <-
    file.path(ziplink_dir,
      paste0("ziplinks_", unit$ID, "_", start.date, "_", end.date, ".csv"))


  ## Run the zip linkages
  if (!file.exists(zip_output_file) | overwrite == T) {
    ## identify dates for hyspdisp averages and dates for files to read in
    vec_dates <-
      seq.Date(as.Date(start.date), as.Date(end.date), by = '1 day')

    vec_filedates <-
      seq.Date(
        from = as.Date(start.date) - ceiling(duration.run.hours / 24),
        to = as.Date(end.date),
        by = '1 day'
      )


    ## list the files
    pattern.file <-
      paste0(
        'hyspdisp_',
        gsub('[*]', '[*]', unitID),
        '_(',
        paste(vec_filedates, collapse = '|'),
        ')'
      )
    files.read <-
      list.files(path = hysp_dir,
        pattern = pattern.file,
        full.names = T)
    ## if hyo_dir2 povided, check for files there too

    ## read in the files
    l <- lapply(files.read, fread)


    ## Combine all parcels into single data table
    d <- rbindlist(l)

    if (length(d) == 0) {
      return(paste("No files available to link in", month_YYYYMM))
    }

    print(paste(Sys.time(), "Files read and combined"))

    ## Trim dates & first hour
    d <- d[d$Pdate %in% as(c(vec_dates), "character") &
        hour > 1, ]

    #Check if extent matches the hpbl raster
    d_xmin <- min(d$lon)

    e_xmin <- extent(pbl.height)[1]

    if (d_xmin < e_xmin - 5) {
      pbl.height <- rotate(pbl.height)
    }


    ## Trim PBL's
    d_trim <- trim_pbl(d, rasterin = pbl.height)
    print(paste(Sys.time(), "PBLs trimmed"))

    ## Link zips
    disp_df_link <-
      link_zip(
        d = d_trim,
        zc = zcta,
        cw = crosswalk.,
        gridfirst = T,
        rasterin = pbl.height
      )

    print(paste(Sys.time(), "ZIPs linked"))

    out <- disp_df_link[, .(ZIP, N)]
    out$ZIP <- formatC(out$ZIP,
      width = 5,
      format = "d",
      flag = "0")
    out$month <- month_YYYYMM
    out$unitID <- unitID

    if (nrow(out) != 0) {
      ## write to file
      write.csv(out, zip_output_file)
      print(paste(Sys.time(), "Linked ZIPs and saved to", zip_output_file))
    }
  } else {
    print(paste(
      "File",
      zip_output_file,
      "already exists! Use overwrite = TRUE to over write"
    ))
    out <- fread(zip_output_file)
    out$ZIP <- formatC(out$ZIP,
      width = 5,
      format = "d",
      flag = "0")
    out$month <- month_YYYYMM
    out$unitID <- unitID
  }

  out <- out[, .(ZIP, N, month, unitID)]
  return(out)
}
