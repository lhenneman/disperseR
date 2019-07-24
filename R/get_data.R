

##########################################################################################
##########################################################################################

## This function is used in get_data
download_file <- function(url, file, dir) {
  out <- tryCatch({
    download.file(url = url, destfile = file)
    if (substr(file, nchar(file) - 3 + 1, nchar(file)) == "zip") {
      # if a zip file unzip it.
      unzip(file, exdir = dir)
    }
  },
    error = function(cond) {
      message(
        paste(
          "Sometimes this URL does not connect properly. In this case, you can try again later"
        )
      )
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning = function(cond) {
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally = {

    })
  return(out)
}


get_data <-
  function(data,
    start.year = NULL,
    start.month = NULL,
    end.year = NULL,
    end.month = NULL,
    unit = NULL,
    linked.zips.data = NULL,
    linked.zips.data.position = NULL) {

    # to avoid error of unused arguments we used them independently of data requested
    startyear <- start.year
    startmonth <- start.month
    endyear <- end.year
    endmonth <- end.month
    linked_zips_data <- linked.zips.data
    linked_zips_data_position <- linked.zips.data.position

    if (data == "zctashapefile" | data == "zcta_dataplot") {
      directory <- zcta_dir
      file <- file.path(directory, 'cb_2017_us_zcta510_500k.zip')
      url <-
        'ftp://ftp2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_zcta510_500k.zip'
    }

    if (data == "pblheight") {
      ## planetary layers data
      directory <- hpbl_dir
      file <- file.path(directory, 'hpbl.mon.mean.nc')
      url <-
        'https://www.esrl.noaa.gov/psd/repository/entry/get/hpbl.mon.mean.nc?entryid=synth%3Ae570c8f9-ec09-4e89-93b4-babd5651e7a9%3AL05BUlIvTW9udGhsaWVzL21vbm9sZXZlbC9ocGJsLm1vbi5tZWFuLm5j'
    }

    # download meteo files
    if (data == "metfiles") {
      if (is.null(startyear) |
          is.null(startmonth) | is.null(endyear) | is.null(endmonth)) {
        stop("Please specify the metfiles dates correctly. Please refer to documentation")
      }
      inputdates <-
        c(
          paste(startyear, startmonth, "01", sep = "/"),
          paste(endyear, endmonth, "01", sep = "/")
        )
      start <- as.Date(inputdates[1])
      end <- as.Date(inputdates[2])

      vectorfiles <- NULL
      i = 1
      while (start <= end) {
        ## process strings for download information
        string <- paste("RP", format(start, "%Y%m"), ".gbl", sep = "")
        vectorfiles[i] <- string
        lubridate::month(start) <- lubridate::month(start) + 1
        i = i + 1
      }
      metfiles <-
        vectorfiles[!(vectorfiles %in% list.files(meteo_dir))]

      if (length(metfiles) > 0) {
        message("Downloading the following files:")
        message(metfiles)
        get_met_reanalysis(files = metfiles, path_met_files = meteo_dir)
      }
      if (length(metfiles) == 0) {
        message("No files to download")
      }
    }

    if (data != "metfiles") {
      if (!file.exists(file)) {
        message(
          "This might take a bit, but sometimes the url will fail to connect, in this case you might want to try again later"
        )
        download_file(url, file, directory)
      } else{
        message("File already exist, not downloading.")
      }
    }

    if (data == "zcta_dataplot") {
      message("Starting Preprocessing")
      zcta <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
      zcta <- st_read(zcta)
      setnames(zcta, 'ZCTA5CE10', 'ZCTA')
      zcta <-
        merge(
          zcta,
          hyspdisp2::crosswalk,
          by = "ZCTA",
          all = F,
          allow.cartesian = TRUE
        )
      return(zcta)
    }

    if (data == "zctashapefile") {
      message("Starting Preprocessing")
      zcta <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
      zcta <- raster::shapefile(x = zcta)
      # It is recommended to transform the ZCTA shapefile to a known projection to maintain consistency throughout the allocation process.
      # Lat-lon projections are preferred, such as the [North American Albers Equal Area Conic](https://epsg.io/102008):
      p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
      zcta.trans <- sp::spTransform(x = zcta, CRSobj = p4s)
      message("Preprocessing complete")
      return(zcta.trans)
    }

    if (data == "pblheight") {
      #Before reading in, it is necessary to set the system time zone to UTC so that the dates are formatted correctly in the raster files.
      Sys.setenv(TZ = 'UTC')
      hpbl_rasterin <-
        suppressWarnings(raster::brick(x = file, varname = 'hpbl'))
      # The following is done to fix error in the dataset. For more information please see
      # https://stackoverflow.com/questions/56806894/raster-warning-message-in-cbindmi-vals-number-of-rows-of-result-is-not/56807318#56807318
      crs(hpbl_rasterin) <- "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
      message("Preprocessing complete")
      return(hpbl_rasterin)
    }
  }

