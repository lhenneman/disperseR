#' create a set of directories to run disperseR
#'
#' \code{get_data}
#'
#' @description `get_data()` helps the user get data for necessary for function execution.
#'
#'
#' @param data `data = "all"` will download all the data that can be downloaded with the function. It has to be used with `start.year`, `start.month`, `end.year` and `end.month`. Other possible options are: `data = "zctashapefile"`, `data = "pblheight"`, `data = "metfiles"` and `data = "zcta_dataset"`.
#'
#' @param start.year specify what year of metfiles data you would like to start with. Please supply it like as a string for example "2005".
#' @param end.year specify what year of metfiles data you would like to end with. Please supply it like as a string for example "2005".
#' @param start.month what month of `start.year` you would like to start the download with? Please supply a string for example "07" for July.
#' @param end.month what month of `end.year` you would like to end the download with? Please supply a string for example "07" for July.
#' @return Creates directories (does not overwrite if existing). Outputs string variables with paths to the environment.


#' @export get_data
#' @export download_file


## This function is used below in get_data().

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
    end.month = NULL) {



    ####################
    # to avoid error of unused arguments we used them independently of data requested
    startyear <- start.year
    startmonth <- start.month
    endyear <- end.year
    endmonth <- end.month

    if (data == "all") {

      ### Crosswalk
      message("Reading in the crosswalk data from disperseR")
      crosswalk <- disperseR::crosswalk
      assign("crosswalk", crosswalk, envir = .GlobalEnv)
      message("   Assigned to crosswalk variable")

      ### PP.units.monthly1995_2017
      message("Reading in the PP.units.monthly1995_2017 data from disperseR")
      PP.units.monthly1995_2017 <- disperseR::PP.units.monthly1995_2017
      assign("PP.units.monthly1995_2017", PP.units.monthly1995_2017, envir = .GlobalEnv)
      message("   Assigned to PP.units.monthly1995_2017 variable")

      ### PP.units.monthly1995_2017

      message("Reading in the zipcodecoordinate data from disperseR")
      zipcodecoordinate <- disperseR::zipcodecoordinate
      assign("zipcodecoordinate", zipcodecoordinate, envir = .GlobalEnv)
      message("   Assigned to zipcodecoordinate variable")

      ### ALL DATA
      ### ZCTA shape files
      message("Start getting the ZCTA file")

      directory <- zcta_dir
      file <- file.path(directory, 'cb_2017_us_zcta510_500k.zip')
      url <-
        'ftp://ftp2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_zcta510_500k.zip'

      if (!file.exists(file)) {
        message(
          "   Downloading the ZCTA shapefile.
          This might take a bit, but sometimes the url will fail to connect, in this case you might want to try again later"
        )
        download_file(url, file, directory)
      }
      else{
        message("   File already exist, not downloading.")
      }

      zcta.shp <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
      if (!file.exists(zcta.shp)) {
        message("   Unzipping ZCTA file.")
        unzip( file, exdir = zcta_dir)
      }

      message("   Starting Preprocessing")
      zcta <- raster::shapefile(x = zcta.shp)
      # It is recommended to transform the ZCTA shapefile to a known projection to maintain consistency throughout the allocation process.
      # Lat-lon projections are preferred, such as the [North American Albers Equal Area Conic](https://epsg.io/102008):
      p4s <-
        "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
      zcta.trans <- sp::spTransform(x = zcta, CRSobj = p4s)
      message("   Preprocessing complete")
      assign("zcta", zcta.trans, envir = .GlobalEnv)
      message("   Assigned to zcta variable")

      ### pblheight

      ## planetary layers data
      message("Start getting the planetary layers data")
      directory <- hpbl_dir
      file <- file.path(directory, 'hpbl.mon.mean.nc')
      url <-
        'https://www.esrl.noaa.gov/psd/repository/entry/get/hpbl.mon.mean.nc?entryid=synth%3Ae570c8f9-ec09-4e89-93b4-babd5651e7a9%3AL05BUlIvTW9udGhsaWVzL21vbm9sZXZlbC9ocGJsLm1vbi5tZWFuLm5j'

      if (!file.exists(file)) {
        message(
          "   Downloading Planetary Layers Data.
          This might take a bit, but sometimes the url will fail to connect, in this case you might want to try again later"
        )
        download_file(url, file, directory)
      } else{
        message("   File already exist, not downloading.")
      }

      # Before reading in, it is necessary to set the system time zone to UTC so that the dates are formatted correctly in the raster files.
      Sys.setenv(TZ = 'UTC')
      hpbl_rasterin <-
        suppressWarnings(raster::brick(x = file, varname = 'hpbl'))
      # The following is done to fix error in the dataset. For more information please see
      raster::crs(hpbl_rasterin) <-
        "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
      message("   Preprocessing complete")
      assign("pblheight", hpbl_rasterin, envir = .GlobalEnv)
      message("   Assigned to  pblheight variable")

      ###  metfiles

      message("Start getting the metfiles data")
      if (is.null(startyear) |
          is.null(startmonth) |
          is.null(endyear) | is.null(endmonth)) {
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
          string <-
            paste("RP", format(start, "%Y%m"), ".gbl", sep = "")
          vectorfiles[i] <- string
          lubridate::month(start) <- lubridate::month(start) + 1
          i = i + 1
        }
        metfiles <-
          vectorfiles[!(vectorfiles %in% list.files(meteo_dir))]

        if (length(metfiles) > 0) {
          message("   Downloading the following files:")
          message(metfiles)
          disperseR::get_met_reanalysis(files = metfiles, path_met_files = meteo_dir)
        }
        if (length(metfiles) == 0) {
          message("   No files to download. Requested files already available")
        }

        ### ZCTA dataset
        directory <- zcta_dir

        message("Start getting the ZCTA dataset for graphs")

        message("   Starting Preprocessing")
        zcta <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
        zcta <- sf::st_read(zcta)
        data.table::setnames(zcta, 'ZCTA5CE10', 'ZCTA')
        zcta <-
          merge(
            zcta,
            disperseR::crosswalk,
            by = "ZCTA",
            all = F,
            allow.cartesian = TRUE
          )
        message("   Preprocessing complete")
        assign("zcta_dataset", zcta, envir = .GlobalEnv)
        message("   Assigned to zcta_dataset variable")


    }




    if (data == "zctashapefile" | data == "zcta_dataset") {
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
          is.null(startmonth) |
          is.null(endyear) | is.null(endmonth)) {
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
        string <-
          paste("RP", format(start, "%Y%m"), ".gbl", sep = "")
        vectorfiles[i] <- string
        lubridate::month(start) <- lubridate::month(start) + 1
        i = i + 1
      }
      metfiles <-
        vectorfiles[!(vectorfiles %in% list.files(meteo_dir))]

      if (length(metfiles) > 0) {
        message("Downloading the following files:")
        message(metfiles)
        disperseR::get_met_reanalysis(files = metfiles, path_met_files = meteo_dir)
      }
      if (length(metfiles) == 0) {
        message("No files to download. Requested files already available")
      }
    }

    if (data != "metfiles" & data != "all") {
      if (!file.exists(file)) {
        message(
          "This might take a bit, but sometimes the url will fail to connect, in this case you might want to try again later"
        )
        download_file(url, file, directory)
      } else{
        message("File already exist, not downloading.")
      }
    }

    if (data == "zcta_dataset") {
      message("Starting Preprocessing")
      zcta <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
      zcta <- sf::st_read(zcta)
      data.table::setnames(zcta, 'ZCTA5CE10', 'ZCTA')
      zcta <-
        merge(
          zcta,
          disperseR::crosswalk,
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
      p4s <-
        "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
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
      raster::crs(hpbl_rasterin) <-
        "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
      message("Preprocessing complete")
      return(hpbl_rasterin)
    }
  }

