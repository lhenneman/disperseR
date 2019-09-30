#' @export get_met_reanalysis
get_met_reanalysis <- function(files = NULL,
  years = NULL,
  path_met_files) {

  reanalysis_dir <- "ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/"

  # Download list of reanalysis met files by name
  if (!is.null(files)) {

    for (i in 1:length(files)) {

      if (.Platform$OS.type == "windows") {
        download(
          url = paste0(reanalysis_dir,
            files[i]),
          destfile = file.path(path_met_files,
            files[i]),
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE)
      }

      if (.Platform$OS.type == "unix") {
        download.file(
          url = paste0(reanalysis_dir,
            files[i]),
          destfile = file.path(path_met_files,
            files[i]),
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE)
      }
    }
  }

  # Download one or more years of reanalysis met files
  if (!is.null(years)) {
    for (i in 1:length(years)) {
      for (j in 1:12) {
        if (.Platform$OS.type == "unix") {
          download.file(
            url = paste0(reanalysis_dir,
              "RP",
              years[i],
              formatC(j, width = 2,
                format = "d",
                flag = "0"),
              ".gbl"),
            destfile = file.path(path_met_files,
              paste0( "RP",
                years[i],
                formatC(j, width = 2,
                  format = "d",
                  flag = "0"),
                ".gbl")),
            method = "auto",
            quiet = FALSE,
            mode = "wb",
            cacheOK = FALSE)
        }

        if (.Platform$OS.type == "windows") {
          download.file(
            url = paste0(reanalysis_dir,
              "RP",
              years[i],
              formatC(j, width = 2,
                format = "d",
                flag = "0"),
              ".gbl"),
            destfile = file.path(path_met_files,
              paste0( "RP",
                years[i],
                formatC(j, width = 2,
                  format = "d",
                  flag = "0"),
                ".gbl")),
            method = "auto",
            quiet = FALSE,
            mode = "wb",
            cacheOK = FALSE)
        }
      }
    }
  }
}
