#' Read a `listing` file
#' @param file_path The path to the \code{listing} file.
#' @noRd
#' @export read_listing_file
read_listing_file <- function(file_path) {
  as.vector(
    read.table(
      file = file_path,
      sep = "\n"
    )[, 1]
  )
}

#' Create default `SETUP.CFG` and `ASCDATA.CFG` files
#' @param dir The directory to which the files should be written.
#' @noRd
#' @export hysplit_config_init
hysplit_config_init <- function(dir) {
  # Default `SETUP.CFG` configuration file
  cat(
    " &SETUP", " tratio = 0.75,", " initd = 0,", " kpuff = 0,", " khmax = 9999,",
    " kmixd = 0,", " kmix0 = 250,", " kzmix = 0,", " kdef = 0,", " kbls = 1,",
    " kblt = 2,", " conage = 48,", " numpar = 2500,", " qcycle = 0.0,", " efile = '',",
    " tkerd = 0.18,", " tkern = 0.18,", " ninit = 1,", " ndump = 1,", " ncycl = 1,",
    " pinpf = 'PARINIT',", " poutf = 'PARDUMP',", " mgmin = 10,", " kmsl = 0,",
    " maxpar = 10000,", " cpack = 1,", " cmass = 0,", " dxf = 1.0,", " dyf = 1.0,",
    " dzf = 0.01,", " ichem = 0,", " maxdim = 1,", " kspl = 1,", " krnd = 6,",
    " frhs = 1.0,", " frvs = 0.01,", " frts = 0.10,", " frhmax = 3.0,", " splitf = 1.0,",
    " tm_pres = 0,", " tm_tpot = 0,", " tm_tamb = 0,", " tm_rain = 0,", " tm_mixd = 0,",
    " tm_relh = 0,", " tm_sphu = 0,", " tm_mixr = 0,", " tm_dswf = 0,", " tm_terr = 0,",
    " /",
    sep = "\n",
    file = paste0(dir, "/", "SETUP.CFG")
  )
  # Default `ASCDATA.CFG` file
  cat(
    "-90.0  -180.0  lat/lon of lower left corner (last record in file)",
    "1.0  1.0	lat/lon spacing in degrees between data points",
    "180  360	lat/lon number of data points",
    "2  	default land use category",
    "0.2  	default roughness length (meters)",
    "'.'  directory location of data files",
    sep = "\n",
    file = paste0(dir, "/", "ASCDATA.CFG")
  )
}

#' @export get_os
get_os <- function() {
  if (.Platform$OS.type == "windows") {
    return("win")
  } else if (Sys.info()["sysname"] == "Darwin") {
    return("mac")
  } else if (.Platform$OS.type == "unix") {
    return("unix")
  } else {
    stop("Unknown OS", call. = FALSE)
  }
}

#' @export dispersion_read
dispersion_read <- function(archive_folder) {
  dispersion_file_list <-
    list.files(
      path = archive_folder,
      pattern = "^GIS_part_[0-9][0-9][0-9]_ps.csv",
      full.names = TRUE)

  # Get each CSV file into a single data frame
  for (i in 1:length(dispersion_file_list)) {
    if (i == 1) {
      dispersion <- as.data.frame(mat.or.vec(nr = 0, nc = 5))
      colnames(dispersion) <- c("particle_no", "lon", "lat", "height", "hour")
    }
    disp <- read.csv(dispersion_file_list[i], header = FALSE)
    colnames(disp) <-
      c("particle_no", "lon", "lat", "height")
    disp$hour <- i
    dispersion <- rbind(dispersion, disp)
  }
  # Return the data frame
  return(dispersion)
}

