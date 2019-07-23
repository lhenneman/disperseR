create_dirs <- function(location=file.path('~', 'Desktop')) {

  message("Creating project setup")

  ## Main - Project Folder
  main_dir <- file.path(location, 'main')
  assign("main_dir", main_dir, envir = .GlobalEnv)
  dir.create(main_dir, showWarnings = FALSE)

  ###### Input
  input_dir <- file.path(main_dir, 'input')
  assign("input_dir", input_dir, envir = .GlobalEnv)
  dir.create(input_dir, showWarnings = FALSE)

  ########### ZCTA
  zcta_dir <- file.path(input_dir, 'zcta_500k')
  assign("zcta_dir", zcta_dir, envir = .GlobalEnv)
  dir.create(zcta_dir, showWarnings = FALSE)

  ########### HPBL
  hpbl_dir <- file.path(input_dir, 'hpbl')
  assign("hpbl_dir", hpbl_dir, envir = .GlobalEnv)
  dir.create(hpbl_dir, showWarnings = FALSE)

  ########### Meteo
  meteo_dir <- file.path(input_dir, 'meteo')
  assign("meteo_dir", meteo_dir, envir = .GlobalEnv)
  dir.create(meteo_dir, showWarnings = FALSE)

  ###### Process
  proc_dir <- file.path(main_dir, 'process')
  assign("proc_dir", proc_dir, envir = .GlobalEnv)
  dir.create(proc_dir, showWarnings = FALSE)

  ###### Output
  output_dir <- file.path(main_dir, 'output')
  assign("output_dir", output_dir, envir = .GlobalEnv)
  dir.create(output_dir, showWarnings = FALSE)

  ########### Hysplit
  hysp_dir <- file.path(output_dir, 'hysplit')
  assign("hysp_dir", hysp_dir, envir = .GlobalEnv)
  dir.create(hysp_dir, showWarnings = FALSE)

  ########### Ziplinks
  ziplink_dir <- file.path(output_dir, 'ziplinks')
  assign("ziplink_dir", ziplink_dir, envir = .GlobalEnv)
  dir.create(ziplink_dir, showWarnings = FALSE)

  ########### Exposure
  exp_dir <- file.path(output_dir, 'exp')
  assign("exp_dir", exp_dir, envir = .GlobalEnv)
  dir.create(exp_dir, showWarnings = FALSE)
  exp_dir

  ########### Rdata
  rdata_dir <- file.path(output_dir, 'rdata')
  assign("rdata_dir", rdata_dir, envir = .GlobalEnv)
  dir.create(rdata_dir, showWarnings = FALSE)

  message(paste("Project created in : ", location))

}
