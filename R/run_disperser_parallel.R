#' run the fac model in parallel
#'
#' \code{run_disperser_parallel}
#'
#' It is possible that running the below code with output a warning "WARNING: map background file not found ../graphics/arlmap". It is safe to ignore it.
#'
#'
#' @param input.refs A data.table with the following columns (and column classes): ID (character) a unique ID assigned to the particles emmission source. uID (character) same as ID. Latitude (numeric) and Longitude (numeric) the emmission source's latitude and longitude. Height (numeric) the emmission source's height in meters above ground level. start_day (Date) and start_hour (numeric) the date and time when the emissions occurrence begins. duration_emiss_hours (numeric) the length in hours of the emission event. duration_run_hours (numeric) the length in hours of the dispersion simulation. Be mindful of the default value of the parameter stringsAsFactors when using the function data.table() to convert a data.frame to a data.table. If disperseR::units data is used, then input.refs can take the data.table that is the result of the `define_input()` function or a subset of that dataset. 
#'
#' @param pbl.height Monthly mean planetary boundary layer heights. See vignettes for more information
#'
#' @param species The package has the possibility to use two types of species. The default one is `species = 'so2'`, but you can also use particulate sulfate `species = 'so4p'`.
#'
#' @param proc_dir directory where the function saves temporary files while running. This is automatically defined by `create_dirs()`
#'
#' @param overwrite if output files already exist should they be overwritten? This is `false` by default.
#'
#' @param npart number of air parcels tracked by HYSPLIT. Defaults to 100
#'
#' @param mc.cores on how many cores should R split the computations. set to  parallel::detectCores() or set to 1 if you want to serial computation.
#'
#' @param keep.hysplit.files logical. If FALSE (the default), clears storage space in the `proc_dir` by removing all HYSPLIT files
#'
#'
#' @return This function returns fac model results.


#' @export run_disperser_parallel

run_disperser_parallel <- function(input.refs = NULL,
  pbl.height = NULL,
  species = 'so2',
  proc_dir = proc_dir,
  overwrite = F,
  npart = 100,
  mc.cores = parallel::detectCores(),
  keep.hysplit.files = FALSE){

  ## run_fac() below assums that there is one year data.
    run_sample <- seq(1, nrow(input.refs))

    ## run the run_fac in parallel
      parallel::mclapply(X = run_sample,
      FUN = run_fac,
      input.refs = input.refs,
      pbl.height = pbl.height,
      species =   species,
      proc_dir = proc_dir,
      overwrite = overwrite,
      npart =  npart,
      keep.hysplit.files = keep.hysplit.files,
      mc.cores = mc.cores)
  }


run_fac <- function(x,
  input.refs = input.refs,
  pbl.height = pbl.height,
  species = species,
  npart = npart,
  overwrite = overwrite,
  keep.hysplit.files,
  proc_dir = proc_dir) {

  subset <- input.refs[x]
  print(subset)

  ## function to negate
  '%ni%' <- function(x, y) {
    return(!('%in%'(x, y)))
  }

  #########################################################################################################
  ## define speciec params depening on species.

  if (species == 'so2') {
    species_param <-
      data.table(
        name = 'so2',
        pdiam = 0,
        density = 0,
        shape_factor = 0,
        resuspension = 1e-10,
        ddep_vel = 0.002)
  } else if (species == 'so4') {
    # so4p (particulate sulfate)
    species_param <-
      data.table(
        name = 'so4p',
        pdiam = 2.5,
        density = 1,
        shape_factor = 1,
        resuspension = 0,
        ddep_vel = 0.002)
  } else {
    stop("No species or incorrect species defined!")
  }

  #########################################################################################################
  ## subset the data using the indexes provided.
  print(paste0(
    'Date: ',
    format(subset$start_day, format = "%Y-%m-%d"),
    ', Hour: ',
    subset$start_hour
  ))

  if (is.na(subset$Height)) {
    stop("Check to make sure your Height is defined in the run_ref_tab!")
  }

  #########################################################################################################
  ## Check if Height parameter in unit is NA

  # create sharded directory structure
  hysp_dir_yr <- file.path( hysp_dir, subset$year)
  hysp_dir_mo <- file.path( hysp_dir_yr,
                            formatC(
                              month( subset$start_day),
                              width = 2, flag = '0'))
  dir.create( hysp_dir_mo, showWarnings = TRUE, recursive = TRUE)

  ## Define output file names
  output_file <- path.expand(file.path(
    hysp_dir_mo,
    paste0(
      "hyspdisp_",
      subset$ID,
      "_",
      subset$start_day,
      "_",
      formatC(
        subset$start_hour,
        width = 2,
        format = "d",
        flag = "0"
      ),
      ".fst"
    )
  ))
  message(paste("output file", output_file))


  ## Initial output data.table
  out <-
    paste(
      "Partial trimmed parcel locations (below height 0 and the highest PBL height) already exist at",
      output_file
    )

  ## Check if output parcel locations file already exists
  tmp.exists <- file.exists( file.path(output_file))

  if (!tmp.exists | overwrite == TRUE) {
    message("Defining HYSPLIT model parameters and running the model.")

    ## Create run directory
    run_dir <- file.path(proc_dir, paste0(subset$ID, '_', paste(subset[, .(ID, start_day, start_hour)], collapse = '_')))

    ## preemptively remove if run_dir already exists, then create
    unlink(run_dir, recursive = TRUE)
    dir.create(run_dir, showWarnings = FALSE)

    ## Define the dispersion model
    dispersion_model <-
      disperseR::create_disp_model() %>%
      disperseR::add_emissions(
        rate = 1,
        duration = subset$duration_emiss_hours,
        start_day = as(subset$start_day, 'character'),
        start_hour = subset$start_hour
      ) %>%
      disperseR::add_species(
        name = species_param$name,
        pdiam = species_param$pdiam,
        density = 0,
        shape_factor = 0,
        ddep_vel = species_param$ddep_vel
      ) %>%
      disperseR::add_grid(range = c(0.5, 0.5),
        division = c(0.1, 0.1)) %>%
      disperseR::add_params(
        lat = subset$Latitude,
        lon = subset$Longitude,
        height = subset$Height,
        duration = subset$duration_run_hours,
        start_day = as(subset$start_day, 'character'),
        start_hour = subset$start_hour,
        direction = "forward",
        met_type = "reanalysis",
        met_dir = meteo_dir
      ) %>%
      disperseR::run_model(npart = npart, run.dir = run_dir)


    ## Extract output from the dispersion model
    dispersion_df <- dispersion_model %>% get_output_df() %>% data.table()

    ## trim particles if they go below zero
    disp_df <- trim_zero(dispersion_df)

    ## Add parcel date and time
    disp_df$Pdate <- subset$start_day + disp_df$hour / 24

    # trims particles that are above the global max boundary value
    disp_df_trim <- disp_df[height <= 2665]

    ## Save R data frame
    save.vars <- c('lon', 'lat', 'height', 'Pdate', 'hour')
    partial_trimmed_parcel_locs <-
      disp_df_trim[, save.vars, with = FALSE]
    write.fst(partial_trimmed_parcel_locs, output_file)
    out <-
      paste(
        "Partial trimmed parcel locations (below height 0 and the highest PBL height) written to",
        output_file
      )

    ## Erase run files
    if (!keep.hysplit.files)
      unlink(run_dir, recursive = TRUE)
  }


  return(out)
}

