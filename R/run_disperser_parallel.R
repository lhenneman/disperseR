#' run the fac model in parallel
#'
#' \code{run_disperser_parallel} takes the following arguments
#'
#' It is possible that running the below code with output a warning "WARNING: map background file not found ../graphics/arlmap". It is safe to ignore it.
#'
#'
#' @param input.refs should be the data table that is the result of the `define_input()` function or a subset of that dataset
#'
#' @param pbl.height Monthly mean planetary boundary layer heights. See vignettes for more information
#'
#' @param crosswalk. Crosswalk ZCTA-to-ZIP. See vignettes for more information
#'
#' @param zcta. ZCTA shape file. See vignette for more information
#'
#' @param species The package has the possibility to use two types of species. The default one is `species = 'so2'`, but you can also use particulate sulfate `species = 'so4p'`.
#'
#' @param link2zip
#'
#' @param proc_dir directory where the function saves temporary files while running. This is automatically defined by `create_dirs()`
#'
#' @param overwrite if output files already exist should they be overwritten? This is `false` by default.
#'
#' @param npart
#'
#' @param mc.cores on how many cores should R split the computations. set to  parallel::detectCores() or set to 1 if you want to serial computation.
#'
#' @param keep.hysplit.files
#'
#'
#' @return This function returns fac model results.


#' @export run_disperser_parallel

run_disperser_parallel <- function(input.refs = NULL,
  pbl.height = NULL,
  crosswalk.= NULL,
  zcta. = NULL,
  species = 'so2',
  link2zip = F,
  proc_dir = proc_dir,
  overwrite = F,
  npart = NULL,
  mc.cores = parallel::detectCores(),
  keep.hysplit.files = FALSE){

  ## run_fac() below assums that there is one year data.
    run_sample <- seq(1, nrow(input.refs))

    ## run the run_fac in parallel
      parallel::mclapply(X = run_sample,
      FUN = run_fac,
      input.refs = input.refs,
      pbl.height = pbl.height,
      crosswalk.= crosswalk.,
      zcta. = zcta.,
      species =   species,
      link2zip = link2zip,
      proc_dir = proc_dir,
      overwrite = overwrite,
      npart =  npart,
      keep.hysplit.files,
      mc.cores = mc.cores)
  }


run_fac <- function(x,
  input.refs = input.refs,
  crosswalk. = crosswalk,
  pbl.height = pbl.height,
  zcta. = zcta.,
  species = species,
  npart = npart,
  overwrite = overwrite,
  link2zip = link2zip,
  keep.hysplit.files,
  proc_dir = proc_dir) {

  subset <- input.refs[x]
  print(subset)

  zcta <- zcta.
  crosswalk <- crosswalk.

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

  ## Define output file names
  output_file <- path.expand(file.path(
    hysp_dir,
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
      ".csv"
    )
  ))
  message(paste("output file", output_file))

  zip_output_file <- file.path(
    ziplink_dir,
    paste0(
      "single_ziplink_",
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
      ".csv"
    )
  )

  ## Initial output data.table
  out1 <-
    paste(
      "Partial trimmed parcel locations (below height 0 and the highest PBL height) already exist at",
      output_file
    )
  out2 <-
    paste("ZIP code parcel counts not called for or already exist at",
      zip_output_file)

  ## Check if output parcel locations file already exists
  tmp.exists <- system(paste("ls -f", file.path(output_file)), intern = TRUE)

  if (output_file %ni% tmp.exists | overwrite == TRUE) {
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
        # okay
        density = 0,
        # okay
        shape_factor = 0,
        # okay
        #resuspension = species_param$resuspension
        ddep_vel = species_param$ddep_vel
      ) %>% # okay
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
        met_type = "reanalysis"
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
    write.csv(partial_trimmed_parcel_locs, output_file)
    out1 <-
      paste(
        "Partial trimmed parcel locations (below height 0 and the highest PBL height) written to",
        output_file
      )

    ## Erase run files
    if (!keep.hysplit.files)
      unlink(run_dir, recursive = TRUE)
  }

  if (link2zip == TRUE) {
    print("Linking parcel locations to ZIP codes. This could take a few minutes...")

    # Check if pbl.height is defined
    if (!hasArg(pbl.height))
      stop("Please define a pbl.height file")

    # Check if crosswalk is defined
    if (!hasArg(crosswalk.))
      stop("Please define a crosswalk file to link zips")

    #Read output file from hysplit
    disp_df <- fread(output_file)

    #Check if extent matches the hpbl raster (pbl.height)
    d_xmin <- min(disp_df$lon)
    e_xmin <- extent(pbl.height)[1]
    if (d_xmin < e_xmin) {
      pbl.height <- rotate(pbl.height)
    }

    ## trim values above PBL
    disp_df_trim <- trim_pbl(disp_df, rasterin = pbl.height)

    ## link to zips
    disp_df_link <- link_zip(
      disp_df_trim,
      zc = zcta.,
      cw = crosswalk,
      gridfirst = TRUE,
      rasterin = pbl.height
    )

    # Write to output csv file
    write.csv(disp_df_link[, .(ZIP, N)], zip_output_file)
    out2 <- paste("ZIP code parcel counts written to", zip_output_file)
  }
  out <- data.table(out = c(out1, out2))
  return(out)
}

