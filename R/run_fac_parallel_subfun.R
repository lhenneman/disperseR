get_output_df <- function(model) {
  if (inherits(model, "traj_model")) {
    if (is.null(model$traj_df)) {
      return(NA)
    }

    if (!is.null(model$traj_df)) {
      return(dplyr::as.tbl(model$traj_df))
    }
  }

  if (inherits(model, "disp_model")) {
    if (is.null(model$disp_df)) {
      return(NA)
    }

    if (!is.null(model$disp_df)) {
      return(dplyr::as.tbl(model$disp_df))
    }
  }
}



dispersion_read <- function(archive_folder) {
  dispersion_file_list <-
    list.files(path = archive_folder,
      pattern = "^GIS_part_[0-9][0-9][0-9]_ps.csv",
      full.names = TRUE)

  # Get each CSV file into a single data frame
  for (i in 1:length(dispersion_file_list)) {
    if (i == 1) {
      dispersion <-
        as.data.frame(mat.or.vec(nr = 0, nc = 5))
      colnames(dispersion) <-
        c("particle_no", "lon", "lat",
          "height", "hour")
    }
    disp <-
      read.csv(dispersion_file_list[i], header = FALSE)
    colnames(disp) <-
      c("particle_no", "lon", "lat", "height")
    disp$hour <- i
    dispersion <- rbind(dispersion, disp)
  }

  # Return the data frame
  return(dispersion)
}


trim_zero <- function(Min) {
  M <- copy(Min)

  p_zero_df <- M[height == 0, ]
  particles <- unique(p_zero_df$particle_no)

  for (p in particles) {
    h_zero <- p_zero_df[particle_no == p, hour]
    M[particle_no == p & hour >= h_zero, ] <- NA
  }
  M <- na.omit(M)
  return(M)
}


add_params <- function(model,
  lat = NULL,
  lon = NULL,
  height = NULL,
  duration = NULL,
  run_period = NULL,
  start_day = NULL,
  start_hour = NULL,
  daily_hours = NULL,
  direction = NULL,
  met_type = NULL,
  vert_motion = NULL,
  model_height = NULL,
  traj_name = NULL,
  exec_dir = NULL,
  met_dir = NULL,
  binary_path = NULL) {
  if (!is.null(lat)) {
    model$lat <- lat
  }

  if (!is.null(lon)) {
    model$lon <- lon
  }

  if (!is.null(height)) {
    model$height <- height
  }

  if (!is.null(duration)) {
    model$duration <- duration
  }

  if (!is.null(run_period)) {
    model$run_period <- run_period
  }

  if (!is.null(start_day)) {
    model$start_day <- start_day
  }

  if (!is.null(start_hour)) {
    model$start_hour <- start_hour
  }

  if (!is.null(daily_hours)) {
    model$daily_hours <- daily_hours
  }

  if (!is.null(direction)) {
    model$direction <- direction
  }

  if (!is.null(met_type)) {
    model$met_type <- met_type
  }

  if (!is.null(vert_motion)) {
    model$vert_motion <- vert_motion
  }

  if (!is.null(model_height)) {
    model$model_height <- model_height
  }

  if (!is.null(traj_name)) {
    model$traj_name <- traj_name
  }

  if (!is.null(exec_dir)) {
    model$exec_dir <- exec_dir
  }

  if (!is.null(met_dir)) {
    model$met_dir <- met_dir
  }

  if (!is.null(binary_path)) {
    model$binary_path <- binary_path
  }

  return(model)
}

# Create the 'disp_model' list object
create_disp_model <- function(name = NULL) {
  # Create the 'disp_model' list object
  disp_model <-
    list(
      disp_name = NULL,
      lat = NULL,
      lon = NULL,
      height = NULL,
      duration = NULL,
      start_day = NULL,
      start_hour = NULL,
      direction = "forward",
      met_type = NULL,
      emissions = NULL,
      species = NULL,
      grids = NULL,
      vert_motion = 0,
      model_height = 20000,
      disp_df = NULL
    )

  attr(disp_model, "class") <- "disp_model"

  if (!is.null(name))
    disp_model$disp_name <- name

  return(disp_model)
}




add_species <- function(model,
  name = NULL,
  pdiam = NULL,
  density = NULL,
  shape_factor = NULL,
  ddep_vel = NULL,
  ddep_mw = NULL,
  ddep_a_ratio = NULL,
  ddep_d_ratio = NULL,
  ddep_hl_coeff = NULL,
  wdep_hl_coeff = NULL,
  wdep_in_cloud = NULL,
  wdep_below_cloud = NULL,
  rad_decay = NULL,
  resuspension = NULL) {
  if (is.null(name)) {
    if (is.null(model$species)) {
      name <- "species_1"
    } else {
      name <- paste0("species_",
        nrow(model$species) + 1)
    }
  }

  if (is.null(pdiam)) {
    pdiam <- 15.0
  }

  if (is.null(density)) {
    density <- 1.0
  }

  if (is.null(shape_factor)) {
    shape_factor <- 1.0
  }

  if (is.null(ddep_vel)) {
    ddep_vel <- 0.0
  }

  if (is.null(ddep_mw)) {
    ddep_mw <- 0.0
  }

  if (is.null(ddep_a_ratio)) {
    ddep_a_ratio <- 0.0
  }

  if (is.null(ddep_d_ratio)) {
    ddep_d_ratio <- 0.0
  }

  if (is.null(ddep_hl_coeff)) {
    ddep_hl_coeff <- 0.0
  }

  if (is.null(wdep_hl_coeff)) {
    wdep_hl_coeff <- 0.0
  }

  if (is.null(wdep_in_cloud)) {
    wdep_in_cloud <- 0.0
  }

  if (is.null(wdep_below_cloud)) {
    wdep_below_cloud <- 0.0
  }

  if (is.null(rad_decay)) {
    rad_decay <- 0.0
  }

  if (is.null(resuspension)) {
    resuspension <- 0.0
  }

  # Write species parameters to a data frame
  species <-
    data.frame(
      name = name,
      pdiam = pdiam,
      density = density,
      shape_factor = shape_factor,
      ddep_vel = ddep_vel,
      ddep_mw = ddep_mw,
      ddep_a_ratio = ddep_a_ratio,
      ddep_d_ratio = ddep_d_ratio,
      ddep_hl_coeff = ddep_hl_coeff,
      wdep_hl_coeff = wdep_hl_coeff,
      wdep_in_cloud = wdep_in_cloud,
      wdep_below_cloud = wdep_below_cloud,
      rad_decay = rad_decay,
      resuspension = resuspension,
      stringsAsFactors = FALSE
    )

  # Write data frame to the `species` list
  # component of `model`
  if (is.null(model$species)) {
    model$species <- species
  } else {
    model$species <-
      rbind(model$species, species)
  }

  return(model)
}



add_emissions <- function(model,
  rate = NULL,
  duration = NULL,
  start_day = NULL,
  start_hour = NULL,
  name = NULL) {
  if (is.null(name)) {
    if (is.null(model$emissions)) {
      name <- "emissions_1"
    } else {
      name <- paste0("emissions_",
        nrow(model$emissions) + 1)
    }
  }

  if (is.null(rate)) {
    rate <- 1
  }

  if (is.null(duration)) {
    duration <- 1
  }

  if (is.null(start_day)) {
    start_day <- "10-05-01"
  }

  if (is.null(start_hour)) {
    start_hour <- 0
  }


  # Write emissions parameters to a data frame
  emissions <-
    data.frame(
      name = name,
      rate = rate,
      duration = duration,
      start_day = start_day,
      start_hour = start_hour,
      stringsAsFactors = FALSE
    )

  # Write data frame to the `emissions` list
  # component of `model`
  if (is.null(model$emissions)) {
    model$emissions <- emissions
  } else {
    model$emissions <-
      rbind(model$emissions, emissions)
  }

  return(model)
}


## function to negatie
'%ni%' <- function(x, y) {
  return ! ('%in%'(x, y))
}


