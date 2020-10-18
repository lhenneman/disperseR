#' \code{plot_impact_weighted}
#'
#'
#' @description `plot_impact_weighted()` takes output from `calculate_exposure()` and creates a spatial plot
#'
#'
#' @param data.linked data for plotting as output from disperseR::calculate_exposure()
#'
#' @param data.units information on unit locations as output from disperseR::units()
#'
#' @param link.to spatial scale for plotting. One of 'zips', 'counties', or 'grids' that should match original input to disperseR::calculate_exposure()
#'
#' @param zcta.dataset ZIP code/ZCTA sptial dataset as imported by disperseR::get_data() or subset thereof. Required only if link.to = 'zips'.
#'
#' @param counties. US counties dataset as imported using USAboundaries::us_counties( ) or subset thereof. Required only if link.to = 'counties'.
#'
#' @param metric plotting metric in `data.linked` input. Defaults to 'hyads', the default output of disperseR::calculate_exposure().
#'
#' @param time.agg time aggregation matching original input to disperseR::calculate_exposure(). Can be 'month' or 'year'
#'
#' @param legend.lims legend limits string. Defaults to NULL, which creates a scale from 0 to the 95th percentile of the 'metric'. Should be in the form c(lower,upper).
#'
#' @param legend.name legend name string. Defaults to NULL, which creates a legend title of 'Aggregate HyADS Exposure'.
#'
#' @param plot.name plot title string. Defaults to NULL, or a blank title
#'
#' @param map.month specific month to map in form YYYYMM if time.agg = 'month'. Can be created using disperseR::get_yearmon()
#'
#' @param graph.dir location to save output.
#'
#' @param ... modeling parameters passed to ggplot2::theme()
#'
#'
#' @return Creates ggplot object of spatial HyADS impacts


#' @export plot_impact_weighted

plot_impact_weighted <- function(data.linked,
                                 data.units,
                                 link.to = 'zips',
                                 zcta.dataset = NULL,
                                 counties. = NULL,
                                 metric = 'hyads',
                                 time.agg = 'year',
                                 legend.lims = NULL,
                                 legend.name = NULL,
                                 plot.name = NULL,
                                 map.month = NULL,
                                 graph.dir = NULL,
                                 zoom = TRUE,
                                 ...) {
  # define a default theme
  theme.default <- theme(
    plot.title = if (!is.null(plot.name)) {
      element_text(size = 16, hjust = 0.5)
    } else
      element_blank(),
    axis.title = element_blank(),
    legend.position = c(.20, .15),
    legend.text = element_text(size = 8),
    legend.background = element_rect(fill = 'transparent'),
    legend.key.size = unit(.05, 'npc'),
    legend.direction = 'horizontal'
  )

  if (time.agg == "month") {
    data.linked <- data.linked[yearmonth == map.month]
    if( link.to == 'zips'){
      dataset_sf <-
        data.table(merge(
          zcta.dataset,
          data.linked,
          by = c('ZIP'),
          all.y = T
        ))
    } else if( link.to == 'counties'){
      dataset_sf <- data.table(merge(
                                 counties.[, c( "statefp",
                                                "countyfp",
                                                "state_name",
                                                "name",
                                                "geoid",
                                                "geometry")],
                                 data.linked,
                                 by = c( "statefp",
                                         "countyfp",
                                         "state_name",
                                         "name",
                                         "geoid"),
                                 all.y = T
                               ))
    } else if( link.to == 'grids'){
      dataset_r <- suppressWarnings( rasterFromXYZ( data.linked))
      dataset_sp <- as( dataset_r, 'SpatialPolygonsDataFrame')
      dataset_sf <- st_as_sf( dataset_sp)
      suppressWarnings(
        st_crs( dataset_sf$geometry) <-  "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
      )
      dataset_sf$uID <- data.linked$uID
      dataset_sf$comb <- data.linked$comb
    }

    setnames(dataset_sf, metric, 'metric')
    dataset_sf$geometry <- st_transform( dataset_sf$geometry, "+proj=longlat +datum=WGS84 +no_defs")

    ## coordinates
    coord <- data.table( st_coordinates( na.omit( dataset_sf)$geometry))
    setnames( coord, c( 'X', 'Y'), c( 'Longitude', 'Latitude'))
    if (zoom == T){
      long <- coord$Longitude
      minlong <-min(long) - 8
      maxlong <-max(long) + 8
      lat <- coord$Latitude
      minlat <-min(lat) - 8
      maxlat <-max(lat) + 8
    }
    if (zoom == F) {
      ## show all the US map
      minlong <- (-123)
      maxlong <- (-69)
      minlat <- 24
      maxlat <- 50
    }

    long <- data.units$Longitude
    lat <- data.units$Latitude

    facility_loc = data.table(x = long, y = lat)

    if (is.null(legend.lims)) {
      legend.lims <- c(0, quantile(dataset_sf$metric, .95))
    }

    ### graph parameters
    colorscale <- scale_color_viridis(
      name = legend.name,
      discrete = F,
      option = 'magma',
      limits = legend.lims,
      oob = squish,
      direction = 1,
      na.value = NA,
      guide = guide_colorbar(
        title.position = 'top',
        title.hjust = 0.5,
        title.vjust = 0 ,
        label.vjust = 1
      )
    )

    fillscale <- scale_fill_viridis(
      name = legend.name,
      discrete = F,
      option = 'magma',
      limits = legend.lims,
      oob = squish,
      direction = 1,
      na.value = NA,
      guide = guide_colorbar(
        title.position = 'top',
        title.hjust = 0.5,
        title.vjust = 0,
        label.vjust = 1
      )
    )

    ## graph
    if (is.null(plot.name)) {
      stringmonth <-
        month.abb[as.numeric(substring(map.month, 5, nchar(map.month)))]
      plot.name <-
        paste(
          stringmonth,
          substr(map.month, start = 1, stop = 4),
          'HyADS Exposure from Units:',
          paste(unique(data.units$ID), collapse = ', ')
        )
    }

    gg <-
      ggplot(data = dataset_sf, aes(fill  = metric, color = metric)) +
      theme_bw() +
      labs(title = plot.name) +
      geom_sf(aes(geometry = geometry), size = 0.01) +
      geom_polygon(
        data = map_data("state"),
        aes(x = long, y = lat, group = group),
        fill = NA,
        colour = "grey50",
        size = .25
      ) +
      geom_point(
        data = facility_loc,
        aes(x = x, y = y),
        shape = 1,
        colour = "forestgreen",
        inherit.aes = F,
        size = 2,
        stroke = 2
      ) +
      scale_shape_discrete(solid = T) +
      ggplot2::coord_sf(xlim = c(minlong, maxlong),
                        ylim = c(minlat, maxlat)) +
      colorscale +
      fillscale +
      theme.default +
      theme(
        ...
      )


    if (!(is.null(graph.dir))) {
      path <- file.path(graph.dir, "plot_impact_weighted_month.pdf")
      ggsave(path,width = 20,height = 20,units = "cm")
    }

    return(gg)
  }

  if (time.agg == 'year') {
    ## prepare the data
    if( link.to == 'zips'){
      dataset_sf <-
        data.table(merge(
          zcta.dataset,
          data.linked,
          by = c('ZIP'),
          all.y = T
        ))
    } else if( link.to == 'counties'){
      dataset_sf <- data.table(merge(
                                 counties.[, c( "statefp",
                                                "countyfp",
                                                "state_name",
                                                "name",
                                                "geoid",
                                                "geometry")],
                                 data.linked,
                                 by = c( "statefp",
                                         "countyfp",
                                         "state_name",
                                         "name",
                                         "geoid"),
                                 all.y = T
                               ))
      dataset_sf[, uID := NULL]
    } else if( link.to == 'grids'){
      dataset_r <- suppressWarnings( rasterFromXYZ( data.linked))
      dataset_sp <- as( dataset_r, 'SpatialPolygonsDataFrame')
      dataset_sf <- st_as_sf( dataset_sp)
      suppressWarnings(
        st_crs( dataset_sf$geometry) <-  "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
      )
      dataset_sf$ID <- data.linked$ID
      dataset_sf$comb <- data.linked$comb
    }

    setnames(dataset_sf, metric, 'metric')
    dataset_sf$geometry <- st_transform( dataset_sf$geometry, "+proj=longlat +datum=WGS84 +no_defs")

    ## coordinates
    coord <- data.table( st_coordinates( na.omit( dataset_sf)$geometry))
    setnames( coord, c( 'X', 'Y'), c( 'Longitude', 'Latitude'))
    if (zoom == T){
      long <- coord$Longitude
      minlong <-min(long) - 10
      maxlong <-max(long) + 10
      lat <- coord$Latitude
      minlat <-min(lat) - 10
      maxlat <-max(lat) + 10
    }
    if (zoom == F) {
      ## show all the US map
      minlong <- (-123)
      maxlong <- (-69)
      minlat <- 24
      maxlat <- 50
    }

    long <- data.units$Longitude
    lat <- data.units$Latitude
    facility_loc = data.table(x = long, y = lat)

    if (is.null(legend.lims)) {
      legend.lims <- c(0, quantile(dataset_sf$metric, .95))
    }

    ### graph parameters
    colorscale <- scale_color_viridis(
      name = legend.name,
      discrete = F,
      option = 'magma',
      limits = legend.lims,
      oob = squish,
      direction = 1,
      na.value = NA,
      guide = guide_colorbar(
        title.position = 'top',
        title.hjust = 0.5,
        title.vjust = 0 ,
        label.vjust = 1
      )
    )

    fillscale <- scale_fill_viridis(
      name = legend.name,
      discrete = F,
      option = 'magma',
      limits = legend.lims,
      oob = squish,
      direction = 1,
      na.value = NA,
      guide = guide_colorbar(
        title.position = 'top',
        title.hjust = 0.5,
        title.vjust = 0,
        label.vjust = 1
      )
    )

    ## graph
    if (is.null(plot.name)) {
      plot.name = paste(
        unique(dataset_sf$year.E),
        'HyADS Exposure from Units:',
        paste(unique(data.units$ID), collapse = ', ')
      )
    }

    gg <-
      ggplot(data = dataset_sf, aes(fill  = metric, color = metric)) +
      theme_bw() +
      labs(title = plot.name) +
      geom_polygon(
        data = map_data("state"),
        aes(x = long, y = lat, group = group),
        fill = 'white',
        colour = "grey50",
        size = .25
      ) +
      geom_sf(aes(geometry = geometry), size = 0.01) +
      geom_point(
        data = facility_loc,
        aes(x = x, y = y),
        shape = 1,
        colour = "forestgreen",
        inherit.aes = F,
        size = 2,
        stroke = 2
      ) +
      scale_shape_discrete(solid = T) +
      colorscale +
      fillscale +
      ggplot2::coord_sf(xlim = c(minlong, maxlong),
                        ylim = c(minlat, maxlat)) +
      theme.default +
      theme(
        ...
      )


    if (!(is.null(graph.dir))) {
      path <- file.path(graph.dir, "plot_impact_weighted_year.pdf")
      ggsave(path,width = 20,height = 20,units = "cm")
    }

    return(gg)

  }
  # save graph as pdf
}
