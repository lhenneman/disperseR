#' \code{plot_impact_single}
#'
#'
#' @description `plot_impact_single()` takes output from `link_all_units()` and creates a spatial plot
#'
#'
#' @param data.linked data for plotting as output from disperseR::link_all_units()
#'
#' @param data.units information on unit locations as output from disperseR::units()
#'
#' @param link.to spatial scale for plotting. One of 'zips', 'counties', or 'grids' that should match original input to disperseR::link_all_units()
#'
#' @param zcta.dataset ZIP code/ZCTA sptial dataset as imported by disperseR::get_data() or subset thereof. Required only if link.to = 'zips'.
#'
#' @param counties. US counties dataset as imported using USAboundaries::us_counties( ) or subset thereof. Required only if link.to = 'counties'.
#'
#' @param metric plotting metric in `data.linked` input. Defaults to 'hyads', the default output of disperseR::calculate_exposure().
#'
#' @param legend.lims legend limits string. Defaults to NULL, which creates a scale from 0 to the 95th percentile of the 'metric'. Should be in the form c(lower,upper).
#'
#' @param legend.name legend name string. Defaults to NULL, which creates a legend title of 'Aggregate HyADS Exposure'.
#'
#' @param plot.name plot title string. Defaults to NULL, or a descriptive title combining the metric, map.month, and map.unitID
#'
#' @param map.month specific month to map in form YYYYMM if time.agg = 'month'. Can be created using disperseR::get_yearmon()
#'
#' @param map.unitID specific unit to map as string. Must match 'ID' column in data.units.
#'
#' @param graph.dir location to save output.
#'
#' @param ... modeling parameters passed to ggplot2::theme()
#'
#'
#' @return Creates ggplot object of spatial HyADS impacts


#' @export plot_impact_single

plot_impact_single  <- function(data.linked,
                                data.units,
                                link.to = 'zips',
                                zcta.dataset = NULL,
                                counties. = NULL,
                                map.month,
                                map.unitID,
                                plot.name = NULL,
                                metric = 'N',
                                legend.lims = NULL,
                                legend.name = NULL,
                                graph.dir = NULL,
                                zoom = TRUE,
                                ...) {

  dataset_sf <- create_impact_table_single(data.linked = data.linked,
                                           data.units = data.units,
                                           link.to = link.to,
                                           zcta.dataset = zcta.dataset,
                                           counties. = counties.,
                                           map.month = map.month,
                                           map.unitID = map.unitID,
                                           metric = metric)
  dataset_sf$geometry <- st_transform( dataset_sf$geometry, "+proj=longlat +datum=WGS84 +no_defs")
  year.use <- as( substr( map.month, 1, 4), 'integer')

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

  dataunits <- data.units[ID == map.unitID & year == year.use]
  long <- dataunits$Longitude
  lat <- dataunits$Latitude

  facility_loc <- data.table(x = long, y = lat)

  if (is.null(legend.lims)) {
    legend.lims <- c(0, quantile(dataset_sf$metric, .95, na.rm = T))
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


  # make a title if missing
  if (is.null(plot.name)) {
    stringmonth <-
      month.abb[as.numeric(substring(map.month, 5, nchar(map.month)))]
    plot.name = paste(
      stringmonth , substr(map.month, start = 1, stop = 4), 'HyADS Partial Raw Exposure from Unit:',
      map.unitID
    )
  }

  # make a default
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


  # make the plot
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
    ggplot2::coord_sf(xlim = c(minlong, maxlong),
                      ylim = c(minlat, maxlat)) +
    colorscale +
    fillscale +
    theme.default +
    theme(
      ...
    )

  if (!(is.null(graph.dir))) {
    path <- file.path(graph.dir, paste0( "plot_impact_single_", link.to, '_', map.unitID, '_',
                                         map.month, ".pdf"))
    ggsave(path, width = 20, height = 20, units = "cm")
  }

  return(gg)

}


