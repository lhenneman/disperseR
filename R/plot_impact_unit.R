#' create a set of directories to run disperseR
#'
#' \code{plot_impact_unit}
#'
#' @description `plot_impact_unit()` produces two graphs. One is a map showing where the units are and showing their rank and the other one contains bar plots.
#'
#'
#' @param data.linked data for plotting as output from disperseR::calculate_exposure()
#'
#' @param zip.codes list of ZIP codes (as character strings) for ranking
#'
#' @param y.lim vertical limits for the spatial plot
#'
#' @param x.lim horizontal limits for the spatial plot
#'
#' @param plot.title plot title character string
#'
#' @param legend.lims legend limits. as c( lower,upper)
#'
#' @param legend.title legend title as string
#'
#' @param legend.text.angle angle of legend text (helpful for large numbers)
#'
#' @param graph.dir location to save output.
#'
#'
#' @return Creates directories (does not overwrite if existing). Outputs string variables with paths to the environment.


#' @export plot_impact_unit

plot_impact_unit <- function(data.linked = NULL,
  zip.codes = NULL,
  y.lim = c(24, 50),
  x.lim = c(-123,-69),
  plot.title = NULL,
  legend.lims = NULL,
  legend.title = NULL,
  legend.text.angle = 0,
  graph.dir = NULL) {
  #################################################################################

  if (is.null(plot.title)){
    plot.title <- paste("Zip Code Locations" ,paste(zipcodes, collapse = ', '))
  }

  if (is.null(data.linked)) {
    error("Please provide data set to the datalink argument")
  }
  if (is.null(zip.codes)) {
    error("Please provide zipcodes")
  }

  dataplot <- data.linked[ZIP %in% zip.codes]
  dataplot <- dataplot[, uID := as(uID, 'character')]
  dataplot <- dataplot[, year := as.numeric(substr(yearmonth, start = 1, stop = 4))]
  dataplot <- dataplot[, month := as.numeric(substr(yearmonth, start = 5, stop = 6))]
  dataplot$date <- with(dataplot, lubridate::ymd(sprintf('%04d%02d%02d', year, month, 1)))

  #################################################################################

  plot1 <-
    ggplot2::ggplot(data = dataplot, aes(
      x = date,
      y = hyads,
      colour = as.factor(uID)
    )) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Exposure", x = "Month") +
    ggplot2::labs(colour = "Unit ID") +
    ggplot2::facet_grid(ZIP ~ ., scales = "free") +
    ggplot2::theme(legend.position = "bottom")+
    ggplot2::scale_x_date(labels = date_format("%m-%Y"))

  #################################################################################

  zipcodecoordinate <- disperseR::zipcodecoordinate

  colorscale <- viridis::scale_color_viridis(
    name = legend.title,
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

  fillscale <- viridis::scale_fill_viridis(
    name = legend.title,
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

  coordsf <- ggplot2::coord_sf(xlim = x.lim,
    ylim = y.lim)

  plot2 <- ggplot() +
    ggplot2::theme_bw() +
    ggplot2::labs(title = plot.title) +
    ggplot2::geom_polygon(
      data = map_data("state"),
      aes(x = long, y = lat, group = group),
      fill = NA,
      colour = "grey50",
      size = .25
    ) +
    ggplot2::geom_point(
      data = as.data.table(zipcodecoordinate)[ZIP %in% zip.codes],
      aes(x = Longitude, y = Latitude),
      shape = 7,
      colour = "blue",
      inherit.aes = F,
      size = 3
    ) +
    ggplot2::scale_shape_discrete(solid = T) +
    coordsf +
    colorscale +
    fillscale +
    ggplot2::theme(
      axis.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.20, .15),
      legend.text = element_text(size = 8, angle = legend.text.angle),
      legend.background = element_rect(fill = 'transparent'),
      legend.key.size = unit(.05, 'npc'),
      legend.direction = 'horizontal'
    )

  gg <-
    gridExtra::grid.arrange(plot1,
      plot2,
      layout_matrix = rbind(c(1, 2),
        c(1, NA)),
      widths = c(2, 1))

  if (!(is.null(graph.dir))) {
    path <- file.path(graph.dir, "plot_impact_unit.pdf")
    ggsave(path, width = 20, height = 20, units = "cm")
  }
}
