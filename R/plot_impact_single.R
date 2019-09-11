plot_impact_single  <- function(data.linked,
  data.units,
  zcta.dataset,
  map.month,
  map.unitID,
  plot.title = NULL,
  metric = 'N',
  legend.lims = NULL,
  legend.title = NULL,
  graph.dir = NULL,
  legend.text.angle = 0) {

  datareduced <- data.linked[month == map.month & unitID == map.unitID]
  dataunits <- data.units[ID == map.unitID]
  zip_dataset_sf <-
    data.table(dataunits,  merge(
      zcta.dataset,
      datareduced,
      by = c('ZIP'),
      all.y = T
    ))
  setnames(zip_dataset_sf, metric, 'metric')

  long <- dataunits$Longitude
  lat <- dataunits$Latitude

  facility_loc <- data.table(x = long, y = lat)

  if (is.null(legend.lims)) {
    legend.lims <- c(0, quantile(zip_dataset_sf$metric, .95))
  }

  ### graph parameters
  colorscale <- scale_color_viridis(
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

  fillscale <- scale_fill_viridis(
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

  ## graph

  if (is.null(plot.title)) {
    stringmonth <-
      month.abb[as.numeric(substring(map.month, 5, nchar(map.month)))]
    plot.title = paste(
      stringmonth , substr(map.month, start = 1, stop = 4), 'HyADS Partial Raw Exposure from Unit:',
      map.unitID
    )
  }
  gg <-
    ggplot(data = zip_dataset_sf, aes(fill  = metric, color = metric)) +
    theme_bw() +
    labs(title = plot.title) +
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
    coord_sf(xlim = c(-123,-69),
      ylim = c(24, 50)) +
    colorscale +
    fillscale +
    theme(
      plot.title = if (!is.null(plot.title)) {
        element_text(size = 16, hjust = 0.5)
      } else
        element_blank(),
      axis.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.20, .15),
      legend.text = element_text(size = 8, angle = legend.text.angle),
      legend.background = element_rect(fill = 'transparent'),
      legend.key.size = unit(.05, 'npc'),
      legend.direction = 'horizontal'
    )

  if (!(is.null(graph.dir))) {
    path <- file.path(graph.dir, "plot_impact_single.pdf")
    ggsave(path, width = 20, height = 20, units = "cm")
  }

  return(gg)

}


