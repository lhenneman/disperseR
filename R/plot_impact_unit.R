plot_impact_unit<-function(
  data.linked = NULL,
  zip.codes = NULL,
  y.lim = c(24, 50),
  x.lim = c(-123, -69),
  plot.title = "Zip Code Locations",
  legend.lims = NULL,
  legend.title = NULL,
  legend.text.angle = 0){

  #################################################################################

  if(is.null(data.linked)){
    error("Please provide data set to the datalink argument")
  }
  if(is.null(zip.codes)){
    error("Please provide zipcodes")
  }

  dataplot<-data.linked[ZIP %in% zip.codes]
  dataplot<-dataplot[, uID := as( uID, 'character')]

  #################################################################################

  plot1 <- ggplot(data=dataplot, aes(x=yearmonth, y=hyads, colour=as.factor(uID)))+
    geom_point()+
    geom_line()+
    theme_bw()+
    labs(y = "Exposure", x = "Month")+
    labs(colour = "Unit ID")+
    facet_grid(ZIP~., scales = "free")+
    theme(legend.position = "bottom")

  #################################################################################

  zipcodecoordinate<-disperseR::zipcodecoordinate

  colorscale <- scale_color_viridis(name = legend.title,
    discrete = F,
    option = 'magma',
    limits = legend.lims,
    oob = squish,
    direction = 1,
    na.value = NA,
    guide = guide_colorbar(title.position = 'top',
      title.hjust = 0.5,
      title.vjust = 0 ,
      label.vjust = 1))

  fillscale <- scale_fill_viridis(name = legend.title,
    discrete = F,
    option = 'magma',
    limits = legend.lims,
    oob = squish,
    direction = 1,
    na.value = NA,
    guide = guide_colorbar(title.position = 'top',
      title.hjust = 0.5,
      title.vjust = 0,
      label.vjust = 1))

  coordsf<-coord_sf(xlim = x.lim, ylim = y.lim, datum = NA)

  plot2 <- ggplot() +
    theme_bw() +
    labs(title = plot.title) +
    geom_polygon(data = map_data("state"),
      aes(x = long, y = lat, group = group),
      fill = NA,
      colour = "grey50",
      size = .25) +
    geom_point(data = as.data.table(zipcodecoordinate)[ZIP %in% zip.codes],
      aes(x = Longitude, y = Latitude),
      shape = 7,
      colour = "blue",
      inherit.aes = F,
      size = 3) +
    scale_shape_discrete(solid = T) +
    coordsf +
    colorscale +
    fillscale +
    theme(axis.title = element_blank(),
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.20, .15),
      legend.text = element_text(size = 8, angle = legend.text.angle),
      legend.background = element_rect(fill = 'transparent'),
      legend.key.size = unit(.05, 'npc'),
      legend.direction = 'horizontal')

  gg<-gridExtra::grid.arrange(plot1, plot2,layout_matrix = rbind(c(1, 2),
    c(1, NA)), widths = c(2, 1))

}
