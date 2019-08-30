plot_units_ranked <- function(data.ranked, data.units, year, graph.dir) {

  data.units <- data.units[year==2005]

  unitRanks <- merge(data.ranked, data.units, by = 'uID')
  long <- unitRanks$Longitude
  lat <- unitRanks$Latitude
  hyads.py.sum<-unitRanks$hyads.py.sum
  rank<-unitRanks$hyads.rank
  facility_loc <- data.table(x = long, y = lat, hyads.py.sum = hyads.py.sum, rank=rank)
  title <- paste("Ranking for year:", year)

  ggmap <- ggplot2::ggplot(data = unitRanks2005) +
    ggplot2::theme_bw() +
    labs(title = title) +
    ggplot2::geom_polygon(
      data = ggplot2::map_data("state"),
      aes(x = long, y = lat, group = group),
      fill = NA,
      colour = "grey50",
      size = .25
    ) +
    ggplot2::geom_point(
      data = facility_loc,
      aes(x = x, y = y),
      shape = 1,
      colour = "red",
      inherit.aes = F,
      size = 2,
      stroke = 2
    ) +
    ggrepel::geom_label_repel(data = facility_loc, aes(x=x, y=y,label = rank),
      nudge_x = 1,
      na.rm = TRUE) +
    ggplot2::scale_shape_discrete(solid = T) +
    ggplot2::coord_sf(xlim = c(-66, -90),
      ylim = c(27, 50),
      datum = NA) +
    ggplot2::theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom")

  unitRanks<-unitRanks %>%
    tidyr::gather(., key = "type", value = "Measurement", hyads.py.sum, SOx) %>%
    mutate(type=ifelse(type=="hyads.py.sum", "Hyads Exposure", "SOx emission"))

  ggbar <- ggplot2::ggplot(data=unitRanks, aes(x=as.character(uID), y=Measurement))+
    ggplot2::geom_bar(stat='identity')+
    ggplot2::facet_wrap(.~type, scales="free")+
    ggplot2::theme_bw()+
    ggplot2::scale_y_continuous(labels = scales::comma)+
    xlab("Unit ID")

  gg <-
    gridExtra::grid.arrange(ggbar,
      ggmap,
      layout_matrix = rbind(c(1, 2),
        c(1, NA)),
      widths = c(2, 1))

  if (!(is.null(graph.dir))) {
    path <- file.path(graph.dir, "plot_ranking.pdf")
    ggsave(path, width = 20, height = 20, units = "cm")
  }

  return(gg)

}
