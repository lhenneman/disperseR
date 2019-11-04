#' @export plot_units_ranked

plot_units_ranked <- function(data.ranked, data.units, year, graph.dir) {

  data.units <- data.units[year==2005]
  data.units[, uID := as( uID, 'character')]
  data.ranked[, uID := as( uID, 'character')]
  unitRanks <- merge(data.ranked, data.units, by = 'uID')

  ## coordinates
  long <- unitRanks$Longitude
  minlong <-min(long) - 8
  maxlong <-max(long) + 8
  lat <- unitRanks$Latitude
  minlat <-min(lat) - 8
  maxlat <-max(lat) + 8

  uID <- unitRanks$uID
  hyads.py.sum<-unitRanks$hyads.py.sum

  rank<-unitRanks$hyads.rank

  facility_loc <- data.table(x = long, y = lat, hyads.py.sum = hyads.py.sum, rank = rank, uID = uID) %>%
    mutate(label = paste("UNIT:", uID, "ranked", rank))

  title <- paste("Ranking (The biggest polluters) for year: ", year)

  ggmap <- ggplot2::ggplot(data = ggplot2::map_data("state")) +
    ggplot2::theme_bw() +
    labs(title = title) +
    ggplot2::geom_polygon(
      aes(x = long, y = lat, group = group),
      fill = NA,
      colour = "grey50",
      size = .25
    ) +
    ggplot2::geom_point(
      data = facility_loc,
      aes(x = x, y = y),
      shape = 1,
      colour = "forestgreen",
      inherit.aes = F,
      size = 2,
      stroke = 2
    ) +
   ggrepel::geom_label_repel(data = facility_loc,
      aes(x=x, y=y,label = label),
      nudge_x = 10,
      nudge_y = 10,
      segment.size = 0.7,
      na.rm = TRUE) +
    ggplot2::scale_shape_discrete(solid = T) +
    ggplot2::coord_sf(xlim = c(minlong, maxlong),
      ylim = c(minlat, maxlat))+
    ggplot2::theme(legend.position = "bottom")

  if (!(is.null(graph.dir))) {
    path <- file.path(graph.dir, "plot_ranking_map.pdf")
    ggsave(path, width = 20, height = 20, units = "cm")
  }

  unitRanks<-unitRanks %>%
    tidyr::gather(., key = "type",
      value = "Measurement",
      hyads.py.sum,
      SOx) %>%
    mutate(type=ifelse(type=="hyads.py.sum",
      "Hyads Exposure",
      "SOx emission"))

  ggbar <- ggplot2::ggplot(data=unitRanks, aes(x = as.character(uID), y = Measurement))+
    ggplot2::geom_bar(stat = 'identity',
      color = "navyblue",
      fill = "grey",
      width = 0.3)+
    ggplot2::facet_wrap(.~type, scales="free")+
    ggplot2::theme_bw()+
    ggplot2::scale_y_continuous(labels = scales::comma)+
    xlab("Unit ID")

  return(list(ggbar = ggbar, ggmap = ggmap))

}
