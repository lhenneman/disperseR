plot_impact_weighted <- function(data.linked,
  data.units,
  zcta.dataset,
  plot.title = NULL,
  metric = 'hyads',
  legend.lims = NULL,
  legend.title = NULL,
  legend.text.angle = 0,
  time.agg = 'year',
  map.month = NULL,
  y.lim = c(24, 50),
  x.lim = c(-123, -69)){

  ### graph parameters
  colorscale <- scale_color_viridis(name = legend.title,
    discrete = F,
    option = 'magma',
    limits = legend.lims,
    oob = squish,
    direction = 1,
    na.value = NA,
    guide = guide_colorbar( title.position = 'top',
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
    guide = guide_colorbar( title.position = 'top',
      title.hjust = 0.5,
      title.vjust = 0,
      label.vjust = 1))

  coordsf<-coord_sf(xlim = x.lim, ylim = y.lim, datum = NA)

  if(time.agg == "month"){

    data.linked <- data.linked[yearmonth==map.month]
    zip_dataset_sf <- data.table(merge(zcta.dataset, data.linked, by = c('ZIP'), all.y = T))
    setnames(zip_dataset_sf, metric, 'metric')

    long <- data.units$Longitude
    lat <- data.units$Latitude

    facility_loc = data.table(x = long, y = lat)

    if(is.null(legend.lims)){
      legend.lims <- c(0, quantile(zip_dataset_sf$metric, .95))
    }

    ## graph

    if (is.null(plot.title)) {
      stringmonth <-
        month.abb[as.numeric(substring(map.month, 5, nchar(map.month)))]
      plot.title <- paste(stringmonth, substr(map.month, start = 1, stop = 4), 'HyADS Exposure from Units:',paste(data.units$ID, collapse = ', ')
      )
    }

    gg <- ggplot(data = zip_dataset_sf,aes(fill  = metric, color = metric)) +
      theme_bw() +
      labs(title = plot.title) +
      geom_sf(aes(geometry = geometry), size = 0.01) +
      geom_polygon(data = map_data("state"),
        aes(x = long, y = lat, group = group),
        fill = NA,
        colour = "grey50",
        size = .25) +
      geom_point(data = facility_loc,
        aes(x = x, y = y),
        shape = 1,
        colour = "forestgreen",
        inherit.aes = F,
        size = 2,
        stroke = 2) +
      scale_shape_discrete(solid = T) +
      coordsf+
      colorscale +
      fillscale +
      theme(
        plot.title = if(!is.null(plot.title)) {
          element_text(size = 10, hjust = 0.5)
        } else
          element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(.20, .15),
        legend.text = element_text(size = 8, angle = legend.text.angle),
        legend.background = element_rect(fill = 'transparent'),
        legend.key.size = unit(.05, 'npc'),
        legend.direction = 'horizontal'
      )
    return(gg)
  }


  if(time.agg == 'year'){

    ## prepare the data
    zip_dataset_sf <- data.table(merge(zcta.dataset, data.linked, by = c('ZIP'), all.y = T))
    setnames(zip_dataset_sf, metric, 'metric')

    long <- data.units$Longitude
    lat <- data.units$Latitude

    facility_loc = data.table(x = long, y = lat)

    if(is.null(legend.lims)){
      legend.lims <- c(0, quantile(zip_dataset_sf$metric, .95))
    }

    ## graph

    if (is.null(plot.title)){
      plot.title = paste(unique(zip_dataset_sf$year.E), 'HyADS Exposure from Units:',paste(data.units$ID, collapse = ', '))
    }
    gg <- ggplot(data = zip_dataset_sf,aes(fill  = metric, color = metric)) +
      theme_bw() +
      labs(title = plot.title) +
      geom_sf(aes(geometry = geometry), size = 0.01) +
      geom_polygon(data = map_data("state"),
        aes(x = long, y = lat, group = group),
        fill = NA,
        colour = "grey50",
        size = .25) +
      geom_point(data = facility_loc,
        aes(x = x, y = y),
        shape = 1,
        colour = "forestgreen",
        inherit.aes = F,
        size = 2,
        stroke = 2) +
      scale_shape_discrete(solid = T) +
      coordsf +
      colorscale +
      fillscale +
      theme(
        plot.title = if(!is.null(plot.title)) {
          element_text(size = 10, hjust = 0.5)
        } else
          element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(.20, .15),
        legend.text = element_text(size = 8, angle = legend.text.angle),
        legend.background = element_rect(fill = 'transparent'),
        legend.key.size = unit(.05, 'npc'),
        legend.direction = 'horizontal'
      )
    return(gg)
  }
}

