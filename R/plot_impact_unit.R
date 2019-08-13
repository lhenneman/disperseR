plot_impact_unit<-function(
  data.linked = NULL,
  zip.codes = NULL){

  if(is.null(data.linked)){
    error("Please provide data set to the datalink argument")
  }
  if(is.null(zip.codes)){
    error("Please provide zipcodes")
  }

  dataplot<-data.linked[ZIP %in% zip.codes]
  dataplot<-dataplot[, uID := as( uID, 'character')]

  gg <- ggplot(data=dataplot, aes(x=yearmonth, y=hyads, colour=as.factor(uID)))+
    geom_point()+
    geom_line()+
    theme_bw()+
    labs(y = "Exposure", x = "Month")+
    labs(colour = "Unit ID")+
    facet_grid(ZIP~.)

  gg<-grid.arrange(gg, gg, nrow = 1)

  return(gg)
}
