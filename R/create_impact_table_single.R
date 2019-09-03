create_impact_table_single <- function(data.linked,
  data.units,
  zcta.dataset,
  map.month,
  map.unitID,
  metric = 'N') {
  datareduced <- data.linked[month == map.month & unitID == map.unitID]
  dataunits <- data.units[ID == map.unitID]
  zip_dataset_sf <- data.table(dataunits,
    merge(
      zcta.dataset,
      datareduced,
      by = c('ZIP'),
      all.y = T
    ))
  setnames(zip_dataset_sf, metric, 'metric')
  myVector<-c("unitID", "Latitude", "Longitude", "metric", "ZIP", "ZCTA", "CITY", "STATE", "TOTALESTIMATE", "MARGINOFERROR")
  zip_dataset_sf <- zip_dataset_sf[, myVector, with=FALSE]
  return(zip_dataset_sf)
}
