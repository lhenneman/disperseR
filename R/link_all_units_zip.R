link_all_units_zip<- function(units.run,
  mc.cores = detectCores(),
  year.mons = NULL,
  start.date = NULL,
  end.date = NULL,
  pbl.height = pblheight,
  crosswalk. = crosswalk,
  duration.run.hours = 240,
  overwrite = FALSE){

  if((is.null(start.date) | is.null(end.date)) & is.null(year.mons)){
    stop( "Define either a start.date and an end.date OR a year.mons")
  }
  zip_link_parallel <- function(unit){
    linked_zips <- parallel::mclapply(yearmons,
      disperseR::disperser_zip_link,
      unit = unit,
      pbl.height = pbl.height,
      crosswalk. = crosswalk.,
      duration.run.hours = duration.run.hours,
      overwrite = overwrite,
      mc.cores = mc.cores)

    print(ncol(linked_zips))
    linked_zips <- data.table::rbindlist(Filter(is.data.table, linked_zips))
    return(linked_zips)
  }

  out <- unitsrun[,zip_link_parallel(.SD), by = seq_len(nrow(unitsrun))]
  out < -out[,comb:=paste("month: ", out[,month], " unitID :", out[,unitID], sep="")]
  return(out)
}
