#' @export rankfacs_by_popwgt_location

rankfacs_by_popwgt_location <- function(link.files = NULL,
  data.linked = NULL,
  crosswalk.,
  rank.by = c('hyads'),
  zip.value = '*',
  state.value = '*',
  city.value = '*',
  year = NULL) {

  crosswalk. <- data.table(crosswalk.)[, year := year]
  data.linked <- data.table(data.linked)[, year := year]

  # make sure either link.files or data.linked edist
  if ((is.null(link.files) &
      is.null(data.linked)) | (!is.null(link.files) & !is.null(data.linked)))
    stop("Please provide EITHER link.files OR data.linked")


  `%ni%` <- Negate(`%in%`)
  # make sure year column is in both data.tables
  if (('year' %ni% names(data.linked)) |
      ('year' %ni% names(crosswalk.)))
    stop("data.linked and crosswalk. should both include a column named 'year'.")

  ## Change name of census population variable
  crosswalk.use <- copy(crosswalk.)

  # read in HyADS link file and ampd file
  if (!is.null(link.files)) {
    data.linked <- fread(link.files)[, V1 := NULL]

    data.linked[, `:=` (
      ZIP  = formatC(
        ZIP,
        width = 5,
        format = "d",
        flag = "0"
      ),
      uID  = gsub('_|-|\\*', '.', uID),
      year = as.integer(gsub('_.*$', '', yearmonth))
    )]
  }

  ## Merge ZIP code and census info with data.linked
  data.linked <- merge(data.linked, crosswalk.use, by = c('ZIP', 'year'))

  ## limit data table to subset.value in subset.feature
  zip.search   <- paste0(zip.value,   collapse = '|')
  state.search <- paste0(state.value, collapse = '|')
  city.search <- paste0(city.value, collapse = '|')

  data.linked.trim <-
    data.linked[Reduce(intersect, list(
      grep(zip.search, ZIP),
      grep(state.search, STATE),
      grep(city.search, CITY)
    ))]

  ## Weight metric by popultion
  names.py <- paste0(rank.by, '.py')
  data.linked.trim[, (names.py) := lapply(rank.by, function(x) {
    TOTALESTIMATE * get(x)
  })]

  ## Sum pop-weighted rank.by by uID
  names.py.sum <- paste0(rank.by, '.py.sum')
  uID.pw <-
    data.linked.trim[, lapply(names.py, function(x) {
      sum(get(x))
    }),
      by = c("uID", "year")]
  setnames(uID.pw, paste0('V', 1:length(names.py.sum)), names.py.sum)

  ## Rank facilities by metric in each year
  names.py.rank <- paste0(rank.by, '.rank')
  uID.pw[, (names.py.rank) := lapply(names.py.sum, function(x) {
    frankv(get(x), order = -1)
  }),
    by = year]

  return(uID.pw)
  unitRanks <- merge(uID.pw, data.units, by = 'uID')
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

}
