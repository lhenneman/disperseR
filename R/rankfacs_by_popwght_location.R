rankfacs_by_popwgt_location <- function(link.files = NULL,
  data.linked = NULL,
  crosswalk.,
  data.units = unitsrun,
  rank.by = c('hyads'),
  zip.value = '*',
  state.value = '*',
  city.value = '*',
  year. = NULL) {

  crosswalk. <- data.table(crosswalk.)[, year := 2005]
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
}
