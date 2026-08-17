context("Unit tests - episodes_af_shift()")

library(testthat)
library(diyar)

date <- function(x) as.Date(x, "%d/%m/%Y")
dttm <- function(x) as.POSIXct(x, "GMT",format="%d/%m/%Y")
suffix <- function(df, x){
  names(df) <- paste0(names(df), ".", x)
  df
}
v.decode <- function(x) as.vector(diyar::decode(x))
uat.episodes <- function(
    date, strata =1, case_length, episode_type = 'fixed', to_s4 = T,
    from_last = FALSE, by_strata = FALSE, episode_unit = 'days', ...){
  dfr <- data.frame(
    sn = seq_len(length(date)), len = case_length, strata,
    episode_type, from_last, eUnit = episode_unit)
  dfr$date <- date
  grps <- unique(dfr$strata)
  dfr$date <- as.number_line(dfr$date)
  opt.is_dt <- inherits(dfr$date@start, c("Date","POSIXct","POSIXt","POSIXlt"))
  if(isTRUE(opt.is_dt)){
    dfr$date <- number_line(
      l = as.POSIXct(dfr$date@start, tz = 'GMT'),
      r = as.POSIXct(right_point(dfr$date), tz = 'GMT')
    )
  }
  x <- list()
  for (i in seq_len(length(grps))) {
    lgk <- dfr$strata == grps[i]

    tmp.date <- dfr$date[lgk]
    tmp.episode_type <- unique(dfr$episode_type[lgk])
    tmp.from_last <- unique(dfr$from_last[lgk])
    tmp.len <- unique(dfr$len[lgk])
    tmp.epid_unit <- unique(dfr$eUnit[lgk])

    opt.epid_unit <- tolower(tmp.epid_unit)

    if(!opt.is_dt){
      opt.epid_unit <- "seconds"
    }
    opt.epid_unit <- match(opt.epid_unit, names(episode_units))
    class(opt.epid_unit) <- "d_label"
    attr(opt.epid_unit, "value") <- as.vector(sort(opt.epid_unit[!duplicated(opt.epid_unit)]))
    attr(opt.epid_unit, "label") <- names(episode_units)[attr(opt.epid_unit, "value")]
    attr(opt.epid_unit, "state") <- "encoded"

    if(tmp.from_last){
      tmp.date <- invert_number_line(tmp.date)
    }

    tmp.len <- tmp.len * episode_units[[opt.epid_unit]]
    x[[i]] <- episodes_af_shift(
      sn = dfr$sn[lgk], date = tmp.date, tmp.len, episode_unit = 'seconds',
      episode_type = tmp.episode_type, case_length = tmp.len, ...)
  }

  x <- do.call('c', x)
  x@epid_interval <- group_stats(x@.Data, left_point(dfr$date), right_point(dfr$date))
  if(isTRUE(opt.is_dt)){
    x@epid_interval@start <- as.POSIXct(x@epid_interval@start, tz = 'GMT')
  }
  x@epid_interval[dfr$from_last] <- reverse_number_line(x@epid_interval[dfr$from_last])
  x@epid_length <- right_point(x@epid_interval) - x@epid_interval@start

  if(to_s4 == F){
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    vrs <- grep('case_nm|wind_nm|epid_dataset', names(x), value = TRUE)
    for(v in vrs){
      if(length(x[[v]]) == 0) next
      x[[v]] <- v.decode(x[[v]])
    }
  }else{
    x@options <- list()
  }
  return(x)
}

source('uat_episodes_simple.R', local = environment())
