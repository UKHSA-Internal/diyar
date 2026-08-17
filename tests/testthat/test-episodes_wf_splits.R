context("Unit tests - episodes_wf_splits()")

library(testthat)
library(diyar)

date <- function(x) as.Date(x, "%d/%m/%Y")
dttm <- function(x) as.POSIXct(x, "GMT",format="%d/%m/%Y")
suffix <- function(df, x){
  names(df) <- paste0(names(df), ".", x)
  df
}
v.decode <- function(x) as.vector(diyar::decode(x))
uat.episodes <- function(..., to_s4 = T, by_strata = FALSE){
  if(to_s4 == F){
    x <- as.data.frame(diyar::episodes_wf_splits(..., display = "none"), stringsAsFactors = FALSE)
    vrs <- grep('case_nm|wind_nm|epid_dataset', names(x), value = TRUE)
    for(v in vrs){
      if(length(x[[v]]) == 0) next
      x[[v]] <- v.decode(x[[v]])
    }
  }else{
    x <- diyar::episodes_wf_splits(..., display = "none")
    x@options <- list()
  }
  return(x)
}

source('uat_episodes.R', local = environment())
source('uat_episodes_simple.R', local = environment())
