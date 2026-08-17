context("Unit tests - links_af_dt()")

library(testthat)
library(diyar)

uat.links <- function(
    ..., group_stats = TRUE, repeats_allowed, recursive, permutations_allowed, batched){
  diyar::links_af_dt(..., group_stats = group_stats)
}

source('uat_links.R', local = environment())
