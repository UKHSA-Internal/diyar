context("Unit tests - links_af_modular()")

library(testthat)
library(diyar)

uat.links <- function(
    criteria, group_stats = TRUE, repeats_allowed, recursive, permutations_allowed,
    batched, data_source = NULL, data_links = NULL,
    stepwise_method = 'expand_with_priority', ...){

  diyar::links_af_modular(
    criteria = criteria, group_stats = group_stats,
    data_source = data_source, data_links = data_links,
    stepwise_method = stepwise_method, ...)
}

source('uat_links.R', local = environment())
