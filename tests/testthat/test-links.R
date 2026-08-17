context("Unit tests - links()")

library(testthat)
library(diyar)

uat.links <- function(..., group_stats = TRUE, recursive = TRUE){
  diyar::links(..., group_stats = group_stats, display = "none", recursive = recursive)
}

source('uat_links.R', local = environment())

attr1 <- c(1,1,1,1,1)
attr2 <- c(1,1,1,2,2)
attr3 <- c(1,1,1,4,5)

tmp.func.1 <- function(x, y) x == 1
tmp.func.2 <- function(x, y) x == y
x6 <- uat.links(list("p1", "p1"),
                sub_criteria = list(
                  cr1 = sub_criteria(attr1, match_funcs = tmp.func.1),
                  cr2 = sub_criteria(attr2, match_funcs = tmp.func.1)),
                shrink = TRUE, batched = "no",
                repeats_allowed = FALSE,
                permutations_allowed = FALSE,
                recursive = TRUE)

x7 <- uat.links(list("p1", "p1"),
                sub_criteria = list(
                  cr1 = sub_criteria(attr1, match_funcs = tmp.func.2),
                  cr2 = sub_criteria(attr2, match_funcs = tmp.func.2)),
                shrink = TRUE, batched = "no",
                repeats_allowed = FALSE,
                permutations_allowed = FALSE,
                recursive = TRUE)

test_that("handling when `shrink` affects `batched`", {
  # `shrink` breaks breaks group-1 via a sub_criteria
  # match_func is in a way that the second half can't remain as a group
  expect_equal(x6@.Data, x1@.Data)
  expect_equal(x6@pid_cri, x1@pid_cri)
  # exact scenario but
  # match_func is in a way that the second half can still remain as a group
  expect_equal(x7@.Data, c(1,1,1,4,4))
  expect_equal(x7@pid_cri, c(2,2,2,2,2))
})

btwn <- function(x, l, r){
  x >= l & x <= r
}
f2 <- function(x, y){
  btwn(abs(x$num - y$num), 0, 40)
}
sb.cri <- sub_criteria(
  attrs(id = 1:10, num = 1:10 * 10), match_funcs = f2,
  equal_funcs = diyar::false
)
d1 <- uat.links(
  criteria = "p1", sub_criteria = list("cr1" = sb.cri), recursive = FALSE)

d2 <- uat.links(
  criteria = "p1", sub_criteria = list("cr1" = sb.cri), recursive = TRUE)

test_that("test `recursive` linkage", {
  expect_equal(d1@.Data, c(1,1,1,1,1,6,6,6,6,6))
  expect_equal(d1@pid_cri, rep(1, 10))
  expect_equal(d2@.Data, rep(1, 10))
  expect_equal(d2@pid_cri, d1@pid_cri)
})

f2.v2 <- function(x, y){
  btwn(abs(x$num - y$num), 0, 50)
}
sb.cri.v2 <- sub_criteria(
  attrs(id = 1:10, num = 1:10 * 10), match_funcs = f2.v2,
  equal_funcs = diyar::false
)

pd1a <- uat.links(
  criteria = "p1", sub_criteria = list("cr1" = sb.cri.v2), recursive = TRUE,
  batched = "yes")

pd1b <- uat.links(
  criteria = "p1", sub_criteria = list("cr1" = sb.cri.v2), recursive = TRUE,
  batched = "semi")

pd1c <- uat.links(
  criteria = "p1", sub_criteria = list("cr1" = sb.cri.v2), recursive = TRUE,
  batched = "no")

test_that("test `batched` linkage", {
  expect_equal(pd1a@.Data, rep(1, 10))
  expect_equal(pd1a@.Data, pd1b@.Data)
  expect_equal(pd1b@.Data, pd1c@.Data)
  expect_equal(max(pd1a@iteration) > max(pd1b@iteration), TRUE)
  expect_equal(max(pd1b@iteration) > max(pd1c@iteration), TRUE)
  expect_equal(max(pd1a@iteration), 10)
  expect_equal(max(pd1b@iteration), 3)
  expect_equal(max(pd1c@iteration), 1)
})

f2.v3 <- function(x, y){
  btwn(x$num - y$num, 0, 50)
}
sb.cri.v3 <- sub_criteria(
  attrs(id = 1:10, num = 1:10 * 10), match_funcs = f2.v3,
  equal_funcs = diyar::false
)
t_sort_v3 <- c(1,1,1,1,1,0,0,0,0,0)
pd2a <- uat.links(
  criteria = "p1", sub_criteria = list("cr1" = sb.cri.v2), recursive = TRUE,
  tie_sort = t_sort_v3, batched = "yes")

pd2b <- uat.links(
  criteria = "p1", sub_criteria = list("cr1" = sb.cri.v2), recursive = TRUE,
  tie_sort = t_sort_v3, batched = "semi")

pd2c <- uat.links(
  criteria = "p1", sub_criteria = list("cr1" = sb.cri.v2), recursive = TRUE,
  tie_sort = t_sort_v3, batched = "no")

test_that("test `batched` with `tie_sort`", {
  # should be the same as with no `tie_sort`
  expect_equal(pd2a@.Data, rep(6, 10))
  expect_equal(pd2a@.Data, pd2b@.Data)
  expect_equal(pd2b@.Data, pd2c@.Data)
  expect_equal(max(pd2a@iteration) > max(pd2b@iteration), TRUE)
  expect_equal(max(pd2b@iteration) > max(pd2c@iteration), TRUE)
  expect_equal(max(pd2a@iteration), 10)
  expect_equal(max(pd2b@iteration), 2)
  expect_equal(max(pd2c@iteration), 1)
})
