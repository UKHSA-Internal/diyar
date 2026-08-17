# Test 1 - Fixed episodes
data <- data.frame(
  date = seq.POSIXt(dttm("01/04/2018"), dttm("31/05/2018"), by="3 days"))
data$pid <- "Patient 1"
data$episode_len <- 6
data$d <- data$episode_len * diyar::episode_units$days

data$rd_id <- 1:nrow(data)
data$date_int <- as.number_line(data$date)
#data$date_int@id <- 1L

# episode grouping with episode_group()
T1 <- rbind(head(data,10), head(data,10))
test_1 <- cbind(T1,
                uat.episodes(strata = paste0(T1$pid, " ", c(rep("DS-A", 10), rep("DS-B", 10))),
                             date = T1$date,
                             case_length = T1$episode_len,
                             episode_type = c(rep("fixed", 10), rep("rolling", 10)),
                             group_stats = T,
                             to_s4 = F, by_strata = TRUE))

l <- c(rep("01/04/2018", 3), rep("10/04/2018", 3), rep("19/04/2018", 3),
       rep("28/04/2018", 1), rep("01/04/2018", 10))

r <- c(rep("07/04/2018", 3), rep("16/04/2018", 3), rep("25/04/2018", 3),
       rep("28/04/2018", 1), rep("28/04/2018", 10))

e_int <- number_line(dttm(l), dttm(r))
e_case_nm <- c("Case", "Duplicate_C", "Duplicate_C", "Case", "Duplicate_C",
               "Duplicate_C", "Case", "Duplicate_C", "Duplicate_C", "Case",
               "Case", "Duplicate_C", "Duplicate_C", "Recurrent",  "Duplicate_R",
               "Recurrent", "Duplicate_R", "Recurrent", "Duplicate_R", "Recurrent")

tmp.func <- function(x){
  x <- gsub('\\_.*', '', x)
  x[x == 'Recurrent'] <- 'Duplicate'
  return(x)
}

test_that("test that test episode identifier is as expected for fixed episodes", {
  expect_equal(test_1$epid, c(1,1,1,4,4,4,7,7,7,10, rep(11, 10)))
  expect_equal(tmp.func(test_1$case_nm), tmp.func(e_case_nm))
  expect_equal(test_1$epid_start, left_point(e_int))
  expect_equal(test_1$epid_end, right_point(e_int))
  expect_equal(test_1$epid_total, c(rep(3,9), 1, rep(10,10)))
  expect_equal(as.numeric(test_1$epid_length, unit = 'days'), as.numeric(c(rep(6,9),0, rep(27,10)), units = "days"))
})

# Test 2 - Case assignment - Reverse chronological order
data_2 <- head(data, 10)
data_2$episode_len_s <- 13
data_2$d <- 13 * diyar::episode_units$days

data_2 <- rbind(data_2, data_2)
test_2 <- cbind(data_2,
                uat.episodes(strata = paste0(data_2$pid, " ", c(rep("DS-A", 10), rep("DS-B", 10))),
                             date = data_2$date,
                             case_length = data_2$episode_len_s,
                             from_last = c(rep(F, 10), rep(T, 10)),
                             group_stats = T,
                             to_s4 = F, by_strata = TRUE))

l <- c(rep("01/04/2018", 5), rep("16/04/2018", 5))
r <- c(rep("13/04/2018", 5), rep("28/04/2018", 5))
e_int <- number_line(dttm(c(l, r)), dttm(c(r, l)))

test_that("test reverse episode grouping", {
  expect_equal(test_2$epid, c(rep(1,5),rep(6,5), rep(15,5),rep(20,5)))
  expect_equal(tmp.func(test_2$case_nm), tmp.func(c(rep(c("Case",rep("Duplicate_C",4)),2), rep(c(rep("Duplicate_C",4),"Case"),2))))
  expect_equal(test_2$epid_start, left_point(e_int))
  expect_equal(test_2$epid_end, right_point(e_int))
  expect_equal(test_2$epid_total, rep(5,20))
  expect_equal(as.numeric(test_2$epid_length, units = 'days'), as.numeric(c(rep(12,10), rep(-12,10)), units = "days" ))
})

# Test 3 - Rolling episodes
test_3 <- cbind(data_2,
                uat.episodes(
                  strata = paste0(data_2$pid, " ", c(rep("DS-A", 10), rep("DS-B", 10))),
                  date = data_2$date,
                  case_length = data_2$episode_len_s,
                  episode_type ="rolling",
                  from_last = c(rep(F, 10), rep(T, 10)),
                  group_stats = T,
                  to_s4 = F, by_strata = TRUE))

l <- rep("01/04/2018", 10)
r <- rep("28/04/2018", 10)
e_int <- number_line(dttm(c(l,r)), dttm(c(r,l)))

e_case_nm <- c("Case",rep("Duplicate_C",4),"Recurrent",rep("Duplicate_R",3),"Recurrent")
e_case_nm <- c(e_case_nm, rev(e_case_nm))
test_that("test rolling/recurring episodes", {
  expect_equal(test_3$epid, c(rep(1,10), rep(20,10)))
  expect_equal(tmp.func(test_3$case_nm), tmp.func(e_case_nm))

  expect_equal(test_3$epid_start, left_point(e_int))
  expect_equal(test_3$epid_end, right_point(e_int))
  expect_equal(test_3$epid_total, rep(10,20))
  expect_equal(as.numeric(test_3$epid_length, units = "days"), as.numeric(c(rep(27,10), rep(-27,10)), units = "days"))
})

hospital_infections <- diyar::infections
# Test 8 - Episode unit
# 16-hour (difference of 15 hours) episodes, and the most recent record defined as the "Case"
test_8a <- cbind(hospital_infections,
                 uat.episodes(date = hospital_infections$date, case_length = hospital_infections$epi_len,
                              from_last = T, episode_unit = "hours", group_stats = T, to_s4 = F))

e_int <- number_line(dttm(format(test_8a$date, "%d/%m/%Y")), dttm(format(test_8a$date, "%d/%m/%Y")))

test_that("testing; episode grouping by the hour", {
  expect_equal(test_8a$epid, 1L:11L)
  expect_equal(tmp.func(test_8a$case_nm), tmp.func(c( "Case", rep("Case",10))))

  e_int@gid <- e_int@id <- 1L:11L

  expect_equal(test_8a$epid_start, left_point(e_int))
  expect_equal(test_8a$epid_end, right_point(e_int))
  expect_equal(test_8a$epid_total, rep(1,11))
  expect_equal(as.numeric(test_8a$epid_length, units = 'hours'), as.numeric(rep(0,11), units = "hours" ))
})

# 15-week (difference of 9072000 seconds) episodes , and the most recent record defined as the "Case"
test_8b <- cbind(hospital_infections,
                 uat.episodes(date = hospital_infections$date, case_length = hospital_infections$epi_len,
                              from_last = T, episode_unit = "weeks",  group_stats = T, to_s4 = F))

l <- rep("31/05/2018", 11)
r <- rep("01/04/2018", 11)
e_int <- number_line(dttm(l), dttm(r))

test_that("testing; episode grouping by weeks", {
  expect_equal(test_8b$epid, rep(11L, 11))

  e_int@id <- 1L:11L
  e_int@gid <- rep(11L, 11)

  expect_equal(tmp.func(test_8b$case_nm), tmp.func(c(rep("Duplicate_C",10),"Case")))
  expect_equal(test_8b$epid_start, left_point(e_int))
  expect_equal(test_8b$epid_end, right_point(e_int))
  expect_equal(test_8b$epid_total, rep(11L, 11))
  expect_equal(round(as.numeric(test_8b$epid_length, units = 'weeks'),6), as.numeric(rep(-8.571429,11), units = "weeks"))
})

infections <- diyar::infections
test_that("test that fixed_uat.episodes() with numeric 'date' works the same as compress_number_line()", {
  a <- uat.episodes(date = c(1,1,4,4,1,4,3,2), case_length = 0, to_s4 = T, group_stats = T)
  b <- compress_number_line(x = as.number_line(c(1,1,4,4,1,4,3,2)), collapse =T, deduplicate = F)
  expect_equal(a@epid_interval, b)
})

test_that("test some generic functions", {
  expect_equal(format(new("epid")), "epid(0)")
  b <- rep(as.epid(5L, 5L), 2)
  b@epid_interval@gid <- b@epid_interval@id <- 1:length(b)
  # temp
  #expect_equal(c(as.epid(5L), as.epid(5L)), b)
})
