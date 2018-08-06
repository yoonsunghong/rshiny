# ===================================================================
# Title: tests.R
# Description:
#   This script tests the code using testthat
# Author: Yoon Sung Hong
# Date: 11-07-2017
# ===================================================================
library(testthat)
source("functions.R")

context("remove_missing")
test_that('remove_missing is correct',{
  expect_equal(remove_missing(c(1,2,3,NA,6)),c(1,2,3,6))
  expect_equal(remove_missing(c(1,2,NA,NA,6)),c(1,2,6))
  expect_equal(remove_missing(c(1,NA,NA,NA,3)),c(1,3))
  expect_equal(remove_missing(c(6,10,50)),c(6,10,50))
})

context("get minimum")
test_that('get_minimum is correct',{
  expect_equal(get_minimum(c(1,2,3,NA,6), na.rm = TRUE),1)
  expect_equal(get_minimum(c(2,3,NA,6), na.rm = TRUE),2)
  expect_equal(get_minimum(c(1,2,2,3,10,50), na.rm = FALSE),1)
  expect_equal(get_minimum(c(6,10,50), na.rm = FALSE),6)
})

context("get maximum")
test_that('get_maximum is correct',{
  expect_equal(get_maximum(c(1,2,3,NA,6), na.rm = TRUE),6)
  expect_equal(get_maximum(c(1,2,1,NA,8), na.rm = TRUE),8)
  expect_equal(get_maximum(c(6,2,100), na.rm = FALSE),100)
  expect_equal(get_maximum(c(6,10,50), na.rm = FALSE),50)
})

context("get range")
test_that('get_range is correct',{
  expect_equal(get_range(c(1,2,3,NA,6), na.rm = TRUE),5)
  expect_equal(get_range(c(1,2,3,NA,8), na.rm = TRUE),7)
  expect_equal(get_range(c(6,2,100,30,40), na.rm = FALSE),98)
  expect_equal(get_range(c(6,10,50), na.rm = FALSE),44)
})

context("get percentile 10")
test_that('get_percentile10 is correct',{
  a <- c(1,4,7,NA,10)
  b <- c(0,1,2,3,4,5,6,7,8,9,10)
  expect_equal(get_percentile10(a,na.rm = TRUE),1.9)
  expect_equal(get_percentile10(b,na.rm = FALSE),1)
  expect_equal(get_percentile10(c(0,1,2,3,4,5,6,7,8,9,10,NA), na.rm = TRUE),1)
  expect_equal(get_percentile10(c(1,4,7,10), na.rm = FALSE),1.9)
})

context("get percentile 90")
test_that('get_percentile10 is correct',{
  a <- c(1,4,7,NA,10)
  b <- c(0,1,2,3,4,5,6,7,8,9,10)
  expect_equal(get_percentile90(a,na.rm = TRUE),9.1)
  expect_equal(get_percentile90(b,na.rm = FALSE),9)
  expect_equal(get_percentile90(c(0,1,2,3,4,5,6,7,8,9,10,NA,NA), na.rm = TRUE),9)
  expect_equal(get_percentile90(c(1,4,7,10), na.rm = FALSE),9.1)
})

context("get median")
test_that('get_median is correct',{
  a <- c(1,4,7,NA,10)
  b <- c(0,1,2,3,4,5,6,7,8,9,10)
  expect_equal(get_median(a,na.rm = TRUE),5.5)
  expect_equal(get_median(b,na.rm = FALSE),5)
  expect_equal(get_median(c(0,1,2,3,4,5,6,7,8,9,10,NA,NA),na.rm = TRUE),5)
  expect_equal(get_median(c(1,4,7,10)),5.5)
})

context("get average")
test_that('get_average is correct',{
  a <- c(1,4,7,NA,10)
  b <- c(0,1,2,3,4,5,6,7,8,9,10)
  expect_equal(get_average(a,na.rm = TRUE),5.5)
  expect_equal(get_median(b,na.rm = FALSE),5)
  expect_equal(get_median(c(0,1,2,3,4,5,6,7,8,9,10,NA,NA),na.rm = TRUE),5)
  expect_equal(get_average(c(1,4,7,10)),5.5)
})

context("get stdev")
test_that('get_stdev is correct',{
  a <- c(1,4,7,NA,10)
  b <- c(0,1,2,3,4,5,6,7,8,9,10)
  expect_equal(get_stdev(a,na.rm = TRUE), sd(c(1,4,7,10)))
  expect_equal(get_stdev(b,na.rm = FALSE), sd(b))
  expect_equal(get_stdev(c(0,1,2,3,4,5,6,7,8,9,10,NA,NA),na.rm = TRUE), sd(b))
  expect_equal(get_stdev(c(1,4,7,10), na.rm = FALSE),sd(c(1,4,7,10)))
})

context("get quartile1")
test_that('get_quartile1 is correct',{
  a <- c(1,4,7,NA,10)
  b <- c(0,1,2,3,4,5,6,7,8,9,10)
  expect_equal(get_quartile1(a,na.rm = TRUE),3.25)
  expect_equal(get_quartile1(b,na.rm = TRUE), 2.5)
  expect_equal(get_quartile1(c(0,1,2,3,4,5,6,7,8,9,10,NA,NA),na.rm = TRUE), 2.5)
  expect_equal(get_quartile1(c(1,4,7,10)),3.25)
})

context("get quartile3")
test_that('get_quartile3 is correct',{
  a <- c(1,4,7,NA,10)
  b <- c(0,1,2,3,4,5,6,7,8,9,10)
  expect_equal(get_quartile3(a,na.rm = TRUE),7.75)
  expect_equal(get_quartile3(b,na.rm = FALSE), 7.5)
  expect_equal(get_quartile3(c(0,1,2,3,4,5,6,7,8,9,10,NA,NA),na.rm = TRUE), 7.5)
  expect_equal(get_quartile3(c(1,4,7,10), na.rm = FALSE),7.75)
})

context("count missing")
test_that('count_missing is correct',{
  a <- c(1,4,7,NA,10)
  b <- c(NA,NA,1)
  v <- c(NA,NA,NA,4)
  expect_equal(count_missing(a),1)
  expect_equal(count_missing(b),2)
  expect_equal(count_missing(v),3)
  expect_equal(count_missing(c(1,4,7,10)),0)
})

context("summary stats")
test_that('summary_stats is correct',{
  a1 <- c(1,4,7,NA,10)
  a2 <- c(0,1,2,3,4,5,6,7,8,9,10)
  a3 <- c(1,4,7,10)
  a4 <- c(0,1,2,3,4,5,6,7,8,9,NA,10)
  x1 <-list(1,1.9,3.25,5.5,5.5,7.75,9.1,10,9,sd(c(1,4,7,10)),1)
  names(x1) <- c("minimum","percent10","quartile1","median","mean","quartile3",
                "percent90","maximum","range","stdev","missing")
  x2 <-list(0,1,2.5,5,5,7.5,9,10,10,sd(c(0,1,2,3,4,5,6,7,8,9,10)),0)
  names(x2) <- c("minimum","percent10","quartile1","median","mean","quartile3",
                "percent90","maximum","range","stdev","missing")
  x3 <-list(1,1.9,3.25,5.5,5.5,7.75,9.1,10,9,sd(c(1,4,7,10)),0)
  names(x3) <- c("minimum","percent10","quartile1","median","mean","quartile3",
                "percent90","maximum","range","stdev","missing")
  x4 <-list(0,1,2.5,5,5,7.5,9,10,10,sd(c(0,1,2,3,4,5,6,7,8,9,10)),1)
  names(x4) <- c("minimum","percent10","quartile1","median","mean","quartile3",
                 "percent90","maximum","range","stdev","missing")
  expect_equal(summary_stats(a1), x1)
  expect_equal(summary_stats(a2), x2)
  expect_equal(summary_stats(a3), x3)
  expect_equal(summary_stats(a4), x4)
})

context("rescale100")
test_that('rescale is correct',{
  a <- c(18,15,16,4,17,9)
  a1 <- c(10,10,10,10,20)
  a2 <- c(2,4,6)
  a3 <- c(100,30,100)
  expect_equal(rescale100(a, xmin = 0, xmax = 20),c(90,75,80,20,85,45))
  expect_equal(rescale100(a1, xmin = 0, xmax = 20),c(50,50,50,50,100))
  expect_equal(rescale100(a2, xmin = 0, xmax = 10),c(20,40,60))
  expect_equal(rescale100(a3, xmin = 0, xmax = 100),c(100,30,100))
})

context("drop_lowest")
test_that('drop_lowest is correct',{
  a <- c(18,15,16,4,17,9)
  a1 <- c(18,10,10,2,8)
  a2 <- c(20,20,10,1,4)
  a3 <- c(6,2,1,6)
  expect_equal(drop_lowest(a),c(18,15,16,17,9))
  expect_equal(drop_lowest(a1),c(18,10,10,8))
  expect_equal(drop_lowest(a2),c(20,20,10,4))
  expect_equal(drop_lowest(a3),c(6,2,6))
})

context("score_homework")
test_that('score_homework is correct',{
  a <- c(100,80,30,70,75,85)
  a1 <- c(0,0,0,0,100)
  a2 <- c(100,80,100,40)
  expect_equal(score_homework(a, drop = TRUE),82)
  expect_equal(score_homework(a, drop = FALSE),(1/3)+73)
  expect_equal(score_homework(a1, drop = TRUE),25)
  expect_equal(score_homework(a2, drop = TRUE),(1/3)+93)
})

context("score_quiz")
test_that('score_quiz is correct',{
  a <- c(100,80,70,0)
  a1 <- c(100,100,100,0)
  a2 <- c(50,60,100,25)
  expect_equal(score_quiz(a, drop = TRUE),83+(1/3))
  expect_equal(score_quiz(a, drop = FALSE),62.5)
  expect_equal(score_quiz(a1, drop = TRUE),100)
  expect_equal(score_quiz(a2, drop = TRUE),70)
})

context("score_lab")
test_that('score_lab is correct',{
  expect_equal(score_lab(12),100)
  expect_equal(score_lab(11),100)
  expect_equal(score_lab(10),80)
  expect_equal(score_lab(9),60)
  expect_equal(score_lab(8),40)
  expect_equal(score_lab(7),20)
  expect_equal(score_lab(6),0)
  expect_equal(score_lab(2),0)
})




