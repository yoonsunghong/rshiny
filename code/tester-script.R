# ===================================================================
# Title: tester-script.R
# Description:
#   This script sinks the tests
# Author: Yoon Sung Hong
# Date: 11-07-2017
# ===================================================================

# test script
library(testthat)

#source in functions to be tested
source('functions.R')

sink('../output/test-reporter.txt')
test_file('tests.R')
sink()