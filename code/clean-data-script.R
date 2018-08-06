# ===================================================================
# Title: clean-data-script.R
# Description:
#   This script cleans up the raw data given, rawscores.csv
# Author: Yoon Sung Hong
# Date: 11-14-2017
# ===================================================================

#loading necessary packages and sourcing
library(stringr)
source("functions.R")

dat <- read.csv("../data/rawdata/rawscores.csv", stringsAsFactors = F)
dat[,10] <- as.numeric(dat[,10])
dat[,12] <- as.numeric(dat[,12])
dat[,14] <- as.numeric(dat[,14])

#sinking the structure
sink('../output/summary-rawscores.txt')
str(dat)
print_stats(summary_stats(dat[,1]))
print_stats(summary_stats(dat[,2]))
print_stats(summary_stats(dat[,3]))
print_stats(summary_stats(dat[,4]))
print_stats(summary_stats(dat[,5]))
print_stats(summary_stats(dat[,6]))
print_stats(summary_stats(dat[,7]))
print_stats(summary_stats(dat[,8]))
print_stats(summary_stats(dat[,9]))
print_stats(summary_stats(dat[,10]))
print_stats(summary_stats(dat[,11]))
print_stats(summary_stats(dat[,12]))
print_stats(summary_stats(dat[,13]))
print_stats(summary_stats(dat[,14]))
print_stats(summary_stats(dat[,15]))
print_stats(summary_stats(dat[,16]))
sink()

#removing NA
for(i in 1:ncol(dat)) {
  for(j in 1:nrow(dat)) {
    if(is.na(dat[j,i]) == TRUE) {
      dat[j,i] <- 0
    }
  }
}

#rescaling
dat$QZ1 <- rescale100(dat$QZ1, xmin = 0, xmax = 12)
dat$QZ2 <- rescale100(dat$QZ2, xmin = 0, xmax = 18)
dat$QZ3 <- rescale100(dat$QZ3, xmin = 0, xmax = 20)
dat$QZ4 <- rescale100(dat$QZ4, xmin = 0, xmax = 20)
dat$Test1 <- rescale100(dat$EX1, xmin = 0, xmax = 80)
dat$Test2 <- rescale100(dat$EX2, xmin = 0, xmax = 90)
#Homework
dat$Homework <- rep(0, nrow(dat))
for(i in 1:nrow(dat)) {
  vec <- c(dat$HW1[i], dat$HW2[i], dat$HW3[i],
           dat$HW4[i], dat$HW5[i], dat$HW6[i],
           dat$HW7[i], dat$HW8[i], dat$HW9[i])
  dat$Homework[i] <- score_homework(vec, drop = TRUE)
}
#Quiz
dat$Quiz <- rep(0, nrow(dat))
for(i in 1:nrow(dat)) {
  vec <- c(dat$QZ1[i], dat$QZ2[i], dat$QZ3[i],
           dat$QZ4[i])
  dat$Quiz[i] <- score_quiz(vec, drop = TRUE)
}
#lab
dat$Lab <- rep(0, nrow(dat))
for(i in 1:nrow(dat)) {
  vec3 <- dat[i,10]
  dat$Lab[i] <- score_lab(vec3)
}

#Overall
dat$Overall <- (0.1*dat$Lab) + (0.3*dat$Homework) + 
  (0.15*dat$Quiz) + (0.2*dat$Test1) + (0.25*dat$Test2)
#Grades
dat$Grade <- rep(0,nrow(dat))
for(i in 1:nrow(dat)) {
  if(dat$Overall[i] <50 & dat$Overall[i] >= 0) {
    dat$Grade[i] <- "F"
  }
  else if(dat$Overall[i] <60 & dat$Overall[i] >= 50) {
    dat$Grade[i] <- "D"
  }
  else if(dat$Overall[i] <70 & dat$Overall[i] >= 60) {
    dat$Grade[i] <- "C-"
  }
  else if(dat$Overall[i] <77.5 & dat$Overall[i] >= 70) {
    dat$Grade[i] <- "C"
  }
  else if(dat$Overall[i] <79.5 & dat$Overall[i] >= 77.5) {
    dat$Grade[i] <- "C+"
  }
  else if(dat$Overall[i] <82 & dat$Overall[i] >= 79.5) {
    dat$Grade[i] <- "B-"
  }
  else if(dat$Overall[i] <86 & dat$Overall[i] >= 82) {
    dat$Grade[i] <- "B"
  }
  else if(dat$Overall[i] <88 & dat$Overall[i] >= 86) {
    dat$Grade[i] <- "B+"
  }
  else if(dat$Overall[i] <90 & dat$Overall[i] >= 88) {
    dat$Grade[i] <- "A-"
  }
  else if(dat$Overall[i] <95 & dat$Overall[i] >= 90) {
    dat$Grade[i] <- "A"
  }
  else if(dat$Overall[i] <=100 & dat$Overall[i] >= 95) {
    dat$Grade[i] <- "A+"
  }
}

#sinking again
savepath <- "../output/"
for(i in 1:6){
n <- i+16
sink(paste0(savepath, names(dat[n]), '-', 'stats', '.txt'))
str(dat[,n])
print_stats(summary_stats(dat[,n]))
sink()
}

#sinking the structure of the clean scores
sink('../output/summary-cleanscores.txt')
str(dat)
sink()

#exporting the clean data frame of scores
write.csv(dat, file = "../data/cleandata/cleanscores.csv", row.names = FALSE)

