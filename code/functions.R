# ===================================================================
# Title: functions.R
# Description:
#   This script creates necessary functions for hw04
# Author: Yoon Sung Hong
# Date: 11-07-2017
# ===================================================================

library(stringr)

#' remove_missing()
#' @param x A vector.
#' @return The input vector without missing values
remove_missing <- function(x) {
     if(class(x) == "numeric") {
       x[!is.na(x)]
     }
}
remove_missing(c(1,NA,2,2,NA))

#' get_minimum()
#' @param x A vector.
#' @return minimum value
get_minimum <- function(x, na.rm = TRUE) {
  if(na.rm == TRUE) {
    x <- sort(remove_missing(x))
    return(x[1])
  }
  else {
    x <- sort(x)
  return(x[1])
  }
}
get_minimum(c(1,2,NA,10,NA))

#' get_maximum()
#' @param x A vector.
#' @return maximum value
get_maximum <- function(x, na.rm = TRUE) {
  if(na.rm == TRUE) {
    x <- sort(remove_missing(x), decreasing = TRUE)
    return(x[1])
  }
  else {
    x <- sort(x, decreasing = TRUE)
    return(x[1])
  }
}
get_maximum(c(1,2,5,5,NA,10,NA))

#' get_range()
#' @param x A vector.
#' @return range: maximum value minus the minimum value
get_range <- function(x, na.rm = TRUE) {
  if(class(x) == "numeric" & na.rm == TRUE){
  a <- get_maximum(remove_missing(x))
  b <- get_minimum(remove_missing(x))
  return(a-b)
  }
  else if(class(x) == "numeric" & na.rm == FALSE) {
    a <- get_maximum(x)
    b <- get_minimum(x)
    return(a-b)
  }
  else {
    stop("non-numeric argument")
  }
}
get_range(c(3,5,5,5,NA,10,NA))

#' get_percentile10()
#' @param x A vector.
#' @return 10th percentile value
get_percentile10 <- function(x, na.rm = TRUE) {
  if(class(x) == "numeric" & na.rm == TRUE) {
    return((quantile(remove_missing(x), 0.1))[[1]])
  }
  else if(class(x) != "numeric") {
    stop("non-numeric argument")
  }
  else if(na.rm == FALSE) {
    return((quantile(x, 0.1))[[1]])
  }
}

#' get_percentile90()
#' @param x A vector.
#' @return 90th percentile value
get_percentile90 <- function(x, na.rm = TRUE) {
  if(class(x) == "numeric" & na.rm == TRUE) {
    x <- ((quantile(remove_missing(x), 0.9))[[1]])
    return(x)
  }
  else if(class(x) != "numeric") {
    stop("non-numeric argument")
  }
  else if(na.rm == FALSE) {
    return((quantile(x, 0.9))[[1]])
  }
}
get_percentile90(c(1,4,7,NA,10))

#' get_median()
#' @param x A vector.
#' @return median value
get_median <- function(x, na.rm = TRUE) {
  if(class(x) == "numeric" & na.rm == TRUE) {
  x <- ((quantile(remove_missing(x),0.5))[[1]])
  return(x)
  }
  else if(class(x) != "numeric") {
    stop("non-numeric argument")
  }
  else if(na.rm == FALSE) {
    return((quantile(x, 0.5))[[1]])
  }
}

#' get_average()
#' @param x A vector.
#' @return average value
get_average <- function(x, na.rm = TRUE) {
  av <- 0
  if(class(x) == "numeric" & na.rm == TRUE) {
    for(i in 1:length(remove_missing(x))) {
      av <- av + remove_missing(x)[i]
    }
    return(av/length(remove_missing(x)))
  }
  else if(class(x) != "numeric") {
    stop("non-numeric argument")
  }
  else if(na.rm == FALSE) {
    for(i in 1:length(x)) {
      av <- av + x[i]
    }
    return(av/length(x))
  }
}

#' get_stdev()
#' @param x A vector.
#' @return standard deviation value
get_stdev <- function(x, na.rm = TRUE) {
  stdev <- 0
  ag <- get_average(x)
  if(class(x) == "numeric" & na.rm == TRUE) {
    for(i in 1:length(remove_missing(x))) {
      stdev <- stdev + ((remove_missing(x)[i]-ag)^2)
    }
    return(sqrt(stdev/(length(remove_missing(x))-1)))
  }
  else if(class(x) != "numeric") {
    stop("non-numeric argument")
  }
  else if(na.rm == FALSE) {
    for(i in 1:length(x)) {
      stdev <- stdev + ((x[i]-ag)^2)
    }
    return(sqrt(stdev/(length(x)-1)))
  }
}

#' get_quartile1()
#' @param x A vector.
#' @return 1st quartile value
get_quartile1 <- function(x, na.rm = TRUE) {
  if(class(x) == "numeric" & na.rm == TRUE) {
    return((quantile(remove_missing(x)))[[2]])
  }
  else if(class(x) != "numeric") {
    stop("non-numeric argument")
  }
  else if(na.rm == FALSE) {
    return((quantile(x))[[2]])
  }
}

#' get_quartile3()
#' @param x A vector.
#' @return 3rd quartile value
get_quartile3 <- function(x, na.rm = TRUE) {
  if(class(x) == "numeric" & na.rm == TRUE) {
    return((quantile(remove_missing(x)))[[4]])
  }
  else if(class(x) != "numeric") {
    stop("non-numeric argument")
  }
  else if(na.rm == FALSE) {
    return((quantile(x))[[4]])
  }
}

#' count_missing()
#' @param x A vector.
#' @return number of NAs in the vector
count_missing <- function(x) {
  if(class(x) == "numeric") {
    sum(is.na(x))
  }
  else if(class(x) != "numeric") {
    return("non-numeric argument")
  }
}

#' summary_stats()
#' @param x A numeric vector.
#' @return list of summary statistics
summary_stats <- function(x){
  summary <- list(get_minimum(x),get_percentile10(x),get_quartile1(x),
                  get_median(x),get_average(x),get_quartile3(x),get_percentile90(x),
                  get_maximum(x),get_range(x),get_stdev(x),count_missing(x))
  names(summary) <- c("minimum","percent10","quartile1","median","mean","quartile3",
                      "percent90","maximum","range","stdev","missing")
  return(summary)
}

#' print_stats()
#' @param x A list of summary statistics.
#' @print values of summary statistics in nice format
print_stats <- function(x) {
  for (i in 1:length(x)) {
    name = names(x)[i]
    len = 9 - nchar(name)
    space = ""
    if (len > 1) {
      for (j in 1:len) {
        space = paste("", space)
      }
    } 
    p_stat = noquote(paste0(names(x)[i], space, ": ", 
                            format(round(x[[i]], 4), nsmall = 4)))
    print(p_stat)
  }
}

#' rescale100()
#' @param x A numeric vector.
#' @return rescaled vector with potential scale from 0 to 100
rescale100 <- function(x, xmin, xmax) {
  scalex <- (100*((x-xmin)/(xmax-xmin)))
  return(scalex)
}

#' drop_lowest()
#' @param x A vector of length n.
#' @return a vector of length n-1 by dropping the lowest value
drop_lowest <- function(x) {
  lowest <- which.min(x)
  lx <- x[-c(lowest)]
  return(lx)
}

#' score_homework()
#' @param x A vector.
#' @return a single homework value
score_homework <- function(x, drop = TRUE) {
  if(drop == TRUE) {
    scorelistx <- drop_lowest(x)
    hwavg <- sum(scorelistx)/length(scorelistx)
    return(hwavg)
  }
  else if(drop == FALSE) {
    hwavg <- sum(x)/length(x)
    return(hwavg)
  }
}

#' score_quiz()
#' @param x A vector.
#' @return average of the quiz scores
score_quiz <- function(x, drop = TRUE) {
  if(drop == TRUE) {
    quizlistx <- drop_lowest(x)
    quizavg <- sum(quizlistx)/length(quizlistx)
    return(quizavg)
  }
  else if(drop == FALSE) {
    quizavg <- sum(x)/length(x)
    return(quizavg)
  }
}

#' score_lab()
#' @param x numeric value of lab attendance.
#' @return lab score out of 12
score_lab <- function(x) {
  if(11==x | x==12) {
    lab <- 100
  }
  else if(x == 10) {
    lab <- 80
  }
  else if(x == 9) {
    lab <- 60
  }
  else if(x == 8) {
    lab <- 40
  }
  else if(x == 7) {
    lab <- 20
  }
  else if(x <=6) {
    lab <- 0
  }
  return(lab)
}




