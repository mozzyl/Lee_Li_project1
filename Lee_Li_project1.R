library(ggplot2)
load("ozone.RData")
data(ozone)
ozone

# Part 1 LOESS/LOWESS Regression

# Note span and degree are shown with their default values.
# degree should be 1 or 2 only
# span can be any value in (0, 1) non-inclusive.

# Your function should return a named list containing the following:
# span: proportion of data used in each window (controls the bandwidth)
# degree: degree of polynomial
# N_total: total number of points in the data set
# Win_total: total number of windows
# n_points: number of points in each window in a vector
# SSE: Error Sum of Squares (Tells us how good of a fit we had).
# loessplot: An object containing the ggplot so that we can see the plot later.
# We want this even if show.plot = FALSE
# Note: you are NOT allowed to simply use stat_smooth() or geom_smooth() to have it automatically do LOESS.
# You should use geom_line() or similar to plot your final the LOESS curve.
# Make sure you can access the objects properly using the $ notation.

myloess <- function(x, y, span = 0.5, degree = 1, show.plot = TRUE){
  # x = x$ozone
  # y = y$ozone
  
  # N_total <- length(x)
  
  # total number of points in dataset
  N_total <- nrow(x)*ncol(x)
  
  # number of points in each window
  n_points <- N_total*span
  
  # total number of windows
  Win_total <- N_total/n_points
  
  # Tukey tri-cube weight function
  if (abs(x) <= 1) {
    y = (1 - abs(x)^3)^3
  } else if (abs(x) > 1){
    y = 0
  }
  
  # Error Sum of Squares (Tells us how good of a fit we had).
  fitNFLAnova <- anova(fitNFL)
  fitNFLAnova
  SSE <- 

  # An object containing the ggplot so that we can see the plot later
  loessplot
    
    
    
  # if (abs(x) <= 1) {
  #   y = (1 - abs(x)^3)^3
  # }
  # else
  #   y = 0
  
  
  
  return(span, degree, N_total, Win_total, n_points, SSE, loessplot)
}


# STEPS OF LOESS fit/function
# 1. determine the distance from each point to the point of estimation
# 2. scale the distances by the maximum distance over all points in the local data set, and
# 3. compute the weights by evaluating the tricube weight function using the scaled distances.




# Part 2 kNN function

# Algorithm

#1. Calculate the distance between each training data point and the data point we're examining.

#2. Identify the first k data points with the smallest distance to our data point of interest.

#3. Find the label which occurs the most out of our k closest training data points (aka majority label). This is the label that you will assign to this observation.


# Your function will have the following inputs similar to what you would find with the
# knn() function

# train - matrix or data frame of training set cases
# test - matrix or data frame of test set cases.
# A vector will be interpreted as a row vector for a single case.
# cl - factor of true classification of training set
# k - number of neighbors considered, the default value is 3

# Lecture 16 pg 78
# install.packages('caret')
library(caret)

mykNN <- function(train, test, cl, k = 3) {
  # Your code goes here
  
  # In-Sample Confusion Matrix
  # Lecture 16, pg 78
  pihat_train <- predict(modfit_best, newdata = Default_train, type = "response")
  threshold <- 0.5
  predicted_category <- factor(ifelse(pihat_train > threshold, "Yes","No"))
  confusionMatrix <- confusionMatrix(data = predicted_category, reference = (Default_train$default))
  
  
  return (confusionMatrix)
  #return(list of objects seen below)
}

# You will return the following
# A categorical vector for the predicted categories for the testing data
# The accuracy of the classification
# The error rate = 1 - accuracy
# A confusion matrix
# The value of k used
