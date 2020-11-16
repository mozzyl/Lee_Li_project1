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
  
  
  # total number of points in dataset
  N_total <- length(x)
  #N_total <- nrow(x)*ncol(x)
  
  # number of points in each window
  n_points <- N_total*span
  
  # total number of windows
  Win_total <- N_total/n_points
  
  # So, we know that x and y are just two different columns of the dataset
  # If we look at one of the links, there is an example with loess using a data set with x and y
  # The steps they use for the LOESS function is below
  # I think we have to use Step 1 and 2 to change x into u (on the instruction pdf) which will be used
  # in the Tukey tri-cube weight function below?
  # I'm not sure, it's what I got from reading the external links references from the instructions.
  
  # STEPS OF LOESS fit/function
  # 1. determine the distance from each point to the point of estimation
  # 2. scale the distances by the maximum distance over all points in the local data set, and
  # 3. compute the weights by evaluating the tricube weight function using the scaled distances.
  
  
  # Tukey tri-cube weight function
  if (abs(x) <= 1) {
    y = (1 - abs(x)^3)^3
  } else if (abs(x) > 1){
    y = 0
  }
  
  # Error Sum of Squares (Tells us how good of a fit we had).
  sse <- sum((fitted(lm(y ~ x)) - mean(y))^2)

  # An object containing the ggplot so that we can see the plot later
  loessplot <- ggplot
    
    
    
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

  
  # In-Sample Confusion Matrix
  # Lecture 16, pg 78
  pihat_train <- predict(modfit_best, newdata = Default_train, type = "response")
  threshold <- 0.5 # We pick the threshold 0.5 = 50%
  predicted_cat_test <- factor( ifelse(pihat_test > threshold,
                                       "Yes",  # Success
                                       "No") ) # Failure
  # Lecture 16 video 4, 1st slide
  # We can make a table (acutal, predicted categories)
  
  
  # Make sure the levels of pred matches the levels of Default$default
  # or else you need to relevel

  # Average Misclassification Rate/ error rate
  err <- mean((as.numeric(Default_train$default)-1) != (pihat_train > 0.5))
  # outputs a decimal number ex. 0.028
  
  # accuracy
  accuracy <- 1 - err
  
  return (confusionMatrix)
  #return(list of objects seen below)
}

# You will return the following
# A categorical vector for the predicted categories for the testing data
# The accuracy of the classification
# The error rate = 1 - accuracy
# A confusion matrix
# The value of k used




# REFERENCES:
# https://rinterested.github.io/statistics/rsquare.html







