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

# STEPS OF LOESS fit/function
# 1. determine the distance from each point to the point of estimation
# 2. scale the distances by the maximum distance over all points in the local data set, and
# 3. compute the weights by evaluating the tricube weight function using the scaled distances.

# Tukey tri-cube weight function
# if (abs(x) <= 1) {
#   y = (1 - abs(x)^3)^3
# } else if (abs(x) > 1){
#   y = 0
# }

myloess <- function(x, y, span = 0.5, degree = 1, show.plot = TRUE){
  # https://www.itl.nist.gov/div898/handbook/pmd/section1/dep/dep144.htm
  # https://www.statsdirect.com/help/nonparametric_methods/loess.htm
  
  # total number of points in dataset
  N_total <- length(x)
  
  # number of points in each window
  n_points <- floor(N_total*span)
  
  # total number of windows
  Win_total <- N_total
  
  # median of window size, used to calculate subsets
  center <- ceiling(n_points/2)
  
  # create an empty vector for the regression result
  Reg_result <- vector(length = N_total)
  
  # Create empty vector for subset_x
  subset_x <-  c(x[1],x[n_points] )
  
  # Create empty vector for subset_y
  subset_y <-  c(y[1],y[n_points] )
  
  # Subset data before it reaches median (1:center)
  for (i in 1:n_points){ 
    subset_x[i] <- x[i] 
    subset_y[i] <-y[i]
  }
  
  # Create empty vector for Distance1
  Distance1 <-  vector(length = n_points)  
  
  # Create empty vector for scaledDistance1
  scaledDistance1 <-  vector(length = n_points)
  
  # Create empty vector for weight1
  weight1 <-  vector(length = n_points)
  
  # Max Distance (constant)
  maxDistance1 <- abs(max(subset_x)-min(subset_x))
  
  for (j in 1:(center-1))
  {
    for (k in 1:n_points) # 1:points in window(55)
    {
      Distance1[k] <- subset_x[k]-subset_x[j]
      scaledDistance1[k]<-Distance1[k]/maxDistance1
      weight1[k] <- (1 - abs(scaledDistance1[k])^3)^3
    }
    # find the wls, outputs: intercept = $coefficients[1] and slope = $coefficients[2]
    wls1 <- lm(subset_y~subset_x,weights=weight1)
    
    # Regression Function Value (Slope * Point of Estimation + Intercept)
    Reg_result[j] <-wls1$coefficients[2] * subset_x[j] + wls1$coefficients[1]
    
    # ggplot 
    # p + geom_line(x= subset_x[i], y=Reg_result[j])
  }
  
  for (n in center:(N_total - center + 1)) # Points of Estimation
  {
    # when point of estimate stays in the middle of the local dataset
    arrayCounter <- 1
    
    # Create empty vector for subset2_x
    subset2_x <-  c(x[n - center +1],x[n + center -1])
    
    # Create empty vector for subset2_y
    subset2_y <-  c(y[n - center +1],y[n + center -1])
    
    # Create empty vector for Distance2
    Distance2 <-  vector(length = n_points)  
    
    # Create empty vector for scaledDistance2
    scaledDistance2 <-  vector(length = n_points)
    
    # Create empty vector for weight2
    weight2 <-  vector(length = n_points)
    
    # MaxDistance changes
    maxDistance2 <- abs(max(subset2_x) - min(subset2_x)) 
    
    # create the data for local window (always changing until it hits )
    for (m in (n - center +1):(n + center -1))
    {
      Distance2[arrayCounter] <- subset2_x[m]-subset2_x[n]
      scaledDistance2[arrayCounter]<-Distance2[arrayCounter]/maxDistance2
      weight2[arrayCounter] <- (1 - abs(scaledDistance2[arrayCounter])^3)^3
      arrayCounter <- arrayCounter + 1
    }
    # find the wls, outputs: intercept = $coefficients[1] and slope = $coefficients[2]
    wls2 <- lm(subset2_y~subset2_x,weights=weight2)
    
    # Regression Function Value (Slope * Point of Estimation + Intercept)
    Reg_result[n] <-wls2$coefficients[2] * subset2_x[n] + wls2$coefficients[1]
  }
  # Graph
  # You need to use print() in order to use ggplot inside a for loop
  # using geomline? To create a local linear regression fit
  # p + geom_line(x= subset2_x[m], y=Reg_result[n]))
  
  
  arrayCounter1 <- 1
  
  # Create empty vector for Distance3
  Distance3 <-  vector(length = n_points)  
  
  # Create empty vector for scaledDistance3
  scaledDistance3 <-  vector(length = n_points)
  
  # Create empty vector for weight3
  weight3 <-  vector(length = n_points)
  
  # Create empty vector for subset3_x
  subset3_x <-  c(x[1],x[n_points] )
  
  # Create empty vector for subset3_y
  subset3_y <-  c(y[1],y[n_points] )
  
  # Subset data before it reaches median (1:center)
  for (ii in (N_total - n_points):N_total){ 
    subset3_x[ii] <- x[ii] 
    subset3_y[ii] <-y[ii]
  }
  maxDistance3 <- abs(max(subset3_x)-min(subset3_x)) # Max Distance (constant)
  for (jj in (N_total - center):N_total)
  {
    for (kk in (N_total - center):N_total)
    {
      Distance3[arrayCounter1] <- subset3_x[kk]-subset3_x[jj]
      scaledDistance3[arrayCounter1] <- Distance3[arrayCounter1]/maxDistance3
      weight3[arrayCounter1] <- (1 - abs(scaledDistance3[arrayCounter1])^3)^3
      arrayCounter1 <- arrayCounter1 + 1
    }
    # find the wls, outputs: intercept = $coefficients[1] and slope = $coefficients[2]
    wls3 <- lm(subset3_y~subset3_x,weights=weight3)
    
    # Regression Function Value (Slope * Point of Estimation + Intercept)
    Reg_result[jj] <-wls3$coefficients[2] * subset3_x[jj] + wls3$coefficients[1]
  }
  
  # Graph
  # You need to use print() in order to use ggplot inside a for loop
  # using geomline? To create a local linear regression fit
  #p + geom_line(x= subset3_x[ii], y=Reg_result[jj]))
  
  # https://rpubs.com/Anil_2498/497822
  # Error Sum of Squares (Tells us how good of a fit we had) = sum(actual - predicted^2)
  SSE <- sum((lm( Reg_result ~ x)$residuals^2))
  # SSE <- sum(resid(lm(Reg_result ~ x))^2) gets the same answer
  
  # Residual Standard Error (RSE)
  RSE <- sigma(lm(Reg_result ~ y))
  
  # get smoothed output
  smoothed <- predict(Reg_result) 
  
  # basic plot with original x and y from the dataset
  loessplot <- plot(y ~ x, type="l", main="Loess Smoothing and Prediction, degree = 1")
  lines(smoothed, x)
  
  
  return(span, degree, N_total, Win_total, n_points, SSE, loessplot)
}


myloess(ozone$temperature, ozone$ozone, span = 0.5, degree = 1, show.plot = TRUE)




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
  
  train <- data[train,]
  test <- data[test,]
  
  # cl - factor of true classification of training set
  trainCl <- factor(data[train,'classifications'])
  testCl <- factor(data[test,'classifications'])
  
  # knnPred
  knn <- 
  
  Knn_pred <-
  acutal_test <-
    
  # Confusion Matrix
  tab <- table(knn_pred, testCl)
  
  ## the normalization function is created (Euclidean distance)
  norm <-function(x) { 
    (x -min(x))/(max(x)-min(x))
  }
  
  accuracy <- function(x){
    sum(diag(x)/(sum(rowSums(x)))) * 100
  }
  
  # Make sure the levels of pred matches the levels of Default$default
  # or else you need to relevel
  
  # accuracy of kNN's prediction
  accuracy <- mean(knn_pred == actual_test)
  
  # Average Misclassification Rate/ error rate
  # mean(knn_pred != actual_test)
  err <- 1 - accuracy 
  # outputs a decimal number ex. 0.028
  
  return (knn_pred, accuracy, err, confusionMatrix, k)
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
# https://rpubs.com/Anil_2498/497822







