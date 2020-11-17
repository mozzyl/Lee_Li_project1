setwd("C:/CMDA 4654/Project1")
#install.packages("readxl")
library("readxl")
# Converted the .txt file into a .xlsx file
sample <- read_excel("Sample1.xlsx")

# In this R file, we basically tried to recreate to table from the following website (given):
# https://www.itl.nist.gov/div898/handbook/pmd/section1/dep/dep144.htm

# We are given q = 0.33, which is also the span (change to 1/3 to get accurate answer)
span = 1/3

# Upon initial observation of the table, we see that there are 21 data points and there are 7 points per window
# Therefore q=1/3 makes sense since (7 points per window/21 points total = 1/3 = span)

# total number of points in dataset (21)
N_total <- length(sample$x)
N_total

# number of points in each window (7)
n_points <- N_total*span
n_points

# total number of windows (21), there seems to be a window for every point in dataset
# since the Point of Estimation is using each of the datapoints
Win_total <- N_total
Win_total


# 
center <- ceiling(n_points/2)
center

# Subset data to match table
subset1 <- (sample[1:7,])
subset2 <- (sample[1:7,])
subset3 <- (sample[1:7,])
subset4 <- (sample[1:7,])
subset5 <- (sample[2:8,])
subset6 <- (sample[3:9,])
subset7 <- (sample[4:10,])
subset8 <- (sample[5:11,])
subset9 <- (sample[6:12,])
subset10 <- (sample[7:13,])
subset11 <- (sample[8:14,])
subset12 <- (sample[9:15,])
subset13 <- (sample[10:16,])
subset14 <- (sample[11:17,])
subset15 <- (sample[12:18,])
subset16 <- (sample[13:19,])
subset17 <- (sample[14:20,])
subset18 <- (sample[15:21,])
subset19 <- (sample[15:21,])
subset20 <- (sample[15:21,])
subset21 <- (sample[15:21,])
# this would contine to subset21
# We see that each window has 1:7 data points until it reaches center point as it's Point of Estimation
# The center is at 4
# We see a pattern where a window is shifted by one point after it hits the center
# Same pattern applies for the last 4 windows, where it uses the same set of data after it surpasses the center

# We use this logic to create for loop

# Point of Estimation (There is a Point of Estimation for each )
PointOfEstimation1 <- subset1$x[1]
PointOfEstimation2 <- subset2$x[2]
PointOfEstimation3 <- subset3$x[3]
PointOfEstimation4 <- subset4$x[4]
PointOfEstimation5 <- subset5$x[4]
PointOfEstimation6 <- subset6$x[4]
PointOfEstimation7 <- subset7$x[4]
PointOfEstimation8 <- subset8$x[4]
PointOfEstimation9 <- subset9$x[4]
PointOfEstimation10 <- subset10$x[4]
PointOfEstimation11 <- subset11$x[4]
PointOfEstimation12 <- subset12$x[4]
PointOfEstimation13 <- subset13$x[4]
PointOfEstimation14 <- subset14$x[4]
PointOfEstimation15 <- subset15$x[4]
PointOfEstimation16 <- subset16$x[4]
PointOfEstimation17 <- subset17$x[4]
PointOfEstimation18 <- subset18$x[4]
PointOfEstimation19 <- subset19$x[5]
PointOfEstimation20 <- subset20$x[6]
PointOfEstimation21 <- subset21$x[7]

# x
subset1$x[1]
subset1$x[2]
subset1$x[3]
subset1$x[4]
subset1$x[5]
subset1$x[6]
subset1$x[7]
subset1$x[8]
subset1$x[9]
subset1$x[10]
subset1$x[11]
subset1$x[12]
subset1$x[13]
subset1$x[14]
subset1$x[15]
subset1$x[16]
subset1$x[17]
subset1$x[18]
subset1$x[19]
subset1$x[20]
subset1$x[21]

# y
subset1$y[1]
subset1$y[2]
subset1$y[3]
subset1$y[4]
subset1$y[5]
subset1$y[6]
subset1$y[7]
subset1$y[8]
subset1$y[9]
subset1$y[10]
subset1$y[11]
subset1$y[12]
subset1$y[13]
subset1$y[14]
subset1$y[15]
subset1$y[16]
subset1$y[17]
subset1$y[18]
subset1$y[19]
subset1$y[20]
subset1$y[21]

# DISTANCE for Subset/Window 1

# Distance
Distance1 <- subset1$x[1]-PointOfEstimation1
Distance2 <- subset1$x[2]-PointOfEstimation1
Distance3 <- subset1$x[3]-PointOfEstimation1
Distance4 <- subset1$x[4]-PointOfEstimation1
Distance5 <- subset1$x[5]-PointOfEstimation1
Distance6 <- subset1$x[6]-PointOfEstimation1
Distance7 <- subset1$x[7]-PointOfEstimation1

# Distance <- subset1$x[i+6] - PointOfEstimation[i]
distanceArray <- c(Distance1,Distance2,Distance3,Distance4,Distance5,Distance6,Distance7)
distanceArray

# Maximum Distance when span < 1
maxDistance <- abs(max(subset1$x)-min(subset1$x))
maxDistance

# Maximum Distance when span > 1, our span will be less than 1, so no need to do this

# Scaled Distance (Distance/Maximum Distance) = u
# ScaledDistance1 <- Distance1/maxDistance
# ScaledDistance2 <- Distance2/maxDistance
# ScaledDistance3 <- Distance3/maxDistance
# ScaledDistance4 <- Distance4/maxDistance
# ScaledDistance5 <- Distance5/maxDistance
# ScaledDistance6 <- Distance6/maxDistance
# ScaledDistance7 <- Distance7/maxDistance
scaledDistanceArray <- distanceArray/maxDistance
scaledDistanceArray

# u always has to be positive because distance/maxDistance is always < 1
# Weight (Tukey tri-weight formula)
# Weight1 <- (1 - abs(Distance1/maxDistance)^3)^3
# Weight2 <- (1 - abs(Distance2/maxDistance)^3)^3
# Weight3 <- (1 - abs(Distance3/maxDistance)^3)^3
# Weight4 <- (1 - abs(Distance4/maxDistance)^3)^3
# Weight5 <- (1 - abs(Distance5/maxDistance)^3)^3
# Weight6 <- (1 - abs(Distance6/maxDistance)^3)^3
# Weight7 <- (1 - abs(Distance7/maxDistance)^3)^3
weightArray <- (1 - abs(scaledDistanceArray)^3)^3
weightArray

# Local Parameter Estimates
wls <- lm(subset1$y~subset1$x, weights=weightArray)
wls

# Regression Function Values [1:7]
Reg_result <- wls$coefficients[2] * subset1$x + wls$coefficients[1] 
Reg_result[1]

# ALL Regression functions values (Put in manually, in order to test)
Reg_result_total <- c(20.59302, 107.1603, 139.7674, 174.263, 207.2334, 216.6616, 220.5445, 229.8607, 229.8347, 229.4301, 226.6045, 220.3904, 172.348, 163.8417, 161.849, 160.3351, 160.192, 161.0556, 227.34, 227.8985, 231.5586)
length(Reg_result_total)

# Residual Standard Error
fm <- lm(Reg_result_total ~ sample$x)
sigma(fm)

# https://rpubs.com/Anil_2498/497822
# Error Sum of Squares (Tells us how good of a fit we had) = sum(actual - predicted^2)
SSE <- sum(resid(fm)^2)
#SSE <- sum((lm(Reg_result_total ~ sample$x)$residuals^2))
SSE
  
# Regression Function Value (y=mx+b)
# Slope * Point Of Estimation + Intercept


## Key points
#distanceArray <- c(Distance1,Distance2,Distance3,Distance4,Distance5,Distance6,Distance7)
#maxDistance <- abs(max(subset1$x)-min(subset1$x))
#scaledDistanceArray <- distanceArray/maxDistance
#weightArray <- (1 - abs(scaledDistanceArray)^3)^3

# Using LOESS built-in Function (Test to see if we get the same graph)
Ltest <- loess(sample$y~sample$x, span = 1/3, degree=1)
smoothedTest <- predict(Ltest)
loessplotTest <- plot(sample$y ~ sample$x, type="l", main="Loess Smoothing and Prediction, degree = 1")
lines(smoothedTest, sample$x, col="red")

myloess(sample$y~sample$x, span = 1/3, degree=1, show.plot = TRUE)

 
