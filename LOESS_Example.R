setwd("C:/CMDA 4654/Project1")
#install.packages("readxl")
library("readxl")
sample <- read_excel("Sample1.xlsx")

# https://codeshare.io/axl4oj
# https://www.itl.nist.gov/div898/handbook/pmd/section1/dep/dep144.htm
# https://github.com/SurajGupta/r-source/blob/master/src/library/stats/man/loess.Rd
# https://www.statsdirect.com/help/nonparametric_methods/loess.htm

# span/ q (smoothing parameter)/ alpha
span = 1/3

# q = 0.33 according to website (7 data points per window/ 21 data points in all dataset  = 0.33)

# total number of points in dataset
N_total <- length(sample$x)
N_total

# number of points in each window
n_points <- N_total*span
n_points

# total number of windows
Win_total <- N_total
Win_total


# # Subset data to match table
# for (i in 1:n_points){
#   subset <- as.data.frame(sample[i,])
# }
# 
# # Point of Estimation For-Loop
# for (i in 1:N_total){
#   PointOfEstimation = x[i]
# }
# 
# PointOfEstimation[i] <- x[i]
# Distance[i] <- x[i+(n_points-(n_points))] - PointOfEstimation[i]
# Distance[i+1] <- x[i+(n_points-(n_points-1))]  - PointOfEstimation[i]
# Distance[i+2] <- x[i+(n_points-(n_points-2))] - PointOfEstimation[i]
# Distance[i+3] <- x[i+(n_points-(n_points-3))] - PointOfEstimation[i]
# Distance[i+4] <- x[i+(n_points-(n_points-4))] - PointOfEstimation[i]
# Distance[i+5] <- x[i+(n_points-(n_points-5))] - PointOfEstimation[i]
# Distance[i+6] <- x[i+(n_points-(n_points-6))] - PointOfEstimation[i]

## Key points
#distanceArray <- c(Distance1,Distance2,Distance3,Distance4,Distance5,Distance6,Distance7)
#maxDistance <- abs(max(subset1$x)-min(subset1$x))
#scaledDistanceArray <- distanceArray/maxDistance
#weightArray <- (1 - abs(scaledDistanceArray)^3)^3



# Subset data to match table
subset1 <- as.data.frame(sample[1:7,])
subset2 <- as.data.frame(sample[1:7,])
subset3 <- as.data.frame(sample[1:7,])
subset4 <- as.data.frame(sample[1:7,])
subset5 <- as.data.frame(sample[2:8,])
subset6 <- as.data.frame(sample[3:9,])
subset7 <- as.data.frame(sample[4:10,])

# Point of Estimation
PointOfEstimation1 <- subset1$x[1]
PointOfEstimation2 <- subset2$x[2]
PointOfEstimation3 <- subset3$x[3]
PointOfEstimation4 <- subset4$x[4]
PointOfEstimation5 <- subset5$x[4]
PointOfEstimation6 <- subset6$x[4]
PointOfEstimation7 <- subset7$x[4]
# Should go all the way to PointOfEstimation21
PointOfEstimation1
PointOfEstimation2
PointOfEstimation3
PointOfEstimation4
PointOfEstimation5
PointOfEstimation6
PointOfEstimation7

# x
subset1$x[1]
subset1$x[2]
subset1$x[3]
subset1$x[4]
subset1$x[5]
subset1$x[6]
subset1$x[7]

# y
subset1$y[1]
subset1$y[2]
subset1$y[3]
subset1$y[4]
subset1$y[5]
subset1$y[6]
subset1$y[7]

# Distance
Distance1 <- subset1$x[1]-PointOfEstimation1
Distance2 <- subset1$x[2]-PointOfEstimation1
Distance3 <- subset1$x[3]-PointOfEstimation1
Distance4 <- subset1$x[4]-PointOfEstimation1
Distance5 <- subset1$x[5]-PointOfEstimation1
Distance6 <- subset1$x[6]-PointOfEstimation1
Distance7 <- subset1$x[7]-PointOfEstimation1
Distance1
Distance2
Distance3
Distance4
Distance5
Distance6
Distance7
# Distance <- subset1$x[i+6] - PointOfEstimation[i]
distanceArray <- c(Distance1,Distance2,Distance3,Distance4,Distance5,Distance6,Distance7)
distanceArray

# Maximum Distance when span < 1
maxDistance <- abs(max(subset1$x)-min(subset1$x))
maxDistance

# Maximum Distance when span > 1, our span will be less than 1, so no need to do this

# Scaled Distance (Distance/Maximum Distance) = u
ScaledDistance1 <- Distance1/maxDistance
ScaledDistance2 <- Distance2/maxDistance
ScaledDistance3 <- Distance3/maxDistance
ScaledDistance4 <- Distance4/maxDistance
ScaledDistance5 <- Distance5/maxDistance
ScaledDistance6 <- Distance6/maxDistance
ScaledDistance7 <- Distance7/maxDistance
ScaledDistance1
ScaledDistance2
ScaledDistance3
ScaledDistance4
ScaledDistance5
ScaledDistance6
ScaledDistance7
scaledDistanceArray <- distanceArray/maxDistance
scaledDistanceArray

# u always has to be positive because distance/maxDistance is always < 1
# Weight (Tukey tri-weight formula)
Weight1 <- (1 - abs(Distance1/maxDistance)^3)^3
Weight2 <- (1 - abs(Distance2/maxDistance)^3)^3
Weight3 <- (1 - abs(Distance3/maxDistance)^3)^3
Weight4 <- (1 - abs(Distance4/maxDistance)^3)^3
Weight5 <- (1 - abs(Distance5/maxDistance)^3)^3
Weight6 <- (1 - abs(Distance6/maxDistance)^3)^3
Weight7 <- (1 - abs(Distance7/maxDistance)^3)^3
Weight1
Weight2
Weight3
Weight4
Weight5
Weight6
Weight7
weightArray <- (1 - abs(scaledDistanceArray)^3)^3
weightArray

# Local Parameter Estimates
wls <- lm(subset1$y~subset1$x, weights=weightArray)
Reg_result <- wls$coefficients[2] * subset1$x + wls$coefficients[1] 
Reg_result[1]

#SSE - sum(actual - predicted^2)
SSE <- sum((lm(subset1$y ~ subset1$x)$residuals^2))
SSE

  
# Regression Function Value (y=mx+b)
# Slope * Point Of Estimation + Intercept

Ltest <- loess(sample$y~sample$x, span = 1/3, degree=1)


