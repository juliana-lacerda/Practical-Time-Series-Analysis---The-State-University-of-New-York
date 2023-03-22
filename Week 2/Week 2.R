setwd("D:/Dropbox/Data Science/Coursera/Practical Time Series Analysis R - The State University of New York")



# ---------------------------------------------------------------------------- #
#                              Using ACF and PACF                              #
# ---------------------------------------------------------------------------- #

acf(x, lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE, ...)

pacf(x, lag.max, plot, na.action, ...)



# ---------------------------------------------------------------------------- #
#                               MA(q) Simulation                               #
# ---------------------------------------------------------------------------- #


# Generate noise
noise=rnorm(10000)

# Introduce a variable
ma_2=NULL

# Loop for generating MA(2) process

for(i in 3:10000){
    ma_2[i]=noise[i]+0.7*noise[i-1]+0.2*noise[i-2]
}

# Shift data to left by 2 units
moving_average_process=ma_2[3:10000]

# Put time series structure on a vanilla data
moving_average_process=ts(moving_average_process)

# Partition output graphics as a multi frame of 2 rows and 1 column
par(mfrow=c(2,1))

# plot the process and plot its ACF
plot(moving_average_process, main='A moving average process of order 2', ylab=' ', col='blue')
acf(moving_average_process, main='Correlogram of a moving average process of order 2')

# print the values of the graph
(acf(moving_average_process, main='Correlogram of a moving average process of order 2'))




# ---------------------------------------------------------------------------- #
#                              Test: Stationarity                              #
# ---------------------------------------------------------------------------- #



# -------------------------------- Question 5 -------------------------------- #
# consider the MA2 example 
# x_(t) = z_(t) + 0.5 * z_(t-1)+ 0.5 * z_(t-2), sigma^2=1

# What is the autocovariance at lag zero, that is calculate gamma(0)


# Generate noise
noise=rnorm(10000,sd=1)

# Introduce a variable
ma_2=NULL

# Loop for generating MA(2) process

for(i in 3:10000){
    ma_2[i]=noise[i]+0.5*noise[i-1]+0.5*noise[i-2]
}

# Shift data to left by 2 units
moving_average_process=ma_2[3:10000]
# Put time series structure on a vanilla data
moving_average_process=ts(moving_average_process)

# print the values of the graph
(acf(moving_average_process, type='covariance',plot=FALSE))

# Answer: 1.495849

# -------------------------------- Question 6 -------------------------------- #
# consider the MA2 example 
# x_(t) = z_(t) + 0.5 * z_(t-1)+ 0.5 * z_(t-2), sigma^2=1

# calculate the autocorrelation function at lag 2, that is rho(2)

# print the values of the graph
(acf(moving_average_process, type='correlation',plot=FALSE))

# -------------------------------- Question 7 -------------------------------- #
# Run the following code to simulate our MA(2) process as shown above.From your graph or the function output, estimate rho(1)
set.seed=1
acf(arima.sim(n=1000, model=list(ma=c(0.5,0.5))),type='correlation',plot=FALSE)
# Answer: 0.488
