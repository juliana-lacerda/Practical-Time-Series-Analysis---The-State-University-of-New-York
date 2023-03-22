setwd("D:/Dropbox/Data Science/Coursera/Practical Time Series Analysis R - The State University of New York/Semana 6")

# ---------------------------------------------------------------------------- #
#                 SARIMA simulation. SARIMA (0,0,1,0,0,1)_(12)                 #
# ---------------------------------------------------------------------------- #

# MA non seasonal, MA seasonal
# Model: X_t = ( 1 + PHI_1 B^(12) ) ( 1 + phi_1 B) Z_t

x=NULL
z=NULL
n=10000

z=rnorm(n)
x[1:13]=1

for(i in 14:n){
  x[i]<-z[i]+0.7*z[i-1]+0.6*z[i-12]+0.42*z[i-13]
}

par(mfrow=c(2,1))
plot.ts(x[12:120], main='The first 10 months of simulation SARIMA(0,0,1,0,0)_12', ylab='') 

acf(x, main='SARIMA(0,0,1,0,0,1)_12 Simulation')


# ---------------------------------------------------------------------------- #
#                              SARIMA fitting J&J                              #
# ---------------------------------------------------------------------------- #
# JJ: Quarterly earnings per share for the Johnson & Johnson Company
library(astsa)

d=1
DD=1

per=4

y = log(jj)

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=y, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals))) # Ljung–Box test
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

print("We choose as the best model: 0 1 1 1 1 0 4 AIC= -150.9134 SSE= 0.6251634  p-VALUE= 0.7079173 ")
print("Because it is the one with a low value of AIC that has high value of p-statistic")
print("p-statistic is measuring if the residuals present significant auto-correlation")
print("So resideuals can be considered white noise")

# fit the model

# Both lines do the same thing
arima(x=y, order = c(0,d,1), seasonal = list(order=c(1,DD,0), period=per))

# But this one plots a lot of information about the residuals
sarima(y,0,1,1,1,1,0,4)


# ------------------------- Forecast into the future ------------------------- #
model = arima(x=y, order = c(0,d,1), seasonal = list(order=c(1,DD,0), period=per))

y_pred = forecast(model)

plot(y_pred)


# ---------------------------------------------------------------------------- #
#                           SARIMA - Milk Production                           #
# ---------------------------------------------------------------------------- #

#The time series is downloaded from TSDL.
milk<-read.csv('monthly-milk-production-pounds-p.csv')
Milk<-milk$Pounds

sarima(Milk, 0,1,0,0,1,1,12)
library(astsa)
library(forecast)

d=NULL
DD=NULL

d=1
DD=1

per=12
for(p in 1:1){
  for(q in 1:1){
    for(i in 1:3){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=Milk, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
model<- arima(x=Milk, order = c(0,1,0), seasonal = list(order=c(0,1,1), period=12))
plot(forecast(model))
forecast(model)




# ---------------------------------------------------------------------------- #
#                      SARIMA -  Sales at a souvenir shop                      #
# ---------------------------------------------------------------------------- #

# The time series is downloaded from TSDL.
# https://datamarket.com/data/set/22mh/monthly-sales-for-a-souvenir-shop-on-the-wharf-at-a-beach-resort-town-in-queensland-australia-jan-1987-dec-1993#!ds=22mh&display=line

SUV = read.csv('monthly-sales-for-a-souvenir-sho.csv')
suv = ts(SUV$Sales)

library(astsa)
library(forecast)

par(mfrow=c(2,2))
plot(suv, main='Monthly sales for a souvenir shop', ylab='', col='blue', lwd=3)
plot(log(suv), main='Log-transorm of sales', ylab='', col='red', lwd=3)
plot(diff(log(suv)), main='Differenced Log-transorm of sales', ylab='', col='brown', lwd=3)
plot(diff(diff(log(suv)),12), main='Log-transorm without trend and seasonaliy', ylab='', col='green', lwd=3)

data = diff(diff((log(suv)),12))
acf2(data, 50)

d=1
DD=1
per=12
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(suv), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

model = arima(x=log(suv), order = c(1,1,0), seasonal = list(order=c(0,1,1), period=12))
plot(forecast(model))

forecast(model)

a = sarima.for(log(suv),12,1,1,0,0,1,1,12)

plot.ts(c(suv,exp(a$pred)), main='Monthly sales + Forecast', ylab='', col='blue', lwd=3)The time series is downloaded from TSDL.


# ---------------------------------------------------------------------------- #
#                         TEST - 'USAccDeaths' dataset                         #
# ---------------------------------------------------------------------------- #

# -------------------------------- Question 1 -------------------------------- #
# This Quiz has several Questions all of which are related and steps towards modeling the time series
#  titled 'USAccDeaths' in 'dataset' package in R.
# Plot the time series titled 'USAccDeaths' in the code block below.
rm(list=ls(all=TRUE))
data = USAccDeaths
plot(data)
print("It is a monthly time series with a span of seasonality 12. ")
print("Time series is not stationary since there is a seasonal trend.")


# -------------------------------- Question 2 -------------------------------- #
# We first get rid of the seasonal trend by differencing the values at the same month of each year.
# Plot the seasonally differenced time series in the code block below. 
data_seasonal_diff = diff(data,12)
plot(data_seasonal_diff)
print("There is a clear upward trend. ")


# -------------------------------- Question 3 -------------------------------- #
# We de-trend the seasonally differenced time series by taking non-seasonal differencing, diff(),
# and call the obtained time series 'acData'. Obtain ACF and PACF of 'acData' in the code block below.
# What do they suggest about the order of AR and seasonal AR terms?

acData = diff(data_seasonal_diff)
par(mfrow=c(3,1))
plot(acData)
acf(acData)
acf(acData, type='partial')

# ACF -> MA: non-seasonal q=0,1, seasonal Q=1 (S=12)
# PACF -> AR: non-seasonal p=1,2, seasonal P=1 (S=12)

print("Significant adjacent lags in PACF suggest the order of AR terms, p<=2")
print("The significant partial autocorrelation coefficient at lag 12 suggests the order of seasonal AR term, P<=1")


# -------------------------------- Question 4 -------------------------------- #
# What do they suggest about the order of MA and seasonal MA terms?
print("Significant adjacent lags in ACF suggest the order of MA terms, q<=1")
print("The significant autocorrelation coefficient at lag 12 suggests the order of seasonal MA term, Q<=1")

# -------------------------------- Question 5 -------------------------------- #
# We try few different models, and choose the model with smallest AIC: SARIMA(0,1,1,0,1,1)_12
# If X_t =USAccDeaths, which of the followings is/are the fitted model?

library(astsa)

# Both lines do the same thing
arima(x=data, order = c(0,1,1), seasonal = list(order=c(0,1,1), period=12))

# But this one plots a lot of information about the residuals
sarima(data,0,1,1,0,1,1,12)

ma1 = -0.4303
sma1 = -0.5528
sigmaZ_squared = 99347

# FINAL MODEL
# (1-B)(1-B^12)X_t = (1-ma1 B)(1- sma1 B^12)Z_t where sigmaZ_squared = 99347

# -------------------------------- Question 6 -------------------------------- #
# We carry residual analysis by using sarima() routine from 'astsa' package.
# What can be said about the residuals?
library(astsa)
sarima(data,0,1,1,0,1,1,12)

print("p-values from Ljung-Box test are high meaning that there is no significant autocorrelation left in the residuals. ")
print("ACF shows no significant autocorrelation in the residuals. ")
print("There is a systematic departure from linearity in QQ-plot which implies that residuals have a heavier tail compared to the Gaussian distribution. ")

# -------------------------------- Question 7 -------------------------------- #
# Obtain the p-values of the coefficients in the fitted model in the code block below. 
# What do they mean?
model<-sarima(USAccDeaths, 0,1,1,0,1,1,12)
print(model$ttable)
print("p-values are 0.0008 and 0.0028 for MA and seasonal MA coefficients, respectively.")
print("The fact that they are both less than any reasonable significant level, both coefficients (terms) are significant.")

# -------------------------------- Question 8 -------------------------------- #
# Use sarima.for() routine in the code block below to obtain the point forecast for the number
# of accidental deaths in the March of 1979. The answer is rounded.
print(data)
print("Data ends at dec 1978. So I have to forecast 3 dates at the future")
n_steps = 3
sarima.for(xdata=data, n.ahead = n_steps, p=0,d=1,q=1,P=0,D=1,Q=1,S=12)

sarima.for(xdata=data, n.ahead = n_steps, p=0,d=1,q=1,P=0,D=1,Q=1,S=12)$pred

print("8315")





# ---------------------------------------------------------------------------- #
#                       Introduction to Forecasting - SES                      #
# ---------------------------------------------------------------------------- #

rm(list=ls(all=TRUE))
rain.data =scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rain.ts <- ts(rain.data, start=c(1813))

par( mfrow=c(1,2) )
hist(rain.data, main="Annual London Rainfall 1813-1912",
xlab="rainfall in inches")
qqnorm(rain.data,main="Normal Plot of London Rainfall")
qqline(rain.data)

par( mfrow=c(2,1) )
plot.ts(rain.ts, main="Annual London Rainfall 1813-1912",
xlab="year", ylab="rainfall in inches")
acf(rain.ts, main="ACF: London Rainfall")

library(forecast)
auto.arima(rain.ts)

# --------------- Implament Simple Exponential Smoothing (SES) --------------- #
alpha=.2 #increase alpha for more rapid decay
forecast.values = NULL #establish array to store forecast values
n = length(rain.data)
#naive first forecast
forecast.values [1] = rain.data[1]
#loop to create all forecast values
for( i in 1:n ) {
forecast.values [i+1] = alpha*rain.data[i] + (1-alpha)* forecast.values [i]
}
paste("forecast for time",n+1," = ", forecast.values [n+1])

# -------------------- Find optimal value of alpha for SES ------------------- #
SSE=NULL
n = length(rain.data)
alpha.values = seq( .001, .999, by=0.001)
number.alphas = length(alpha.values)
for( k in 1:number.alphas ) {
 forecast.values=NULL
 alpha = alpha.values[k]
 forecast.values[1] = rain.data[1]
 for( i in 1:n ) {
 forecast.values[i+1] = alpha*rain.data[i] + (1-alpha)*forecast.values[i]
 }
 SSE[k] = sum( (rain.data - forecast.values[1:n])^2 )
}
plot(SSE~alpha.values, main="Optimal alpha value Minimizes SSE")

index.of.smallest.SSE = which.min(SSE) #returns position 24
alpha = alpha.values[which.min(SSE)] #returns 0.024

forecast.values = NULL #establish array to store forecast values
n = length(rain.data)
#naive first forecast
forecast.values [1] = rain.data[1]
#loop to create all forecast values
for( i in 1:n ) {
forecast.values [i+1] = alpha*rain.data[i] + (1-alpha)* forecast.values [i]
}
paste("forecast for time",n+1," = ", forecast.values [n+1])


# --------------------- HoltWinters() to perform the SES --------------------- #
HoltWinters(rain.ts, beta=FALSE, gamma=FALSE)






# ---------------------------------------------------------------------------- #
#             Introduction to Forecasting – Holt-Winters for Trend             #
# ---------------------------------------------------------------------------- #

# Note: 
# LEVEL_1 = X(1)
# TREND_1 = X(2) - X(1)

rm(list=ls(all=TRUE))

# use your appropriate directory!
#setwd("the directory where you have your data")
setwd("C:/Users/yourname/Desktop") #for example
money.data = read.table("volume-of-money-abs-definition-m.txt")
money.data.ts = ts(money.data[,2],start=c(1960,2) , frequency=12)
par(mfrow=c(3,1))
plot(money.data.ts, main="Time Plot of Volume of Money")
acf(money.data.ts, main="ACF of Volume of Money")
acf(money.data.ts, type="partial", main="PACF of Volume of Money")

# ------------------------- Make predictions manually ------------------------ #

#set up our transformed data and smoothing parameters
data = money.data[,2]
N = length(data)
alpha = 0.7
beta = 0.5

#prepare empty arrays so we can store values
forecast = NULL
level = NULL
trend = NULL

#initialize level and trend in a very simple way
level[1] = data [1]
trend[1] = data [2]- data [1]

#initialize forecast to get started
forecast[1] = data [1]
forecast[2] = data [2]

#loop to build forecasts
for( n in 2:N ) {
level[n] = alpha* data [n] +
(1-alpha)*(level[n-1]+trend[n-1])
trend[n] = beta*(level[n] - level[n-1]) +
(1-beta)*trend[n-1]
forecast[n+1] = level[n] + trend[n]
}

#display your calculated forecast values
forecast[3:N]

# ----------------------- Same output with HoltWinters ----------------------- #
#verify that we have recovered HoltWinters() output
m = HoltWinters(data, alpha = 0.7, beta = 0.5, gamma = FALSE)
m$fitted[,1]

plot(m, main="Holt Winters Fitting of Money Volume with Bogus Parameters")

# ---------- Letting the routine find optimal alpha and beta values ---------- #
m=HoltWinters(data, gamma = FALSE)
plot(m, main="Holt Winters Fitting of Money Volume with Optimal Parameters")


# ------------------------------- Airline Data ------------------------------- #
plot(AirPassengers)
plot(log10(AirPassengers)) #without heteroscedasticity

m = HoltWinters(x = log10(AirPassengers), beta = FALSE, gamma = FALSE)
m

m$SSE





# ---------------------------------------------------------------------------------------------  #
#           Forecasting Using Holt Winters for Trend and Seasonality    ((Triple Exponential))   #
# ---------------------------------------------------------------------------------------------  #

# ---------------- Naive forescasting, no trend or seasonality --------------- #
AirPassengers.SES = HoltWinters( log10(AirPassengers), beta=FALSE, gamma=FALSE )
print(AirPassengers.SES$SSE)

# ---------------------------- Triple Exponential ---------------------------- #

AirPassengers.HW = HoltWinters( log10(AirPassengers), seasonal = c("additive") ) #Additive seasonality is the default
print(AirPassengers.HW$SSE)
print(AirPassengers.HW)


alpha = AirPassengers.HW$alpha
beta = AirPassengers.HW$beta
gamma = AirPassengers.HW$gamma

print("Coefficients")
print(AirPassengers.HW$coefficients)
# a = 2.68 (level_144)
#b = 0.003900787 (trend_144)
#s1...s12: (seasonality)
#s1 -0.031790733 (jan)
#s2 -0.061224237 (feb)
#s3 -0.015941495
#s4 0.006307818
#s5 0.014138008
#s6 0.067260071
#s7 0.127820295
#s8 0.119893006
#s9 0.038321663
#s10 -0.014181699
#s11 -0.085995400
#s12 -0.044672707 (dec)

print("Understanding the results and forecasting into the future. Data set has 144 months")
print("Start date 1949-Jan, end date 1960-Dec")
print(length(AirPassengers))
print(AirPassengers)

# ------------------------- Forecast for January 1961 ------------------------ #

# One month into future: h = 1
# Seasonality of one year: m = 12
# Last measurement: n = 144

# FORMULA: x_(n+h) = level_n + h * trend_n + seasonal_(n+h-m)

# With the data from the model:
# 𝑥_(144+1) = level_144 + 1 * trend_144 + seasonal_(144 + 1 - 12)
# 𝑥_145 = a + b + (seasonal_jan)
# 𝑥_145 = 2.680599 + 0.003900787 + (−0.031790733)
# 𝑥_145 = 2.652709

# ------------------------- Forecast for August 1961 ------------------------- #

# Eight months into future: h = 8
# Seasonality of one year: m = 12
# Last measurement: n = 144

# FORMULA: x_(n+h) = level_n + h * trend_n + seasonal_(n+h-m)

# With the data from the model:
# x_(144+8) = level_144 + 8 * trend_144 + seasonal_(144 + 8 - 12)
# x_152 = 2.680599 + 8 ⋅ 0.003900787 + (0.119893006)

# -------------------------- Forecast for March 1962 ------------------------- #
# Eight months into future: h = 15
# Seasonality of one year: m = 12
# Last measurement: n = 144

# x_(144+15) = 2.680599 + 15 ∗ 0.003900787 + (−0.015941495) = 2.723169



# ---------------------- Forecast with FORECAST PACKAGE ---------------------- #
#rm(list=ls(all=TRUE))
library("forecast")
AirPassengers_HW_model = HoltWinters( log10(AirPassengers), seasonal = c("additive") ) 
AirPassengers_forecast = forecast(AirPassengers_HW_model, h=15) #h=months to predict
print(AirPassengers_forecast)

plot(AirPassengers_forecast, xlim=c(1949, 1963))






# ---------------------------------------------------------------------------- #
#                              TEST - FORECASTING                              #
# ---------------------------------------------------------------------------- #

# -------------------------------- Question 1 -------------------------------- #
# Let's take a look at the data set called sunspots (time series spotted below).
# There isn't much of an apparent trend, but the series certainly seems to exhibit seasonality
# It ends at Dec 1983
# The last observation is for December 1983. What is your forecast for January 1984?

plot(sunspots)

sunspots_hw_model = HoltWinters(sunspots, seasonal=c("additive"))
print(sunspots_hw_model)

# h = 1
# m = 12
# n = n

# x_(n+1) = level_n + h * trand_n + seasonal_(n+h-m)

X_n_plus_1 =  44.44311347 + 1 * (-0.01320934) + (-12.28749771)
print(X_n_plus_1)

# -------------------------------- Question 2 -------------------------------- #
# What is your forecast (using HoltWinters() ) for April 1984?
# h = 4
library(forecast)
forecast( sunspots_hw_model, h =4) #35.85609

# Model Formula:
# x_(n+4) = level_n + 4 * trend_n + seasonal_(n+4-12)
# x_(2820+1) = 44.44311347 + 4 ∗ (−0.01320934) + (−8.53419032)


# -------------------------------- Question 3 -------------------------------- #

# Do you forecast that the monthly mean relative sunspot number for August 1985 will be higher or lower
# than the observed number for August 1983? Note: you can recover the measured value from August 1983 by
#  typing: sunspots on the R command line. You should see the value:  August 1983 (71.8)

forecast( sunspots_hw_model, h =24) 
# Aug 1985       38.65281

print(sunspots) # Aug 1983 71.8
