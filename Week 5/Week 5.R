# ---------------------------------------------------------------------------- #
#                Akaike Information Criterion and Model Quality                #
# ---------------------------------------------------------------------------- #

# ---------------------------- Model the process: ---------------------------- #
# X_t = Z_t + 0.7 X_(t-1) - 0.2 X_(t-2)

# list(order = c(2,0,0), ar =c(0.7, -0.2))
# order = (autoregressive order, differencing order, moving average part)
# ar = (phi1,phi2)

rm(list=ls(all=TRUE))

phi_1 =  0.7
phi_2 = -0.2


set.seed(43) 
data = arima.sim( list(order = c(2,0,0), ar =c(phi_1,phi_2)), n = 2000)

par(mfrow=c(1,2))
acf(data, main="ACF of AR Data of Second Order")
acf(data, type="partial", main="PACF of Time Series")

# ----------------- Determine the coefficients of the process ---------------- #
# must be the same as the original ones

arima(data, order=c(2,0,0), include.mean =FALSE)
# These estimates compare quite favorably to our established values phi_1 and phi_2


# -------------------------- For several values of p ------------------------- #
SSE=NULL
AIC=NULL
for (p in 1:5) {
    m = arima(data, order=c(p,0,0), include.mean=FALSE )
    SSE[p] = sum(resid(m)^2)
    AIC[p] = m$aic
    print( m$coef )
    print( paste(m$aic, sum(resid(m)^2)) )
    }


# A naïve suggestion would be to try an order, e.g. p=3, estimate the
# coefficients for this order, and then determine the residual sum of squares for our candidate model.
# We could then pick whichever model gives us the lowest aggregate residual sum of squares. We
# have our 5 models for p=1 through p=5. We can extract the residuals from each, and then look at
# the sum of the squares of the errors (SSE) for each. When I plot the residual squared terms against
# the order I obtain the graph on the left.

par(mfrow=c(1,2))
order=c(1,2,3,4,5)
plot(SSE~order, main="SSE plotted on Order of AR(p) Model", ylim=c(1800, 2100))
plot(AIC~order, main="AIC plotted on Order of AR(p) Model", ylim=c(5500, 5800))



# ---------------------------------------------------------------------------- #
#                                  ARMA MODELS                                 #
# ---------------------------------------------------------------------------- #


# ------------------------------ ARMA (p=1,q=1) ------------------------------ #

# Model:
# X_t = 0.7 X_(t-1) + Z_t + 0.2 Z_(t-1)

rm(list=ls(all=TRUE)) # clean variables 

p = 1
q = 1
phi_1 = 0.7
theta_1 = 0.2

set.seed(500) 
data = arima.sim( list(order = c(p,0,q), ar =(phi_1), ma=(theta_1)), n = 1000000)
par(mfcol = c(3,1 ))
plot(data, main="ARMA(1,1) Time Series: phi1=.7, theta1=.2", xlim=c(0,400)) #first terms
acf(data, main="Autocorrelation of ARMA(1,1), phi1=.7, theta1=.2")
acf(data, type="partial", main="Partial Autocorrelation of ARMA(1,1), phi1=.7, theta1=.2")


# ---------------------------------------------------------------------------- #
#                         ARMA PROPERTIES AND EXAMPLES                         #
# ---------------------------------------------------------------------------- #


# ---------------------------- Example: ARMA Model --------------------------- #

plot(discoveries,
main = "Time Series of Number of Major Scientific Discoveries in a Year")

stripchart(discoveries, method = "stack", offset=.5, at=.15,pch=19,
main="Number of Discoveries Dotplot",
xlab="Number of Major Scientific Discoveries in a Year",
ylab="Frequency")

# the time plot has no obvious trends or seasonality, so we can try fitting an ARMA model.

par(mfcol = c(2,1 ))
acf(discoveries, main="ACF of Number of Major Scientific Discoveries in a Year")
acf(discoveries, type="partial", main="PACF of Number of Major Scientific Discoveries
in a Year")

# Try some p and q values at the arima model and check the value of AIC
p=0
q=1
AIC( arima( discoveries, order=c(p,0,q))) 

p=1
q=1
AIC( arima( discoveries, order=c(p,0,q))) 

p=3
q=2
AIC( arima( discoveries, order=c(p,0,q))) 

# Choosing the simplest model
arima( discoveries, order=c(1,0,1) ) 


# ---------------- Automatic Routines: auto.arima()---------------- #
install.packages('forecast', dependencies = TRUE)
install.packages("rlang")

library(forecast)

approx = (length(discoveries)>100 | frequency(discoveries)>12)

# ic: quality criteria
# Bayesian Information Criterion (bic),
# Akaike Information Criterion (aic), or
# “corrected AIC” (aicc), DEFAULT

auto.arima(discoveries, d=0, ic=c('aicc'), approximation=approx) # d refers to the I in ARIMA.



# ---------------------------------------------------------------------------- #
#                           ARIMA (2,1,1) SIMULATION                           #
# ---------------------------------------------------------------------------- #



# parameters
phi=c(.7, .2)
beta=0.5
sigma=3
n_points=10000
p = 2
d = 1
q = 1
set.seed(5)

Simulated.Arima=arima.sim(n=n_points,list(order = c(p,d,q), ar = phi, ma= beta))

plot(Simulated.Arima, ylab=' ',main='Simulated time series from ARIMA(2,1,1) process', col='blue', lwd=2)

acf(Simulated.Arima)

Diff.Simulated.Arima=diff(Simulated.Arima)

plot(Diff.Simulated.Arima)

acf(Diff.Simulated.Arima)

pacf(Diff.Simulated.Arima)

library(astsa)
# sarima(data, p, d, q, P = 0, D = 0, Q = 0, S = -1)
P = 0
D = 0
Q = 0
sarima(Simulated.Arima,p,d,q,P,D,Q) 

library(forecast)
auto.arima(Simulated.Arima) 

fit1<-arima(Diff.Simulated.Arima, order=c(4,0,0))
fit1

fit2<-arima(Diff.Simulated.Arima, order=c(2,0,1))
fit2

fit3<-arima(Simulated.Arima, order=c(2,1,1))
fit3


# ---------------------------------------------------------------------------- #
#                                  DAILY BIRTH                                 #
# ---------------------------------------------------------------------------- #

# The following time series is taken from Time Series Data Library (TSDL)
# TSDL was created by Rob Hyndman
# Professor of Statistics at Monash University, Australia.
# ==============================================================
# ====== Daily total female birth in California, 1959 =======
# Data is exported as csv file to the wroking directory
# Link: https://datamarket.com/data/list/?q=cat:fwy%20provider:tsdl

library(astsa)

setwd("D:/Dropbox/Data Science/Coursera/Practical Time Series Analysis R - The State University of New York/Semana 5")

# read data to R variable
birth.data<-read.csv("daily-total-female-births-in-cal.csv")

# pull out number of births column
number_of_births<-birth.data$Daily.total.female.births.in.California..1959

# use date format for dates
birth.data$Date <- as.Date(birth.data$Date, "%m/%d/%Y")


plot.ts(number_of_births,main='Daily total female births in california, 1959', ylab = 'Number of births')

# Test for correlation
# H0 -> rho_i = 0 for all i up to the lag tested.
Box.test(number_of_births, lag = log(length(number_of_births)))

# Plot the differenced data
plot.ts(diff(number_of_births), main='Differenced series', ylab = '')

# Test for correlation in the differenced data

Box.test(diff(number_of_births), lag = log(length(diff(number_of_births))))
# acf and pacf of the differenced data

acf(diff(number_of_births), main='ACF of differenced data', 50)
pacf(diff(number_of_births), main='PACF of differenced data', 50)
# Fit various ARIMA models


model1<-arima(number_of_births, order=c(0,1,1))
SSE1<-sum(model1$residuals^2)
# make the box test of the residuals to see if they are correlated. we expect no correlation
# so a p_value>0.05, meaning that we can not reject the null hypothesis
# H0 -> rho_i = 0 for all i up to the lag tested.
model1.test<-Box.test(model1$residuals, lag = log(length(model1$residuals)))

model2<-arima(number_of_births, order=c(0,1,2))
SSE2<-sum(model2$residuals^2)
model2.test<-Box.test(model2$residuals, lag = log(length(model2$residuals)))

model3<-arima(number_of_births, order=c(7,1,1))
SSE3<-sum(model3$residuals^2)
model3.test<-Box.test(model3$residuals, lag = log(length(model3$residuals)))

model4<-arima(number_of_births, order=c(7,1,2))
SSE4<-sum(model4$residuals^2)
model4.test<-Box.test(model4$residuals, lag = log(length(model4$residuals)))

df<-data.frame(row.names=c('AIC', 'SSE', 'p-value'), c(model1$aic, SSE1, model1.test$p.value), 
               c(model2$aic, SSE2, model2.test$p.value), c(model3$aic, SSE3, model3.test$p.value),
               c(model4$aic, SSE4, model4.test$p.value))
colnames(df)<-c('Arima(0,1,1)','Arima(0,1,2)', 'Arima(7,1,1)', 'Arima(7,1,2)')



format(df, scientific=FALSE)

# Fit a SARIMA model
sarima(number_of_births, 0,1,2,0,0,0)
# output:
# ma1 = -0.8511
# ma2 = -0.1113
# constant = 0.015

# Final model: (1-B) X_t = 0.015 + Z_t - 0.08511 Z_(t-1) - 0.1113 Z_(t-2)
# Isolating X_t: X_t = X_(t-1) +  0.015 + Z_t - 0.08511 Z_(t-1) - 0.1113 Z_(t-2)






# ---------------------------------------------------------------------------- #
#                            TEST - BJsales dataset                            #
# ---------------------------------------------------------------------------- #

# This Quiz has several questions all of which are related and are steps toward modeling the time series titled 'BJsales' in 'datasets' package in R.

# --------------- Plot the time series in the code block below. -------------- #
plot(BJsales)

# --------- Plot the differenced data below. Does it seem stationary? -------- #
plot(diff(BJsales))
print("It does not seem to be stationary since there are still upward or downward trends in different parts of the time plot")



# --- To get rid of a still remaining trend, we apply one more differencing -- #
plot(diff(diff(BJsales)))


# ----- Find the PACF of diff(diff(BJsales)).  Which lags are significant? ---- #
acf(diff(diff(BJsales)), type='partial')
print("Lag 1, Lag 2, Lag 3, Lag 10, Lag 19")
print("About the model: Keeping parsimony principle in mind, the order of AR terms can be 0, 1, 2 or 3.")

# ----- Find the ACF of diff(diff(BJsales)).  Which lags are significant? ---- #
acf(diff(diff(BJsales)), type='correlation')
print("Lag 1, Lag 8, Lag 11")
print("About the model: If we ignore barely significant lags, the order of MA term can be 0 or 1. ")


# ------- Now we try few different models and compare their AIC values. ------ #
d=2
for(p in 1:4){
  for(q in 1:2){
        if(p+d+q<=6){
          model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
}


print("We chose the one with lowest AIC. ARIMA(0,2,1)")

# ---------------------------- We fit ARIMA(0,2,1) --------------------------- #
# look at the time plot, ACF and PACF of the residuals.

model<-arima(BJsales, order=c(0,2,1))

par(mfrow=c(2,2))

plot(model$residuals)
acf(model$residuals)
pacf(model$residuals)
qqnorm(model$residuals)

print("Is there compelling evidence against the whiteness of the residuals? (white noise)")
print("No, since ACF nad PACF has no significant lags and QQ-plot seems linear.")

# ---------------- Let X_t=BJsales and Y_t = diff(diff(X_t)). ---------------- #

print("==> What is the fitted model ARIMA(0,2,1) for Y_t? ")
print(model)
# ma1 = -0.74
# var Z = 1.866
print("Model: Y_t = Z_t -0.748 Z_(t-1)")


print("What is the fitted model for X_t?")
print("(1-B)^2 X_t = Z_t -0.748 Z_(t-1)")

