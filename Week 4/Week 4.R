######### !!!!!!!!!!
# Definition: A moving average process of order q has an ACF that cuts off after q lags!
# Definition: An autoregressive process of order p, an AR(p), has a PACF that cuts off after p lags.
######### !!!!!!!!!!

# ---------------------------------------------------------------------------- #
#              Partial Autocorrelation and the PACF First Examples             #
# ---------------------------------------------------------------------------- #

rm( list=ls( all = TRUE ) )
par(mfrow=c(3,1))
phi.1 = .6
phi.2 = .2
data.ts = arima.sim(n = 500, list(ar = c(phi.1, phi.2)))
plot(data.ts, main=paste("Autoregressive Process with phi1=",phi.1," phi2=",phi.2) )
acf(data.ts, main="Autocorrelation Function")
acf(data.ts, type="partial", main="Partial Autocorrelation Function")

# ------------------------- New time series with phi3 ------------------------ #
phi.1 = .9
phi.2 = -.6
phi.3 = .3
data.ts = arima.sim(n = 500, list(ar = c(phi.1, phi.2, phi.3)))
plot(data.ts, main= paste("Autoregressive Process with phi1=", phi.1," phi2=",phi.2," phi3=",phi.3) )
acf(data.ts, main="Autocorrelation Function")
acf(data.ts, type="partial", main="Partial Autocorrelation Function")


# ---------------------------------------------------------------------------- #
#                  PACF and the Beveridge Wheat Price Data Set                  #
# ---------------------------------------------------------------------------- #

beveridge = read.table("beveridge.txt", header=TRUE)
beveridge.ts = ts(beveridge[,2], start=1500)
plot( beveridge.ts, ylab="price", main="Beveridge Wheat Price Data")
beveridge.MA = filter(beveridge.ts, rep(1/31, 31), sides = 2)
lines(beveridge.MA, col="red")

# -------- scaling each data point by its corresponding smoothed value ------- #

par(mfrow=c(3,1))
Y = beveridge.ts/beveridge.MA
plot( Y, ylab="scaled price", main="Transformed Beveridge Wheat Price Data")
#The acf() function doesn’t like missing data, so the first and last 15 numbers are ignored or omitted 
acf(na.omit(Y), main="Autocorrelation Function of Transformed Beveridge Data")
acf(na.omit(Y), type="partial", main="Partial Autocorrelation Function of Transformed Beveridge Data")


# ---------------------------------------------------------------------------- #
#                                 FUNCTION AR()                                #
# ---------------------------------------------------------------------------- #

# We let the ar() function find the coefficients for us. We discuss selecting a model using quality
# criteria such as the Akaike Information Criterion (AIC) in other lectures. As R will tell you, the
# ar() routine will “Fit an autoregressive time series model to the data, by default selecting the
# complexity by AIC.” By complexity we mean how many terms to take, or what the value of p is.
# If we will allow up to 5 terms in our model (a reasonable number) we call

ar(na.omit(Y), order.max = 5)


# ---------------------------------------------------------------------------- #
#                     Partial Autocorrelation and the PACF                     #
# ---------------------------------------------------------------------------- #
install.packages("isdals")
library(isdals)
data(bodyfat)

attach(bodyfat)
pairs( cbind( Fat, Triceps, Thigh, Midarm) )

cor( cbind( Fat, Triceps, Thigh, Midarm) )

# Since Triceps and Thigh are also clearly related r= 0.9238425, we wonder if we can measure the
# correlation of Fat and Triceps, after controlling for or “partialling out” Thigh. We first try to
# account for the effect of Thigh on both Fat and Triceps by regressing them on Thigh. After we
# remove the contribution of Thigh, we then find the correlation of Fat and Triceps. This is pretty
# easy to do; just use the lm() command we’ve previously discussed.

Fat.hat = predict(lm(Fat~Thigh))
Triceps.hat = predict( lm(Triceps~Thigh) )
cor( (Fat- Fat.hat), (Triceps- Triceps.hat) )
# Faster way to calculate this:
install.packages("ppcor")
library(ppcor)
pcor(cbind(Fat,Triceps,Thigh))

# So, a great deal of the correlation between Fat and Triceps is accounted for by controlling for
# Thigh circumference. What happens when we control for both Thigh and Midarm?

Fat.hat = predict(lm(Fat~Thigh+Midarm))
Triceps.hat = predict( lm(Triceps~Thigh+Midarm) )
cor( (Fat- Fat.hat), (Triceps- Triceps.hat) ) #returns 0.33815

pcor( cbind( Fat, Triceps, Thigh, Midarm) )

# --------------- PLOAT PACF: acf( time_series, type="partial") -------------- #
phi.1 = .6;
phi.2 = -.6;
data.ts = arima.sim(n = 1000, list(ar = c(phi.1, phi.2)))
acf(data.ts, type="partial",
main=paste("PACF of Time Series Data, phi1=",phi.1,", phi.2=",phi.2) )
print("We would conclude for the time series exhibited here that we have a second order model")




# ---------------------------------------------------------------------------- #
#                         YULE-WALKER ESTIMATION AR(2)                         #
# ---------------------------------------------------------------------------- #

# Simulation of AR(2) process
# xt=phi1*x(t-1)+phi2*x_(t-2)+z_t z_t~ N(0, sigma^2)

#set seed a common number, so we can reproduce the same datasets
set.seed(2017)

#model parameters (we will estimate them)
sigma=4
phi=NULL
phi[1:2]=c(1/3,1/2)
phi

#number of data points
n=10000

#simulate ar process
ar.process=arima.sim(n,model=list(ar=c(1/3,1/2)), sd=4)
ar.process[1:5]

#find and name 2nd and 3rd sample autocorrelation
r=NULL
r[1:2]=acf(ar.process, plot=F)$acf[2:3]
r

#matrix R
R=matrix(1,2,2) # matrix of dimension 2 by 2, with entries all 1's.
R

#edit R
R[1,2]=r[1] # only diagonal entries are edited
R[2,1]=r[1] # only diagonal entries are edited
R

#b-column vector on the right
b=matrix(r,nrow=2,ncol=1)# b- column vector with no entries
b

#solve(R,b) solves Rx=b, and gives x=R^(-1)b vector
#hard way: phi.hat=matrix(c(solve(R,b)[1,1], solve(R,b)[2,1]),2,1)
# easy way:
phi.hat=solve(R,b)
phi.hat

#variance estimation
c0=acf(ar.process, type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))
var.hat

#plot time series, along with acf, pacf
par(mfrow=c(3,1))
plot(ar.process, main='Simulated AR(2)')
acf(ar.process, main='ACF')
pacf(ar.process, main='PACF')



# ---------------------------------------------------------------------------- #
#            Estimation of model patrameters of an AR(3) simulation            #
# ---------------------------------------------------------------------------- #
# x_t=phi1*x_(t-1)+phi2* x_(t-2)+\phi_3*x_(t-3)+z_t
# z_t~ N(0, sigma^2)

set.seed(2017)
sigma=4
phi=NULL
phi[1:3]=c(1/3,1/2,7/100)
n=100000

#Simulate AR(3) process
ar3.process=arima.sim(n,model=list(ar=c(1/3,1/2, 7/100)), sd=4)
r=NULL
r[1:3]=acf(ar3.process, plot=F)$acf[2:4]
r

R=matrix(1,3,3) 
R[1,2]=r[1] 
R[1,3]=r[2]
R[2,1]=r[1]
R[2,3]=r[1]
R[3,1]=r[2]
R[3,2]=r[1]
R

# b-column vector on the right
b=matrix(,3,1)# b- column vector with no entries
b[1,1]=r[1]
b[2,1]=r[2]
b[3,1]=r[3]
b

# solve Rx=b and find phi's
phi.hat=solve(R,b)
phi.hat

# sigma estimation
c0=acf(ar3.process, type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))
var.hat

#Plots
par(mfrow=c(3,1))
plot(ar3.process, main='Simulated AR(3)')
acf(ar3.process, main='ACF')
pacf(ar3.process, main='PACF')



# ---------------------------------------------------------------------------- #
#                                     TEST                                     #
# ---------------------------------------------------------------------------- #


# ------------------------------------ Q2 ------------------------------------ #
# Sample autocorrelation coefficients of an AR(3) process are given: r1 = 0.8, r2 = 0.6, r3=0.2
# Use Yule-Walker equations in matrix form to estimate model parameters phi_hat1, phi_hat2, phi_hat_3

# autocorrelations 
r=NULL
r[1] = 0.8
r[2] = 0.6
r[3] = 0.2

# build b
b=matrix(,3,1)# b- column vector with no entries
b[1,1]=r[1]
b[2,1]=r[2]
b[3,1]=r[3]
b

# Build R


R=matrix(1,3,3) 
R[1,2]=r[1] 
R[1,3]=r[2]
R[2,1]=r[1]
R[2,3]=r[1]
R[3,1]=r[2]
R[3,2]=r[1]
R

# find phi_hat
phi.hat=solve(R,b)
phi.hat

# ------------------------------------ Q3 ------------------------------------ #
# Use question 2 information and the fact that sample autocivariance at lag 0 (c(0)=5)
# to estimate the variance of the noise in the same AR(3) process, i.e., sigma^2_hat

c0 = 5
var.hat=c0*(1-sum(phi.hat*r))
var.hat





# ---------------------------------------------------------------------------- #
#                       Recruitment Data - Model Fitting                       #
# ---------------------------------------------------------------------------- #

# Modeling recruitment time series from 'astsa' package as an AR process
install.packages("astsa")
library(astsa)
my.data=rec

# Plot rec 
plot(rec, main='Recruitment time series', col='blue', lwd=3)
# subtract mean to get a time series with mean zero
ar.process=my.data-mean(my.data)

# ACF and PACF of the process
par(mfrow=c(2,1))
acf(ar.process, main='Recruitment', col='red', lwd=3)
pacf(ar.process, main='Recruitment', col='green', lwd=3)

# order
p=2

# sample autocorreleation function r
r=NULL
r[1:p]=acf(ar.process, plot=F)$acf[2:(p+1)]
cat('r=',r,'\n')

# matrix R
R=matrix(1,p,p) # matrix of dimension p by p, with entries all 1's.

# define non-diagonal entires of R
for(i in 1:p){
    for(j in 1:p){
        if(i!=j)
            R[i,j]=r[abs(i-j)]
        }
    }
R

# b-column vector on the right
b=NULL
b=matrix(r,p,1)# b- column vector with no entries
b

# solve(R,b) solves Rx=b, and gives x=R^(-1)b vector
phi.hat=NULL
phi.hat=solve(R,b)[,1]
phi.hat

#variance estimation using Yule-Walker Estimator
c0=acf(ar.process, type='covariance', plot=F)$acf[1]
c0

var.hat=c0*(1-sum(phi.hat*r))
var.hat

# constant term in the model
phi0.hat=mean(my.data)*(1-sum(phi.hat))
phi0.hat
cat("Constant:", phi0.hat," Coeffcinets:", phi.hat, " and Variance:", var.hat, '\n')

# final fitted model
print("Fitted Model: X_t = 7 + 1.3 x_(t-1) - 0.4 X_(t-2) + Z_t")
print("Where Z_t ~ Normal(0, 94.1)")




# ---------------------------------------------------------------------------- #
#                         JohnsonJohnson Model Fitting                         #
# ---------------------------------------------------------------------------- #

#Johnson & Johnson quarterly earnings per share
install.packages("astsa")
library(astsa)

# Time plot for Johnson&Johnson
plot(JohnsonJohnson, main='Johnson&Johnosn earnings per share', col='blue', lwd=3)

# log-return of Johnson&Johnson
jj.log.return=diff(log(JohnsonJohnson))
jj.log.return.mean.zero=jj.log.return-mean(jj.log.return)

# Plots for log-returns
par(mfrow=c(3,1))
plot(jj.log.return.mean.zero, main='Log-return (mean zero) of Johnson&Johnosn earnings per share')
acf(jj.log.return.mean.zero, main='ACF')
pacf(jj.log.return.mean.zero, main='PACF')

# Order
p=4

# sample autocorreleation function r
r=NULL
r[1:p]=acf(jj.log.return.mean.zero, plot=F)$acf[2:(p+1)]
r

# matrix R
R=matrix(1,p,p) # matrix of dimension 4 by 4, with entries all 1's.

# define non-diagonal entires of R
for(i in 1:p){
    for(j in 1:p){
        if(i!=j)
            R[i,j]=r[abs(i-j)]
        }
    }
R

# b-column vector on the right
b=matrix(r,p,1)# b- column vector with no entries
b

phi.hat=solve(R,b)[,1]
phi.hat

# Variance estimation using Yule-Walker Estimator
c0=acf(jj.log.return.mean.zero, type='covariance', plot=F)$acf[1]
c0

var.hat=c0*(1-sum(phi.hat*r))
var.hat

# Constant term in the model
phi0.hat=mean(jj.log.return)*(1-sum(phi.hat))
phi0.hat
cat("Constant:", phi0.hat," Coeffcinets:", phi.hat, " and Variance:", var.hat, '\n')

# final fitted model
print("Fitted Model: r_t = 0.07 - 0.6 r_(t-1) - 0.5 r_(t-2) - 0.4 r_(t-3) + 0.2 r_(t-4) + Z_t")
print("Where r_t = log(X_t/X_(t-1)). r_t: log return") 
print("Where Z_t ~ Normal(0, 0.01)")


# ---------------------------------------------------------------------------- #
#                                     TEST                                     #
# ---------------------------------------------------------------------------- #
y = LakeHuron

ar_process=diff(y)
ar_process_mean_zero=ar.process - mean(ar.process)

plot(y)
print("There is a downward trend in the time series.")

print("How one can remove the trend (i.e. de-trend) the time series 'LakeHuron' in R?")
plot(ar_process) 

print("Find the PACF of the differenced time series in a code block below.")
pacf(ar_process_mean_zero, main='PACF')

# --------------- Find the first 3 autocorrelation coefficients of the differenced time series -------------- #

# order
p=3

# sample autocorreleation function r
r=NULL
r[1:p]=acf(ar_process, plot=F)$acf[2:(p+1)]
cat('r=',r,'\n')



# ------------------ Estimate the coefficients of the AR(2) ------------------ #
# order
p=2

# sample autocorreleation function r
r=NULL
r[1:p]=acf(ar_process, plot=F)$acf[2:(p+1)]
cat('r=',r,'\n')

# matrix R
R=matrix(1,p,p) # matrix of dimension p by p, with entries all 1's.

# define non-diagonal entires of R
for(i in 1:p){
    for(j in 1:p){
        if(i!=j)
            R[i,j]=r[abs(i-j)]
        }
    }
R

# b-column vector on the right
b=NULL
b=matrix(r,p,1)# b- column vector with no entries
b

# solve(R,b) solves Rx=b, and gives x=R^(-1)b vector
phi.hat=NULL
phi.hat=solve(R,b)[,1]
phi.hat

# -------------------- Estimate the variance of the noise -------------------- #

# Variance estimation using Yule-Walker Estimator
c0=acf(ar_process, type='covariance', plot=F)$acf[1]
c0

var.hat=c0*(1-sum(phi.hat*r))
var.hat

# Constant term in the model
phi0.hat=mean(ar_process)*(1-sum(phi.hat))
phi0.hat
