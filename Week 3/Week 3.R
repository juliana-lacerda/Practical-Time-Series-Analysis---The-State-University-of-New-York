# ---------------------------------------------------------------------------- #
#                           Autoregressive Processes                           #
# ---------------------------------------------------------------------------- #
set.seed(2016)
N=1000
Z = rnorm(N,0,1)

X=NULL
X[1] = Z[1]

# --------------------------------- AR(1) phi = 0.4 -------------------------------- #
# X_t = Z_t + phi * X_(t-1)
phi = 0.4
for (t in 2:N) {
X[t] = Z[t] + phi*X[t-1] ;
}
X.ts = ts(X)
par(mfrow=c(2,1))
plot(X.ts,main="AR(1) Time Series on White Noise, phi=.4")
X.acf = acf(X.ts, main="AR(1) Time Series on White Noise, phi=.4")

# --------------------------------- AR(1) phi = 1.0 -------------------------------- #
# X_t = Z_t + phi * X_(t-1)
phi=1
for (t in 2:N) {
X[t] = Z[t] + phi*X[t-1] ;
}
X.ts = ts(X)
par(mfrow=c(2,1))
plot(X.ts,main="AR(1) Time Series on White Noise, phi=.4")
X.acf = acf(X.ts, main="AR(1) Time Series on White Noise, phi=.4")

# ---------------------- Simulation of AR(2) with arima.sim --------------------- #
# phi1 = 0.7, phi2 = 0.2 => X_t = Z_t + phi_1 * X_(t-1) + phi_2 X_(t-2)
set.seed(2017)
X.ts <- arima.sim(list(ar = c(.7, .2)), n=1000)
par(mfrow=c(2,1))
plot(X.ts,main="AR(2) Time Series, phi1=.7, phi2=.2")
X.acf = acf(X.ts, main="Autocorrelation of AR(2) Time Series")

# ------------ Simulation of AR(2) with arima.sim and negative phi ----------- #
# phi1 = 0.7, phi2 = 0.2 => X_t = Z_t + phi_1 * X_(t-1) + phi_2 X_(t-2)
phi1 = .5
phi2 = -.4

X.ts <- arima.sim(list(ar = c(phi1 , phi2)), n=1000)
par(mfrow=c(2,1))
plot(X.ts,main=paste("AR(2) Time Series, phi1=",phi1, "phi2=", phi2))
X.acf = acf(X.ts, main="Autocorrelation of AR(2) Time Series")

r_coef = X.acf$acf
print('autocorrelations')
print(r_coef)



