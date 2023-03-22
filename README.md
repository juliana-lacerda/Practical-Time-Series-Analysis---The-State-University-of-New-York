# Practical Time Series Analysis - Coursera - The State University of New York
Course notes in R and my python script related to the course Practical Time Series Analysis - Coursera - The State University of New York

## Contents: My Python Script
### plot_functions.py and statistical_functions.py: Functions to make plots and statistical tests.
### main.ipynb: Notebook with milk production time series and my personal analysis. 
  * SARIMA model
    - The time series is plotted and shows traces of seasonality (s=12 months) and an upward trend. No heteroscedasticity is noted so log-return was not used.
    - The series is turned into a stationary series by performing seasonal and non-seasonal differencing of order 1. The Augmented Dickey-Fuller test is used to test for stationarity.
    - The ACF and Partial ACF are then plotted and analysed. The ACF indicates order of MA term q=0 and order of seasonal MA term Q <= 1. The PACF indicates order of AR term p=0 and order of seasonal AR term P <= 1. Seasonality was previously defined as s = 12 and d = 1 and D = 1 as we used it to make the time series stationary.
    - So the model must be **SARIMA(order=(0,1,0), seasonal_order=(0 or 1, 1, 0 or 1, 12))**
    - The auto_arima function chose the model ARIMA(0,1,0)(0,1,1)[12] as the best fit, as expected.
    - A residual analysis is then performed by the use of Ljung-Box Test, Q-Q plot and ACF. Residuals presented autocorrelation so this model is not a good fit for this data set.
    - The predictions for the model are plotted as a function of time.
 * Holt-Winters method
    - A triple exponential smoothing model with an additive trend and seasonality and seasonal periods of 12.
    - The same residual analysis is performed and this time, residuals present no autocorrelation and are normally distributed, with the exception of a few outliers.
    - The predictions and then plotted as a function of time, along with the SARIMA model.
    - Evaluation metrics are then used to check the performance of both models. The metrics root mean squared error and mean absolute error are calculated. As expected, the HW model presents lower values of these metrics and is a better fit for this dataset.

## Contents: Week 1, ..., Week 6
  * Slides and R scipts of all the classes of the course.


