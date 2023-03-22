import statsmodels.api as sm
import numpy as np
from statsmodels.tsa.stattools import adfuller
import pandas as pd

def box_test(df,alpha=0.05,lag_number=None):
    '''
    Ljung-Box test for autocorrelation
    H0 -> rho_i = 0 for all i up to the lag tested -> Lags are independent
    H1 -> Lags are not independent

    lag_number: list or int with lag_numbers to test. ex: [4,5] or 4
        if not provided, the lag is set as ln(size df)
    '''

    if lag_number:
        lag = lag_number
    else:
        lag = int(np.log(df.shape[0]))

    test = sm.stats.acorr_ljungbox(df, lags = lag,return_df=True)

    print(f"Signifcance level: {alpha}")
    
    for i in range(1,test.shape[0]+1):
        if test.loc[i,'lb_pvalue'] < alpha:
            print(f"Reject H0. There is autocorrelation at lag {i}. p-value: {test.loc[i,'lb_pvalue']:.4f}")
        else:
            print(f"Accept H0. There is NO autocorrelation at lag {i}. p-value: {test.loc[i,'lb_pvalue']:.4f}")

    return(test)

# ---------------------------------------------------------------------------- #

def adfuller_test(df,alpha=0.05):

    '''
        Performs augmented Dickey-Fuller test
        
        * H0: time series IS NOT stationary
        * H1: time series IS stationary
    '''
    df_test = adfuller(df,autolag='AIC')

    labels = ['ADF test statistic','p-value','# lags used','# observations']
    df_out = pd.Series(df_test[0:4],index=labels)

    for key,val in df_test[4].items():
        df_out[f'critical value ({key})']=val
    
    if df_test[1] <=alpha:
        print("Reject H0. Time series IS stationary.")
    else:
        print("Fail to reject H0. Time series IS NOT stationary.")

    return(df_out)
