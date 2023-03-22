from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
import matplotlib.pyplot as plt
from statsmodels.tsa.seasonal import seasonal_decompose
import scipy.stats as stats
import statsmodels.api as sm
import seaborn as sns
import numpy as np
sns.set_context("talk")


def plot_seasonal_decomposition(df_in,feature,model='additive',period=None,save_fig=False,
                                 fig_path='', fig_name='Seasonal_Decomposition'):
    '''
        Plota a saida da funcao seasonal decomposition: Serie temporal observada, a tendencia, a sazonalidade e os residuos, todos em funcao do tempo.
    '''
    df = df_in.copy()
    
    #df.index = df.index.to_timestamp()
    
    result = seasonal_decompose(df[feature],model=model, period=period)

    fig = result.plot()
    fig.set_size_inches((12,8))
    fig.tight_layout()
    plt.show()
    
    if save_fig:
        fig.savefig(fig_path + fig_name + '.png')  
    
# ---------------------------------------------------------------------------- #

def plot_acf_pacf(df,feature,lags=None,save_fig=False,fig_path='',fig_name_acf='Autocorrelation',
                  fig_name_pacf='Partial_Autocorrelation',title_acf='Autocorrelation',
                  title_pacf='Partial Autocorrelation'):
    '''
        Plots Autocorrelation and Partial Autocorrelation
    '''
    fig = plot_acf(df[feature],title=title_acf,lags=lags)
    fig.set_size_inches((10,5))
    plt.xlabel("Lag")
    fig.tight_layout()
    plt.show()  
    if save_fig:
        fig.savefig(fig_path + fig_name_acf + '.png')
    
    fig = plot_pacf(df[feature],lags=lags,title=title_pacf,method='ywm')
    fig.set_size_inches((10,5))
    plt.xlabel("Lag")
    fig.tight_layout()
    plt.show() 
    if save_fig:
        fig.savefig(fig_path + fig_name_pacf + '.png') 

# ---------------------------------------------------------------------------- #
    
def plot_time_series(df, feature, title, ylabel, xlabel, with_marker=False, save_fig=False, fig_path ='',
                      fig_name ='Time_Series', xlim=None):
    '''
        Plota uma serie temporal em funcao do tempo.
    '''
    fig = plt.figure()

    if with_marker:
        ax = df[feature].plot(figsize=(12,6),title=title,style='bo-')
    else:
        ax = df[feature].plot(figsize=(12,6),title=title)

    ax.autoscale(axis='x', tight=True)
    #ax.set_ylim(0,)
    if xlim:
        ax.set_xlim(xlim[0],xlim[1])
    ax.set(xlabel=xlabel,ylabel=ylabel);
    
    fig.tight_layout()
    plt.show()

    if save_fig:
        fig.savefig(fig_path + fig_name + '.png') 

# ---------------------------------------------------------------------------- #
    
def plot_time_series_and_prediction(df, df_pred, title, ylabel, xlabel, df_pred2=None, df_pred3=None,
                                     with_marker=False, save_fig=False, fig_name= 'Time_Series_and_Prediction',
                                       fig_path ='', xlim=None):
    '''
        Plot the original time series and its prediction as a function of time.
        It can also receive an optinal second and third predictions (from different models) to plot there as well.
    '''
    fig = plt.figure()
    
    if with_marker:
        ax = df.plot(figsize=(12,6),title=title,style='bo-',ms='2',legend=True,label='Original Data')
        
    else:
        ax = df.plot(figsize=(12,6),title=title,color='blue',legend=True,label='Original Data')

    df_pred.plot(legend=True,color='red',alpha=0.5)

    if not isinstance(df_pred2,type(None)):
        df_pred2.plot(legend=True,color='green',alpha=0.5)
    if not isinstance(df_pred3,type(None)):
        df_pred3.plot(legend=True,color='black',alpha=0.5)

    ax.autoscale(axis='x', tight=True)
    #ax.set_ylim(0,)
    if xlim:
        ax.set_xlim(xlim[0],xlim[1])
    ax.set(xlabel=xlabel,ylabel=ylabel);
    
    fig.tight_layout()
    plt.show()

    if save_fig:
        fig.savefig(fig_path + fig_name + '.png')    

# ---------------------------------------------------------------------------- #

def qq_plot(residuals,title,save_fig=False,fig_path='',fig_name='QQ_Plot'):

    fig = sm.qqplot(residuals, stats.t, fit=True, line="45")
    fig.set_figheight(6)
    fig.set_figwidth(10)
    plt.title(title)
    fig.tight_layout()
    
    plt.show()

    if save_fig:
        fig.savefig(fig_path + fig_name + '.png') 

# ---------------------------------------------------------------------------- #

def plot_box_test(df,lag_number=None,alpha=0.05,save_fig=False,fig_path='',fig_name='box_test'):
    '''
    Ljung-Box test for autocorrelation
    H0 -> rho_i = 0 for all i up to the lag tested -> Lags are independent
    H1 -> Lags are not independent
    '''

    if lag_number:
        lag = lag_number
    else:
        lag = int(np.log(df.shape[0]))


    fig = plt.figure(figsize=(10,6))

    test = sm.stats.acorr_ljungbox(df, return_df=True, lags=lag_number)

    plt.plot(test['lb_pvalue'],'o')
    plt.axhline(y=0.05, color='black', linestyle='--')
    plt.ylim(-0.1,1.1)
    plt.xlabel('Lag')
    plt.ylabel('p-value')
    plt.title('p-values for Ljung-Box statistic')
    plt.show()

    if save_fig:
        fig.savefig(fig_path + fig_name + '.png') 

          