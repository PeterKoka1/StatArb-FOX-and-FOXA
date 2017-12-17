import pandas as pd
import pickle
import bs4 as bs
import os
from os.path import exists
import requests
import sys

from googlefinance.client import get_prices_time_data
import quandl
from alpha_vantage.timeseries import TimeSeries
from fredapi import Fred

def update_ticks():
    r = requests.get('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')
    soup = bs.BeautifulSoup(r.text, 'lxml')
    SP500 = soup.find('table', {'class':'wikitable sortable'})

    tickers = []
    for ticker in SP500.find_all('tr')[1:]:
        stock = ticker.find_all('td')[0].text
        tickers.append(stock)

    with open('SP500quotes.pickle', 'wb') as f:
        pickle.dump(tickers, f)

    return tickers

def alpha_vantage_pull(request_again, api_key):
    if request_again == True:
        tickers = update_ticks()

    else:
        with open('SP500quotes.pickle', 'rb') as f:
            tickers = pickle.load(f)

    if not os.path.exists('stock_dfs'):
        os.makedirs('stock_dfs')
    else:
        print('Directory Exists')

    for stock in tickers:
        if not exists('SP500/{}.csv'.format(stock)):
            try:
                ts = TimeSeries(key=api_key,
                                output_format='pandas')
                ticker, meta_data = ts.get_daily(symbol=stock,
                                                 outputsize="full")
                ticker = ticker.iloc[2500:]
                drop_labs = [
                    'open',
                    'high',
                    'low',
                    'volume',
                ]
                ticker.drop(drop_labs, axis=1, inplace=True)
                ticker.rename(columns={'close':stock}, inplace=True)
                if ticker.empty:
                    print("Empty dataframe for {} - will not add to stocks_dfs dir".format(stock))
                else:
                    ticker.to_csv('stock_dfs/{}.csv'.format(stock))
                    print("Adding {} to stock_dfs directory".format(stock))
            except:
                s = "Error: {}".format(sys.exc_info()[0])
                s += "\nwhile fetching data for {}".format(stock)

def single_df():

    missing_ticks = []
    with open('SP500quotes.pickle', 'rb') as f:
        tickers = pickle.load(f)

    main_df = pd.DataFrame()

    for stock in tickers:
        try:
            df = pd.read_csv('stock_dfs/{}.csv'.format(stock))
            if main_df.empty:
                main_df = df
            else:
                if '2009-12-10' in str(df.head(1)['Date']) and '2017-12-13' in str(df.tail(1)['Date']):
                    main_df = main_df.merge(df)
                else:
                    missing_ticks.append(stock)
        except FileNotFoundError:
            print("No csv for {}".format(stock))
            missing_ticks.append(stock)
    print(main_df.shape)
    try:
        main_df.to_csv('Joined_Closes.csv')
        print("CSV with joined closes saved as 'Joined_Closes.csv'.")
    except:
        print("Unable to save CSV of joined closes.")

###: rest can be ignored
def quandl_pull(api_key):
    quandl.ApiConfig.api_key = api_key

    if not os.path.exists('stock_dfs'):
        os.makedirs('stock_dfs')
    else:
        print('Directory Exists')
    ticks = [
        'FOX',
        'FOXA'
             ]

    for stock in ticks:
        if not exists('SP500/{}.csv'.format(stock)):
            try:
                data = quandl.get('EOD/{}'.format(stock),
                                  paginate=True,
                                  start_date="2010-01-01",
                                  end_date="2017-12-13")
                drop_cols = [name for name in data.columns if name != 'Close']
                data.drop(labels=drop_cols, inplace=True, axis=1)
                data.rename(columns={'Close':stock}, inplace=True)
                data.to_csv('stock_dfs/{}.csv'.format(stock))
                print("Adding {} to stock_dfs directory".format(stock))
            except:
                print("Unable to read URL for: {}".format(stock))

###: googlefinance.client having issues with pulling historicals
def goog_pull(request_again):

    if request_again == True:
        tickers = update_ticks()

    else:
        with open('SP500quotes.pickle', 'rb') as f:
            tickers = pickle.load(f)

    if not os.path.exists('stock_dfs'):
        os.makedirs('stock_dfs')
    else:
        print('Directory Exists')

    period = "5Y"
    interval = "86400"

    for stock in tickers:
        if not exists('SP500/{}.csv'.format(stock)):
            try:
                param = [
                    {
                        'q': stock,
                        'x': "INDEXSP",
                    },
                ]
                ticker = get_prices_time_data(param, period=period, interval=interval)
                drop_labs = [
                    '{}_Open'.format(stock),
                    '{}_High'.format(stock),
                    '{}_Low'.format(stock),
                    '{}_Volume'.format(stock),
                ]
                ticker.drop(drop_labs, axis=1, inplace=True)
                ticker.rename(columns={'{}_Close'.format(stock):stock}, inplace=True)
                ticker.to_csv('stock_dfs/{}.csv'.format(stock))
                print("Adding {} to stock_dfs directory".format(stock))
            except:
                print("Unable to read URL for: {}".format(stock))

def currency_data(api_key):
    fred = Fred(api_key=api_key)

    USEUROforex = fred.get_series('DEXUSEU')
    USEUROforex = pd.DataFrame(USEUROforex)
    USEUROforex.columns = ['USvsEURO']

    USEUROforex.fillna(
        method="bbfill",
        inplace=True,
    )

    JC = pd.read_csv('Joined_Closes.csv').iloc[:1478]

    exchange_indices = [str(ix)[0:10] for ix in USEUROforex.index.values]
    JC_indices = [ix[0:10] for ix in JC['Periods']]
    labs = [i for i in exchange_indices if i not in JC_indices]
    USEUROforex.reset_index(inplace=True)
    USEUROforex_dict = {'USvsEURO':[]}

    for i, ix in enumerate(USEUROforex['index']):
        if str(ix)[0:10] in labs:
            pass
        else:
            USEUROforex_dict['USvsEURO'].append(USEUROforex['USvsEURO'][i])

    USEUROforex = pd.DataFrame.from_dict(USEUROforex_dict)

    JC = JC.join(USEUROforex)
    JC.drop('Unnamed: 0', axis=1, inplace=True)

    JC.to_csv('Indicators_Joined_.csv')

def sectors():

    params = [
        {
            'q': 'XLF',
            'x': 'NYSEARCA',
        },
        {
            'q': '.INX',
            'x': 'INDEXSP'
        }
    ]

    comparables = get_prices_time_data(params, period="10Y", interval="86400")

    index = ['XLF', '.INX']
    for name in index:
        comparables.drop(labels=[
            "{}_Open".format(name),
            "{}_High".format(name),
            "{}_Low".format(name),
            "{}_Volume".format(name),
        ],
            axis=1, inplace=True
        )
    comparables.to_csv('Sectors.csv')
