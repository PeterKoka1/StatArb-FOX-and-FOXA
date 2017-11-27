import pandas as pd
import pickle
import bs4 as bs
import os
from os.path import exists
import requests

from googlefinance.client import get_prices_time_data
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

    period = "10Y"
    interval = "86400"

    for stock in tickers:
        if not exists('SP500/{}.csv'.format(stock)):
            try:
                param = [
                    {
                        'q': stock,
                        'x': "SP",
                    },
                ]
                ticker = get_prices_time_data(param, period=period, interval=interval)
                drop_labs = [
                    '{}_Open'.format(stock),
                    '{}_High'.format(stock),
                    '{}_Low'.format(stock),
                ]
                ticker.drop(drop_labs, axis=1, inplace=True)
                ticker.rename(columns={'{}_Close'.format(stock):stock}, inplace=True)
                ticker.to_csv('stock_dfs/{}.csv'.format(stock))
                print("Adding {} to stock_dfs directory".format(stock))
            except:
                print("Unable to read URL for: {}".format(stock))

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
        ],
            axis=1, inplace=True
        )

    comparables.to_csv('stock_dfs/comps.csv')

def single_df():

    with open('SP500quotes.pickle', 'rb') as f:
        tickers = pickle.load(f)

    # tickers.append('comps')
    main_df = pd.DataFrame()

    for stock in tickers:
        try:

            df = pd.read_csv('stock_dfs/{}.csv'.format(stock))

            # df.reset_index(inplace=True)
            df.rename(columns={'Unnamed: 0':'Periods'}, inplace=True)
            period_start = str(df['Periods'][0])
            period_end = str(df.tail(1)['Periods'])

            if '2017-11-24' in period_end and '2007-11-27' in period_start:
                if main_df.empty:
                    main_df = df

                else:
                    main_df = main_df.merge(df)
        except:
            pass

    main_df.to_csv('Joined_Closes.csv')

def currency_data():
    fred = Fred(api_key='cf154315e654c009c1b944f20dd1e028')

    USEUROforex = fred.get_series('DEXUSEU')
    USEUROforex = pd.DataFrame(USEUROforex).iloc[2321:]
    USEUROforex.columns = ['USvsEURO']

    USEUROforex.fillna(
        method='bfill',
        inplace=True
    )

    JC = pd.read_csv('Joined_Closes.csv').iloc[:1478]

    exchange_indices = [str(ix)[0:10] for ix in USEUROforex.index.values]
    JC_indices = [ix[0:10] for ix in JC['Periods']]

    labs = []
    for i in exchange_indices:
        if i not in JC_indices:
            labs.append(i)

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
        ],
            axis=1, inplace=True
        )

    comparables.to_csv('Sectors.csv')

update_ticks()
goog_pull(request_again=False)
single_df()
currency_data()
sectors()
