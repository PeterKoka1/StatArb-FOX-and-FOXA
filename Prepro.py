import pandas as pd
import pickle
import bs4 as bs
import os
from os.path import exists
import requests

from googlefinance.client import get_prices_time_data


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

    period = "8Y"
    interval = "86400"

    # tickers.append('.INX')
    for stock in tickers:
        if not exists('SP500/{}'.format(stock)):
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
                    '{}_Volume'.format(stock)
                ]
                ticker.drop(drop_labs, axis=1, inplace=True)
                ticker.rename(columns={'{}_Close'.format(stock):stock}, inplace=True)
                ticker.to_csv('stock_dfs/{}.csv'.format(stock))
                print("Adding {} to stock_dfs directory".format(stock))
            except:
                print("Unable to read URL for: {}".format(stock))

def single_df():
    with open('SP500quotes.pickle', 'rb') as f:
        tickers = pickle.load(f)

    main_df = pd.DataFrame()

    for stock in tickers:
        df = pd.read_csv('stock_dfs/{}.csv'.format(stock))

        # df.reset_index(inplace=True)
        df.rename(columns={'Unnamed: 0':'30m'}, inplace=True)

        period_start = str(df['30m'][0])
        period_end = str(df.tail(1)['30m'])

        if '2017-11-24' in period_end and '2009-11-27' in period_start:
            if main_df.empty:
                main_df = df

            else:
                main_df = main_df.merge(df)
        else:
            pass

    main_df.to_csv('SP500JC.csv')

update_ticks()
goog_pull(request_again=False)
single_df()


