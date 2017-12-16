import pandas as pd
import pickle

def suitable():
    jc = pd.read_csv('Joined_Closes.csv')
    jc.drop(labels='Unnamed: 0', axis=1, inplace=True)
    jc_corrs = jc.corr(method='pearson')
    high_corrs = {}
    with open('SP500quotes.pickle', 'rb') as f:
        tickers = pickle.load(f)
    for tick in tickers:
        try:
            for i in range(len(jc_corrs[tick])):
                if 0.98 < jc_corrs[tick][i] < 1.0:
                    high_corrs[tick] = "{} with {}".format(str(jc_corrs[tick][i])[:4], jc_corrs.index[i])
        except KeyError:
            pass

    for tick, corr in high_corrs.items():
        print(tick, corr)

suitable()

