import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import warnings
warnings.filterwarnings("ignore")


def back_test():
    df1 = pd.read_csv('signals1.csv')
    df1.drop(labels=['Unnamed: 0'], axis=1, inplace=True)
    df1.rename(columns={'trade.sig': 'trade_sig', 'hedge': 'spread'}, inplace=True)

    zEntries = [1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0]
    exitZscore = 0.0

    cum_rets = pd.DataFrame()

    for z in zEntries:
        df1['long entry'] = ((df1.trade_sig < - z) & (df1.trade_sig.shift(1) > - z))
        df1['long exit'] = ((df1.trade_sig > - exitZscore) & (df1.trade_sig.shift(1) < - exitZscore))
        df1['num units long'] = np.nan
        df1.loc[df1['long entry'], 'num units long'] = 1
        df1.loc[df1['long exit'], 'num units long'] = 0
        df1['num units long'][0] = 0
        df1['num units long'] = df1['num units long'].fillna(method='pad')

        # set up num units short
        df1['short entry'] = ((df1.trade_sig > z) & (df1.trade_sig.shift(1) < z))
        df1['short exit'] = ((df1.trade_sig < exitZscore) & (df1.trade_sig.shift(1) > exitZscore))
        df1.loc[df1['short entry'], 'num units short'] = -1
        df1.loc[df1['short exit'], 'num units short'] = 0
        df1['num units short'][0] = 0
        df1['num units short'] = df1['num units short'].fillna(method='pad')

        df1['numUnits'] = df1['num units long'] + df1['num units short']
        df1['spread pct ch'] = (df1['spread'] - df1['spread'].shift(1)) / (
            (df1['FOXA'] * (-1.0 * df1['beta'])) + df1['FOX'])
        df1['port rets'] = df1['spread pct ch'] * df1['numUnits'].shift(1)

        df1['cum rets'] = df1['port rets'].cumsum()
        df1['cum rets'] = df1['cum rets'] + 1

        if cum_rets.empty:
            cum_rets = pd.DataFrame(df1['cum rets'])
            cum_rets.rename(columns={'cum rets': 'cumRets_{}'.format(z)}, inplace=True)
        else:
            cum_rets = cum_rets.join(pd.DataFrame(df1['cum rets']), how="outer")
            cum_rets.rename(columns={'cum rets': 'cumRets_{}'.format(z)}, inplace=True)

    print("\nBACKTEST FOX AND FOXA:\n")
    print("2016 CAGR:\n")
    for i in cum_rets.columns:
        print(
            "Z-Score Entry {}: ".format(i[8:11]),
            str((cum_rets[i][249] - cum_rets[i][1])*100)[0:4], "\b%"
        )
    print("\n2017 CAGR\n")
    for i in cum_rets.columns:
        print(
            "Z-Score Entry of {}: ".format(i[8:11]),
            str((cum_rets[i][489] - cum_rets[i][250])*100)[0:4], "\b%"
        )
    maxCAGR = 0.0
    bestZ = ""
    for i in cum_rets.columns:
        # i = float(i[8:11])
        if float((cum_rets[i][489] - cum_rets[i][1])*100) > float(maxCAGR):
            maxCAGR = (cum_rets[i][489] - cum_rets[i][1])*100
            bestZ = i
    print("\nBest 2-yr return {} with {} ZScore Entry".format(
        str((cum_rets[bestZ][489] - cum_rets[bestZ][1])*100)[0:5],
        bestZ[8:11])
    )
    cum_rets.to_csv("Cumulative_Returns.csv")

back_test()