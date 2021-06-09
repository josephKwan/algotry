from os import close
import websocket, json, pprint, talib, numpy as np,config,csv
import pandas as pd
from binance.client import Client
from binance.enums import *
import datetime
SOCKET = "wss://stream.binance.com:9443/ws/ethbusd@kline_1m"
client = Client(config.API_KEY, config.API_SECRET)

# prices = client.get_all_tickers()

# for price in prices:
#     print(price)
print (datetime.datetime.now())
csvfile = open('2020_15minutes.csv', 'w', newline='') 
candlestick_writer = csv.writer(csvfile, delimiter=',')
candlesticks = client.get_historical_klines("ETHUSDT", Client.KLINE_INTERVAL_1MINUTE, "26 May, 2021", "27 May, 2021")


for candlestick in  candlesticks:
    candlestick[0] = candlestick[0] / 1000
    candlestick_writer.writerow(candlestick)

csvfile.close()

df = pd.read_csv('2020_15minutes.csv', header=None)
df_5 = df.iloc[-5:,4].tolist()
#df_5 = np.sum(df_5)/5
df_20 = np.array(df.iloc[-20:,4])
#df_20 = np.sum(df_20)/20


print (df_5)
print (df_20.shape[0])

trades = client.get_my_trades(symbol='ETHUSDT')

print (trades)