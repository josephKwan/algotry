import config,csv
from binance import Client, ThreadedWebsocketManager, ThreadedDepthCacheManager

client = Client(config.api_key, config.secret_key)
prices = client.get_all_tickers()
# print (prices)

# for price in prices:
    # print (price)

#candles = client.get_klines(symbol='BTCBUSD', interval=Client.KLINE_INTERVAL_30MINUTE)
# candles = client.get_klines(symbol='BTCBUSD', interval=Client.KLINE_INTERVAL_15MINUTE)
# print (len(candles))

csvfile = open('2017-2021.csv','w',newline='')
candlestick_writer = csv.writer(csvfile,delimiter=',')

klines = client.get_historical_klines("ETHUSDT", Client.KLINE_INTERVAL_5MINUTE, "19 May, 2021", "20 May, 2021")
# for candlestick in candles:
    # print (candlestick)
    # candlestick_writer.writerow(candlestick)
for kline in klines:
    candlestick_writer.writerow(kline)
