from pandas._libs.tslibs import Period
import websocket, json, pprint, talib,csv
import pandas as pd
import numpy as np
import config
from os import close
from binance.client import Client
from binance.enums import *

SOCKET = "wss://stream.binance.com:9443/ws/ethusdt@kline_1m"

RSI_PERIOD = 14
RSI_OVERBOUGHT = 70
RSI_OVERSOLD = 30
EMA_PERIOD = 4
TRADE_SYMBOL = 'ETHUSDT'
TRADE_QUANTITY = 0.15

client = Client(config.API_KEY, config.API_SECRET)

def init():
    csvfile = open('2020_15minutes.csv', 'w', newline='') 
    candlestick_writer = csv.writer(csvfile, delimiter=',')
    candlesticks = client.get_historical_klines("ETHUSDT", Client.KLINE_INTERVAL_1MINUTE, "27 May, 2021", "28 May, 2021")

    for candlestick in  candlesticks:
        candlestick[0] = candlestick[0] / 1000
        candlestick_writer.writerow(candlestick)

    csvfile.close()

    df = pd.read_csv('2020_15minutes.csv', header=None)
    df_5 = df.iloc[-5:,4].tolist()
    df_14 = df.iloc[-15:,4].tolist()
    df_20 = df.iloc[-20:,4].tolist()
    df_50 = df.iloc[-50:,4].tolist()
    df_250 = df.iloc[-250:,4].tolist()
    print ("This is first 5")
    print (df_5)
    print ("This is first 14")
    print (df_14)
    print ("This is first 20")
    print (df_20)
    print ("This is first 50")
    print (df_50)
    print ("This is first 250")
    print (df_250)

    return df_5,df_14,df_20,df_50,df_250


df_5,df_14,df_20,df_50,df_250 = init()

closes = df_14
ema_list = df_5
twenty_list = df_20
fifty_list = df_50
twofifty_list = df_250

in_position = False


def order(side, quantity, symbol,order_type=ORDER_TYPE_MARKET):
    try:
        print("sending order")
        order = client.create_order(symbol=symbol, side=side, type=order_type, quantity=quantity)
        print(order)
    except Exception as e:
        print("an exception occured - {}".format(e))
        return False

    return True

    
def on_open(ws):
    print('opened connection')

def on_close(ws):
    print('closed connection')

def on_message(ws, message):
    global closes, in_position
    
    print('received message')
    json_message = json.loads(message)
    pprint.pprint(json_message)

    candle = json_message['k']

    is_candle_closed = candle['x']
    close = candle['c']
    close = float(close)


    if is_candle_closed:
        print("candle closed at {}".format(close))
        #closes.append(float(close))
        print("closes")

        if len(ema_list) > 4:
            print ("del")
            ema_list.pop(0)
            ema_list.append(float(close))
        else:
            ema_list.append(float(close))
        
        if len(twenty_list) > 19:
            print ("del")
            twenty_list.pop(0)
            twenty_list.append(float(close))
        else:
            twenty_list.append(float(close))
        
        if len(fifty_list) > 49:
            print ("del")
            fifty_list.pop(0)
            fifty_list.append(float(close))
        else:
            fifty_list.append(float(close))

        if len(twofifty_list) > 249:
            print ("del")
            twofifty_list.pop(0)
            twofifty_list.append(float(close))
        else:
            twofifty_list.append(float(close))

        if len(closes) > 14:
            print ("del")
            closes.pop(0)
            closes.append(float(close))
        else:
            closes.append(float(close))
        
        print ("This is for RSI")
        print(closes)
        print ("This is for SMA 5")
        print (ema_list)
        print ("This is for SMA 20")
        print (twenty_list)
        print ("This is for SMA 50")
        print (fifty_list)
        print ("This is for SMA 250")
        print (twofifty_list)
      

    if len(ema_list) > 4:
        print ("ok")
        sma_list = np.array(ema_list)
        five_line = np.sum(sma_list,dtype=np.int32)
        five_line = five_line/5
        print ("the current close is {}".format(close))
        print("the current 5 SMA is {}".format(five_line))
    
    if len(twenty_list) > 19:
        print ("ok")
        smat_list = np.array(twenty_list)
        twenty = np.sum(smat_list,dtype=np.int32)
        twenty = twenty/20
        print("the current 20 SMA is {}".format(twenty))

    if len(fifty_list) > 49:
        print ("ok")
        sma50_list = np.array(fifty_list)
        fiveZero = np.sum(sma50_list,dtype=np.int32)
        fiveZero = fiveZero/50
        print("the current 50 SMA is {}".format(fiveZero))

    if len(twofifty_list) > 249:
        print ("ok")
        sma250_list = np.array(twofifty_list)
        twofivezero = np.sum(sma250_list,dtype=np.int32)
        twofivezero = twofivezero/250
        print("the current 250 SMA is {}".format(twofivezero))

    if len(closes) > RSI_PERIOD:
        np_closes = np.array(closes)
        rsi = talib.RSI(np_closes,RSI_PERIOD)
        print ("This is rsi")
        #print (rsi)
        last_rsi = float(rsi[-1])
        print("the current rsi is {}".format(last_rsi))

    if last_rsi > RSI_OVERBOUGHT and close > five_line and five_line > twenty and twenty > fiveZero and fiveZero > twofivezero :
        if in_position:
            print("Overbought! Sell! Sell! Sell!")
            # put binance sell logic here
            order_succeeded = order(SIDE_SELL, TRADE_QUANTITY, TRADE_SYMBOL)
            if order_succeeded:
                in_position = False
        else:
            print("It is overbought, but we don't own any. Nothing to do.")
    
    if last_rsi < RSI_OVERSOLD and close < five_line and five_line < twenty and twenty < fiveZero and fiveZero < twofivezero:
        if in_position:
            print("It is oversold, but you already own it, nothing to do.")
        else:
            print("Oversold! Buy! Buy! Buy!")
            # put binance buy order logic here
            order_succeeded = order(SIDE_BUY, TRADE_QUANTITY, TRADE_SYMBOL)
            if order_succeeded:
                in_position = True

    if in_position:
            trades = client.get_my_trades(symbol='ETHUSDT')
            last_trade = trades[-1]
            for k,v in last_trade.items():
                if k == 'price':
                    last_price = v
            diff = (float(close)-float(last_price))/float(last_price)*100
            if diff <= -0.01:
                order_succeeded = order(SIDE_SELL, TRADE_QUANTITY, TRADE_SYMBOL)
                if order_succeeded:
                    in_position = False

                
ws = websocket.WebSocketApp(SOCKET, on_open=on_open, on_close=on_close, on_message=on_message)
ws.run_forever()