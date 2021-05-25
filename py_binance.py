import websocket, json, pprint, talib, numpy
import config
from binance.client import Client
from binance.enums import *

SOCKET = "wss://stream.binance.com:9443/ws/ethbusd@kline_1m"

RSI_PERIOD = 14
RSI_OVERBOUGHT = 70
RSI_OVERSOLD = 30
EMA_PERIOD = 4
TRADE_SYMBOL = 'ETHBUSD'
TRADE_QUANTITY = 0.15

closes = []
ema_list =[]
in_position = False

client = Client(config.API_KEY, config.API_SECRET, tld='us')

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
    
 

    if is_candle_closed:
        print("candle closed at {}".format(close))
        #closes.append(float(close))
        print("closes")

        if len(ema_list) > 4:
            ema_list.pop(0)
        else:
            ema_list.append(float(close))
        
        if len(closes) > 13:
            closes.pop(0)
        else:
            closes.append(float(close))
        
        print ("This is for RSI")
        print(closes)
        print ("This is for EMA")
        print (ema_list)

        if len(ema_list) > EMA_PERIOD:
            print ("ok")
            emaList = numpy.array(ema_list)
            ema = talib.EMA(emaList, 5)
            print("all EMA calculated so far")
            print(ema)
            last_ema = float(ema[-1])
            print("the current EMA is {}".format(last_ema))

        

        if len(closes) > RSI_PERIOD:
            np_closes = numpy.array(closes)
            rsi = talib.RSI(np_closes, RSI_PERIOD)
            print("all rsis calculated so far")
            print(rsi)
            last_rsi = rsi[-1]
            print("the current rsi is {}".format(last_rsi))

            if last_rsi > RSI_OVERBOUGHT and close > last_ema :
                if in_position:
                    print("Overbought! Sell! Sell! Sell!")
                    # put binance sell logic here
                    order_succeeded = order(SIDE_SELL, TRADE_QUANTITY, TRADE_SYMBOL)
                    if order_succeeded:
                        in_position = False
                else:
                    print("It is overbought, but we don't own any. Nothing to do.")
            
            if last_rsi < RSI_OVERSOLD and close < last_ema:
                if in_position:
                    print("It is oversold, but you already own it, nothing to do.")
                else:
                    print("Oversold! Buy! Buy! Buy!")
                    # put binance buy order logic here
                    order_succeeded = order(SIDE_BUY, TRADE_QUANTITY, TRADE_SYMBOL)
                    if order_succeeded:
                        in_position = True

            if in_position:
                trades = client.get_my_trades(symbol='ETHBUSD')
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