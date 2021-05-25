from flask import Flask, json, render_template, jsonify
import config,csv
from binance import Client, ThreadedWebsocketManager, ThreadedDepthCacheManager
from binance.enums import *

app = Flask(__name__)

client = Client(config.api_key,config.secret_key)

@app.route("/")
def index():
    
    #order = client.create_order(symbol='LTC',side=SIDE_BUY,type=ORDER_TYPE_LIMIT,timeInForce=TIME_IN_FORCE_GTC,quantity=100,price='0.00001')
    title = 'myFirst'
    info = client.get_account()
    balances = info['balances']
    return render_template('index.html', title=title, my_balances=balances)

@app.route('/buy')
def buy():
    return 'buy'

@app.route('/sell')
def sell():
    return 'sell'

@app.route('/settings')
def settings():
    return 'settings'

@app.route('/history')
def history():
    candlesticks = client.get_historical_klines("ETHUSDT", client.KLINE_INTERVAL_1MINUTE,"11 hours ago UTC")

    processed_candlesticks =[]

    for data in candlesticks:
        candlestick = {
            "time":data[0]/1000,
            "open":data[1],
            "high":data[2],
            "low":data[3],
            "close":data[4]
        }
        processed_candlesticks.append(candlestick)

    return jsonify(processed_candlesticks)