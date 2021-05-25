import backtrader as bt 

class RSIStrategy(bt.Strategy):

    def __init__(self):
        self.rsi = bt.talib.RSI(self.data, period=14)

    def next(self):
        if self.rsi < 30 and not self.position :
            self.buy(size=1)
        
        if self.rsi > 70 and self.position:
            self.close()
        

cerebro = bt.Cerebro()

data = bt.feeds.GenericCSVData(dataname=r'C:\Users\Administrator\Desktop\binance_project\algotry\data\2017-2021.csv',    
    dtformat=2
    )

cerebro.adddata(data)

cerebro.addstrategy(RSIStrategy)

cerebro.run()

cerebro.plot()