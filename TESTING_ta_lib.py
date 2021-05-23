import numpy
import talib
from talib import MA_Type

from numpy import genfromtxt

file = r'C:\Users\Administrator\Desktop\binance_project\algotry\15mins.csv'
mydata =  genfromtxt(file,delimiter=',')

#print (mydata)

close = mydata[:,4]


rsi = talib.RSI(close)

print (rsi)