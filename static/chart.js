var chart = LightweightCharts.createChart(document.getElementById('chart'), {
    width: 1000,
    height: 300,
    layout: {
            backgroundColor: '#000000',
            textColor: 'rgba(255, 255, 255, 0.9)',
            },
    grid: {
            vertLines: {
            color: 'rgba(197, 203, 206, 0.5)',
                        },
            horzLines: {
                color: 'rgba(197, 203, 206, 0.5)',
            },
            },
            crosshair: {
                mode: LightweightCharts.CrosshairMode.Normal,
            },
            rightPriceScale: {
                borderColor: 'rgba(197, 203, 206, 0.8)',
            },
            timeScale: {
                borderColor: 'rgba(197, 203, 206, 0.8)',
            },
            });

var candleSeries = chart.addCandlestickSeries({
upColor: 'rgba(255, 144, 0, 1)',
downColor: '#000',
borderDownColor: 'rgba(255, 144, 0, 1)',
borderUpColor: 'rgba(255, 144, 0, 1)',
wickDownColor: 'rgba(255, 144, 0, 1)',
wickUpColor: 'rgba(255, 144, 0, 1)',
});

fetch('http://127.0.0.1:5000/history')
    .then((r) => r.json())
    .then((response) =>{
        console.log(response)
        candleSeries.setData(response);
})

var binanceSocket = new WebSocket("wss://stream.binance.com:9443/ws/ethusdt@kline_1m");

binanceSocket.onmessage = function (event) {
    console.log(event.data);

    var message = JSON.parse(event.data);
    var candle = message.k;
   
    candleSeries.update({
        time:candle.t/1000,
        open:candle.o,
        high:candle.h,
        low: candle.l,
        close:candle.c

    } 
    )
}