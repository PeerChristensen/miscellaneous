#Time series examples
#Decomposition
#Forecasting with Arima

setwd("/Users/peerchristensen/Desktop")
library(plyr)

##Decomposition
apts=ts(AirPassengers,frequency=12)
f=decompose(apts)
plot(f)
plot(f$figure)

##Forecasting

# build an ARIMA model
fit <- arima(AirPassengers, order = c(1, 0, 0), list(order = c(2,
                                                               1, 0), period = 12))
fore <- predict(fit, n.ahead = 24)
# error bounds at 95% confidence level U <- fore$pred + 2 * fore$se
U <- fore$pred + 2 * fore$se
L <- fore$pred - 2 * fore$se

ts.plot(AirPassengers, fore$pred, U, L,
        col = c(1, 2, 4, 4), lty = c(1, 1, 2, 2))
legend("topleft", col = c(1, 2, 4), lty = c(1, 1, 2),
       c("Actual", "Forecast", "Error Bounds (95% Confidence)"))
