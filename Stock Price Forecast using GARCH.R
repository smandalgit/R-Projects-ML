library(tseries)
library(FinTS) #install FinTS package from https://R-forge.Rproject.org
library(rugarch)
library(zoo)
library(forecast)
#getting the data from yahoo finance website
AAPL_data <- get.hist.quote(instrument = "AAPL",start = "2014-01-01",end = "2019-01-01",quote = "AdjClose",
                            provider = "yahoo",compression = "d",retclass = "zoo")
head(AAPL_data)
plot(AAPL_data,main = "APPL closing Data",ylab = "PRICE (USD)",xlab = "Date")
AAPL_return <- diff(log(AAPL_data))*100
head(AAPL_return)
plot(AAPL_return,main = "Compounded Monthly Returns",xlab = "Date",ylab = "%age Return")
fit_arima <- auto.arima(AAPL_return,trace = TRUE,test = "kpss",ic = "bic")
fit_arima
#Check ARCH Effect
Box.test(fit_arima$residuals^2,lag = 12,type = "Ljung-Box")
resd_garch11_specs <- ugarchspec(variance.model = list(garchOrder = c(1,1),mean.model = list(armaOrder = c(1,1))))
resd_garch11_fit <- ugarchfit(spec = resd_garch11_specs,data = AAPL_return)
resd_garch11_fit

contrl <- list(tol = 10^(-5),delta = 10^(-6))
resd_garch11_roll <- ugarchroll(resd_garch11_specs,data = AAPL_return,n.start = 150,refit.every = 1,
                                refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = 0.01,
                                keep.coef = TRUE,solver.control = contrl,fit.control = list(scale = 1))


report(resd_garch11_roll,type = "VaR",VaR.alpha = 0.01,conf.level = 0.99)
plot(resd_garch11_fit)
#Forecast for 12 periods ahead
resd_garch11_forecast <- ugarchforecast(resd_garch11_fit,n.ahead = 12)
resd_garch11_forecast
