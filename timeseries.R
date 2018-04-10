rm(list = ls(all.names = TRUE))

setwd("E:\\rahul\\Cognizant_Bangalore_Training\\day_9\\Time_Series_Activity\\time_series_activity")

# Data description:
# Yearly mean total sunspot number obtained by taking a
# simple arithmetic mean of the daily total sunspot
# number over all days of each year. 
# (NB: in early years in particular before 1749, 
#  the means are computed on only a fraction of the
#  days in each year because on many days, no observation is available).

# read the data from csv file... no header

data <- read.csv("sunspots_yearly.csv", header = FALSE)

# we need to split the data so that we can use test data later for 
# error calculation

traindata <- data[1:269,]

testdata <- data[270:289,]

# convert the traindata into the time series
# this is annual data, so frequency parameter is 1

sunspotseries <- ts(traindata, start = c(1700),frequency = 1)

# plotting

plot.ts(sunspotseries)

# Decomposing TS

sunspotcomponents <- decompose(sunspotseries)

# time series has no or less than 2 periods

plot(sunspotcomponents) # will lead to error... read above comment

#Moving averages

library(TTR)

par(mfrow=c(1,1))
    
plot(sunspotseries)

SMAsunspots4 <- SMA(sunspotseries, n=4)
SMAsunspots4[1:10]
plot.ts(SMAsunspots4)

SMAsunspots8 <- SMA(sunspotseries,n=8)
plot.ts(SMAsunspots8)

WMAsunspots4 <- WMA(sunspotseries, n=4)
WMAsunspots4
plot.ts(WMAsunspots4)

WMAsunspots8 <- WMA(sunspotseries, n=8)
plot.ts(WMAsunspots8)


EMAsunspots4 <- EMA(sunspotseries, n=4)
EMAsunspots4
plot.ts(EMAsunspots4)

EMAsunspots8 <- EMA(sunspotseries, n=8)
plot.ts(EMAsunspots8)

#Effect of K
par(mfrow=c(1,1))
plot(sunspotseries, type="l", col="green")
lines(SMAsunspots4, col="red")
lines(SMAsunspots8, col="blue")
lines(WMAsunspots4, col="blue")
lines(EMAsunspots4,col="black")


par(mfrow = c(1, 2))
plot.ts(SMAsunspots4)
plot.ts(SMAsunspots8)
par(mfrow = c(1, 1))


# Holtwinters model - now there is no seasonality, but there is trend

# gamma represents seasonality

sunspotforecast <- HoltWinters(sunspotseries, gamma = FALSE)
sunspotforecast
plot(sunspotforecast)
sunspotforecast$SSE

# Forecasting with Holt-Winters Model
# forecasting for next 20 years, so that we can compare
# with the test data

library(forecast)
HoltPrediction <- forecast.HoltWinters(sunspotforecast,h=20)

plot(HoltPrediction)

# checking model fitness

acf(HoltPrediction$residuals, lag.max = 20, 
    main = "Holt-Winters residuals ACF Plot")

Box.test(HoltPrediction$residuals, lag = 20, type = "Ljung-Box")

qqnorm(HoltPrediction$residuals)
qqline(HoltPrediction$residuals)

mean(HoltPrediction$residuals)

# p-value is signifcant, so there is strong evidence of
# non-zero autocorrelations. So model is not very good

#Calculating the Error metrics for Holt-Winters model

library(DMwR)

regr.eval(testdata , data.frame(HoltPrediction)$Point.Forecast)

# ARIMA Models

# Its necessary for ARIMA models to have stationary time series.
# Stationary time series means that mean is about zero and
# there is constant variance. If the mean is not near zero, then
# we do differencing and if the variance is not constant, then
# it is necessary to transform the original data. 

#Differencing the time series and make it stationary
par(mfrow=c(1,3))
plot.ts(sunspotseries,main="Actual Data")
timeseriesdiff1 <-  diff(sunspotseries, differences=1)
plot.ts(timeseriesdiff1,main="Data with one difference")

timeseriesdiff2 <- diff(sunspotseries, differences=2)
plot.ts(timeseriesdiff2,main="Data with two differences")

# ndiffs function from forecast package gives the order
# of differencing, but sometimes its not the best choice

ndiffs(sunspotseries)

# We will use the order of differencing as d = 0 here.
# d = 0 refers to the original timeseries.
# To get the p and q values for the ARIMA model, its necessary
# to look at the pacf and acf plots of the differenced time
# series.

acf(sunspotseries, lag.max=20)
pacf(sunspotseries, lag.max=20)

# From the plots, we will choose p = 2 and q = 2
# We already decided that d = 0

# Fitting the Arima Model by change p,d,q values in order = c(p,d,q)

fit1 <- arima(sunspotseries, order=c(2,0,2))

fit1
# Check model fitness
par(mfrow=c(1,1))

acf(fit1$residuals)
Box.test(fit1$residuals, type="Ljung-Box")

# p-value is not significant, so there is no evidence
# of non-zero autocorrelation. This is good for the
# model. We can also check if the residuals follow
# normal distribution

qqnorm(fit1$residuals)
qqline(fit1$residuals)

mean(fit1$residuals)

# Forecasting with ARIMA

# Once the model is assessed, we can do the forecast
# We can forecast for next 20 time periods so that we
# can compare with the testdata

arimaforecast1 <- forecast.Arima(fit1 , h = 20)

plot.forecast(arimaforecast1)

# Error metrics

require(DMwR)

regr.eval(testdata , data.frame(arimaforecast1)$Point.Forecast)

# Auto Arima

# auto arima picks up values of p,d,q automatically using
# internal algorithm. We use the original timeseries as 
# an input to the auto.arima function. We don't give the
# differenced time series as an input here.

fit2 <- auto.arima(sunspotseries)

fit2

# Again we can check the model fitness here

acf(fit2$residuals)
Box.test(fit2$residuals, type="Ljung-Box")

# p-value is not significant, so there is no evidence
# of non-zero autocorrelation. This is good for the
# model. We can also check if the residuals follow
# normal distribution

qqnorm(fit2$residuals)
qqline(fit2$residuals)

mean(fit2$residuals)

# Forecasting with Auto Arima

arimaforecast2 <- forecast.Arima(fit2 , h = 20)

plot.forecast(arimaforecast2)

# Error metrics

require(DMwR)

regr.eval(testdata , data.frame(arimaforecast2)$Point.Forecast)

# Models with less AIC are better, also models with lower forecast
# errors are better