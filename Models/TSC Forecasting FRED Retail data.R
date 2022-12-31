# Fri Nov 12 19:23:54 2021 ------------------------------
(list=ls()); require(forecast); library(dplyr) ; library(ggplot2) ; library(gridExtra); library(ggthemes)
setwd("C:/Users/saifd/Desktop/Analyser/Forecasting") 
data <- read.csv("RETAILMNSA.csv")
data <- data %>% 
  select("RETAILSMNSA") 

#NUMBER BLOCK####
#Useful application-specific numbers block
s <- 12 #set up the data frequency
startyear <- 1
startperiod <- 1; #month or quarter when you data actually starts
h <- 12 #choose the length of your forecast
T <- length(data$RETAILSMNSA) #set up the length of your data
years <- T/s #NOTE: works only if each year has a full set of seasonal observations


#####PLOT OF DATA#####
data <- ts(data, start=c(startyear,startperiod),frequency=s); autoplot(data)
y<-log(data); autoplot(y)

#####Seasonally Adjusted Data#####
dcmp <- decompose(y); autoplot(y-dcmp$seasonal)
######TREND#####
t=(1:T)
t2=(t^2)
trend <- lm(y~ t+t2, data)

summary(trend)

a <- autoplot(y) +
  autolayer(y-trend$residuals, color = "green") +
  labs(y = "Retail in Millions of Dollars", 
       title = "Retail Trend")

#Series detrended. Residuals showing clear seasonal pattern
a1 <- autoplot(y-trend$fitted.values) +
  labs(y = "Retail in Millions of Dollars", 
       title = "Residuals: Seasonal Component")

######Season######
M <- seasonaldummy(y)

TrSeasQ <- model.matrix(~ t + t2 + M)
trendseason <- lm(y ~ t + t2 + M, data)

summary(trendseason)

#####Plot of trend season#####
b <- autoplot(y) + 
  autolayer(y-trendseason$residuals, color = "firebrick", alpha = 0.8) +
  labs(y = "Retail in Millions of Dollars", 
       title = "Retail Trend and Season")

#Series showing a Cyclical Component 
b1 <- autoplot(y-trendseason$fitted.values) +
  labs(y = "Retail in Millions of Dollars", 
       title = "Residuals: Cyclical Component")

#####PLOT OF RESIDUALS####
ggAcf(trendseason$residuals)
ggPacf(trendseason$residuals)
checkresiduals(trendseason)

ggplot(data.frame(trendseason$residuals), aes(x = trendseason$residuals)) +
  geom_histogram(color = "white", binwidth = 0.015)

accuracy(trendseason)
#####ARIMA MODEL OF CYCLICAL COMPONENT#####
model.arma <- auto.arima(trendseason$residuals, d=0, seasonal = FALSE, ic = "aic",
                         stepwise = FALSE, 
                         approximation = FALSE, trace = TRUE)

#####ARMA3 model####
model.ar3 <- Arima(y,order = c(3, 0, 0),include.mean = FALSE, xreg = TrSeasQ)
summary(model.ar3)
#####TSC model#####
TSC <- y-model.ar3$residuals

c <- autoplot(y) +
  autolayer(TSC, color = "orangered") +
  labs(y = "Retail in Millions of Dollars", 
       title = "Retail TSC model with AR(3)") +
  theme(legend.position = "none")


myforecast <- forecast(model.ar3, h=h, xreg=TrSeasQ)
autoplot(myforecast)


#####Forecast#####
h <- s*2
t <- ((T+1):(T+h))
t2 <- t^2
M <- I(seasonaldummy(y,h))
ForecastTrSeas <- model.matrix(~ t+t2+M)
FIT <- y-model.ar3$residuals
myforecast <- forecast(model.ar3, h=h, xreg=ForecastTrSeas)

#UN-log the forecast back to the levels via exp( )
myforecast$mean<-exp(myforecast$mean)
myforecast$upper<-exp(myforecast$upper)
myforecast$lower<-exp(myforecast$lower)
myforecast$x<-exp(myforecast$x)
myforecast$fitted<-exp(myforecast$fitted)
myforecast

FIT <- exp(FIT) #Un-log the regression fit
e <- autoplot(myforecast) +
  autolayer(myforecast$fitted,color="steelblue", alpha = 0.85) +
  labs(y = "Retail in Millions of Dollars", 
       title = "Retail TSC model Forecast")


TSCResiduals <- autoplot(data-myforecast$fitted) +
  labs(y = "Retail in Millions of Dollars", 
       title = "Residuals: White Noise")

#####ALL PLOTS COMBINED####
grid.arrange(a, a1, b, b1, c, TSCResiduals, ncol = 2)

grid.arrange(a, a1)
grid.arrange(b,b1)
grid.arrange(c,d)
grid.arrange(c,e)

#Residuals Check
grid.arrange(e, TSCResiduals , HW, HWResiduals)

#####Holt Winters Forecast#####
HWSmooth <- HoltWinters(data)
HWForecast <- forecast(HWSmooth, h=h) 

HW <- autoplot(HWForecast) +
  autolayer(HWForecast$fitted, color = "red", alpha = 0.75) + 
  labs(y = "Retail in Millions of Dollars",
       title = "Retail Holt Winters Model Forecast") 

HWResiduals <- autoplot(data-HWForecast$fitted) +
  labs(y = "Retail in Millions of Dollars", 
       title = "Residuals: White Noise");HW

acf(HWForecast$residuals, na.action = na.pass); pacf(HWForecast$residuals, na.action = na.pass)


accuracy(myforecast); accuracy(HWForecast)

