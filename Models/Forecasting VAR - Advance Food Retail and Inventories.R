# Fri Nov 12 19:23:54 2021 ------------------------------
(list=ls()); 
library(forecast); library(dplyr) ; library(ggplot2) ; library(gridExtra); 
library(ggthemes); library(vars); library(dplyr); library(Quandl)
setwd("C:/Users/saifd/Onedrive/CBS/5-6 semester/Forecasting in Business and Economics/Exam/Final") 

Quandl.api_key("vrXduZL9AGKsV2h87vWJ")
#Henter alt relevant data og i korrekt format#
sales <- Quandl("FRED/RSAFSNA", type = "ts")
sales <- ts(sales[1:357], frequency = 12, start =1992)
inventories <- ts(read.csv("Retail Inventories Food.csv"), frequency = 12, start =1992) 
inventories <- inventories[,2]

data <- ts.union(sales, inventories) 
data <- ts(data, frequency = 12, start =1992) 

summary(ur.df(sales, type = "drift"))
summary(ur.df(inventories, type = "drift"))

autoplot(data, facets = TRUE)

ggAcf(data,na.action = na.omit, lag.max = 24)


VAR <- VARselect(data ,lag.max=10, type="trend", season = 12)
VAR
#Based on the AIC, VAR(10) seems to be the proper model.
p <- 10

#Let's model the series using a VAR(4)
model.var <- VAR(data,p=10,type="trend")
#Let's investigate the results
summary(model.var)
stargazer::stargazer(model.var$varresult$sales, model.var$varresult$inventories, type="text")
residuals <- resid(model.var)
plot(ts(residuals)); 
#Does sales help predicting inventories?
causality(model.var, cause = "sales")
#Do not reject the null that sales do not Granger-cause inventories at 1%

#Does inventories help predicting sales?
causality(model.var, cause = "inventories")
#Reject the null that inventories do not Granger-cause sales at 1%


par(mfrow=c(2,1), mar=c(3,5,3,3)) 
acf(residuals[ ,1]);pacf(residuals[ ,1])
acf(residuals[ ,2]);pacf(residuals[ ,2])


#################### Impulse Response functions ###############
#The irf() function allows us to look at the impulse response
#functions. Let's look at the response of inventories when we shock sales
h<-60
irf.comp <- irf(model.var,impulse="sales",response="inventories",
                n.ahead=h,boot=TRUE,seed=12345)
#The plot is the response of inventories to a shock in sales. 
plot.impulse <- plot(irf.comp)
irf.comp <- irf(model.var,impulse="sales",response="sales",
                n.ahead=h,boot=TRUE,seed=12345)
#The plot is the response of sales to a shock in sales. 
plot.impulse <- plot(irf.comp)
#Let's look at the response of sales when we shock inventories
irf.st <- irf(model.var,impulse="inventories",response="sales",
              n.ahead=h,boot=TRUE,seed=12345)
#The plot is the response of inventories to a shock in sales. 
plot.impulse <- plot(irf.st)

##################### Forecasting #################
Forecast <- forecast(model.var,h=h)
Forecast
par(mfrow=c(2,1), mar=c(3,3,1,1)) 
autoplot(Forecast)

acf(residuals)

