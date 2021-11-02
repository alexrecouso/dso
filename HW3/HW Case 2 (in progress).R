library(readxl)
library(xlsx)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(dygraphs)
library(forecast)

profitN = "Nike.xlsx" %>% read_xlsx(range="T22:DE22", col_types="numeric",col_names=FALSE) %>% unlist(use.names=FALSE)
datesN = "Nike.xlsx" %>% read_xlsx(range="T11:DE11", col_types="text",col_names=FALSE) %>% unlist(use.names=FALSE)

profitA = "Adidas.xlsx" %>% read_xlsx(range = "T22:DE22",col_types="numeric",col_names = FALSE) %>% unlist(use.names = FALSE)
datesA =  "Adidas.xlsx" %>% read_xlsx(range = "T11:DE11",col_types="text",col_names = FALSE) %>% unlist(use.names = FALSE)


Year = NULL 
for(i in 1999:2020){
  Year = c(Year,rep(i,4))
}
Year = c(Year, rep(2021,2))

Quarter = c(rep(1:4,2020-1999+1),1:2)
length(Quarter)
length(Year)
length(profitN)
data = data.frame(Year,Quarter,profitN,profitA)
                  
head(data)
tail(data)

data$Time = 1:(dim(data)[1])

# Graph points for Nike
data %>% ggplot(aes(x=Time,y=profitN)) + geom_line() + geom_point() +theme_bw()
# we see an upward trend and seasonality

# NA for data for COVID period
data$ActualN = data$profitN
data$profitN[data$Year>2019]=NA
tail(data,10)
data$Quarter = factor(data$Quarter)

# Data visualization 1
M1 = lm(profitN ~ Time + Quarter, data = data)
data$M1 = NA
data$M1[!is.na(data$profitN)]=M1$fitted.values
data %>% ggplot(aes(x=Time,y=profitN)) + geom_line() + geom_point() + theme_bw() + geom_line(aes(x=Time,y=M1),color="red") + geom_point()
tsdisplay(M1$residuals)

# ACF graph shows data is outside of the confidence interval in the positive direction at the beginning
# the autocorrelation among lags decrease and are no longer significant after about lag 3


# Data visualization 2
data$ProfitLag1 = lag(data$profitN,1)
head(data)
tail(data)

M2 = lm(profitN ~ Time + Quarter + ProfitLag1, data = data)
tsdisplay(M2$residuals)

data$M2[!is.na(data$profitN) & !is.na(data$ProfitLag1)] = M2$fitted.values
data %>% ggplot(aes(x=Time,y=profitN)) + geom_line() + geom_point() + theme_bw() + geom_line(aes(x=Time,y=M2),color="red") + geom_point()
tsdisplay(M2$residuals)
# this model is much better and none of the lags are significant


# Data visualization 3
data$ProfitLag2 = lag(data$profitN,2)
data$ProfitLag3 = lag(data$profitN,3)
data$ProfitLag4 = lag(data$profitN,4)

M3 = lm(profitN ~ Time + Quarter + ProfitLag1 + ProfitLag2 + ProfitLag3  + ProfitLag4, data = data)
summary(M3)
tsdisplay(M3$residuals)

data$M3[!is.na(data$profitN) & !is.na(data$ProfitLag1) & !is.na(data$ProfitLag2) & 
          !is.na(data$ProfitLag3) & !is.na(data$ProfitLag4)] = M3$fitted.values

# M2 was better, M3 showed that lag 4 and 5 are significant

for(i in 85:90){
  data$ProfitLag1[i] = ifelse(!is.na(data$profitN[i-1]),data$profitN[i-1],data$M3[i-1])
  data$ProfitLag2[i] = ifelse(!is.na(data$profitN[i-2]),data$profitN[i-2],data$M3[i-2])
  data$ProfitLag3[i] = ifelse(!is.na(data$profitN[i-3]),data$profitN[i-3],data$M3[i-3])
  data$ProfitLag4[i] = ifelse(!is.na(data$profitN[i-4]),data$profitN[i-4],data$M3[i-4])
  data$M3[i] = predict(M3,newdata = data[i,])
}
tail(data,8)

# data, predicted/fitted values on the pre-COVID set and forecast on the testing set
data %>% ggplot(aes(x=Time,y=ActualN)) + geom_line() + geom_point(size=2) + theme_bw() + 
  geom_line(aes(x=Time,y=M3),color="red") + geom_point(aes(x=Time,y=M3),color="red",size=2)


# just pre-COVID set
data[data$Year<2020,] %>% ggplot(aes(x=Time,y=ActualN)) + geom_line() + geom_point(size=2) + theme_bw() + 
  geom_line(aes(x=Time,y=M3),color="red") + geom_point(aes(x=Time,y=M3),color="red",size=2)

# just COVID set
data[data$Year>=2020,] %>% ggplot(aes(x=Time,y=ActualN)) + geom_line() + geom_point(size=2) + theme_bw() + 
  geom_line(aes(x=Time,y=M3),color="red") + geom_point(aes(x=Time,y=M3),color="red",size=2)


# RMSE calculations: 

# errors for model 3
data$M3errors = NA
data$M3errors = data$ActualN - data$M3

# RMSE on pre-COVID set:
sqrt(mean(data[data$Year<2020,"M3errors"]^2,na.rm= TRUE))

# CV on pre-COVID set, we want <10%
paste0(round(100*sqrt(mean(data[data$Year<2020,"M3errors"]^2,na.rm= TRUE)) / mean(data$ActualN[data$Year<2020]),2),"%")
# "4.13%" so it is good

# RMSE on testing set:
sqrt(mean(data[data$Year>=2020,"M3errors"]^2,na.rm= TRUE))

# MAPE calculations: 

# Column of Absolute Percentage errors for M3
data$M3APerrors = NA
data$M3APerrors = abs((data$ActualN - data$M3)/data$ActualN)*100

# MAPE on the pre-COVID set:
paste0(round(mean(data$M3APerrors[data$Year<2020],na.rm = TRUE),2),"%")
# "3.73%" so it is good

# MAPE on the testing set:
paste0(round(mean(data$M3APerrors[data$Year>=2020],na.rm = TRUE),2),"%")
# "19.98%" is greater than 10% sso the MAPE during test years over the time period of COVID is too large

# COVID impact in $
sum(data$M3errors[data$Year>=2020])
# -1078093

# COVID impact in %
paste0(round(100 * sum(data$M3errors[data$Year>=2020]) / sum(data$ActualN[data$Year>=2020]),2),"%")
# "-4.28%"

###############################################
# ADIDAS


# Graph points for Adidas
data %>% ggplot(aes(x=Time,y=profitA)) + geom_line() + geom_point() +theme_bw()
# we see an upward trend and seasonality

# NA for data for COVID period
data$ActualN = data$profitA
data$profitA[data$Year>2019]=NA
tail(data,10)
data$Quarter = factor(data$Quarter)

# Data visualization 1
M1A = lm(profitA ~ Time + Quarter, data = data)
data$M1A = NA
data$M1A[!is.na(data$profitA)]=M1A$fitted.values
data %>% ggplot(aes(x=Time,y=profitA)) + geom_line() + geom_point() + theme_bw() + geom_line(aes(x=Time,y=M1A),color="red") + geom_point()
tsdisplay(M1A$residuals)

# ACF graph shows data is outside of the confidence interval in the positive direction at the beginning
# the autocorrelation among lags decrease and are no longer significant after about lag 3


# Data visualization 2
data$ProfitALag1 = lag(data$profitA,1)
head(data)
tail(data)

M2A = lm(profitA ~ Time + Quarter + ProfitALag1, data = data)
tsdisplay(M2A$residuals)

data$M2A[!is.na(data$profitA) & !is.na(data$ProfitALag1)] = M2A$fitted.values
data %>% ggplot(aes(x=Time,y=profitA)) + geom_line() + geom_point() + theme_bw() + geom_line(aes(x=Time,y=M2),color="red") + geom_point()
tsdisplay(M2A$residuals)
# this model is much better and none of the lags are significant


# Data visualization 3
data$ProfitALag2 = lag(data$profitA,2)
data$ProfitALag3 = lag(data$profitA,3)
data$ProfitALag4 = lag(data$profitA,4)

M3A = lm(profitA ~ Time + Quarter + ProfitALag1 + ProfitALag2 + ProfitALag3  + ProfitALag4, data = data)
summary(M3A)
tsdisplay(M3A$residuals)

data$M3A[!is.na(data$profitA) & !is.na(data$ProfitALag1) & !is.na(data$ProfitALag2) & 
          !is.na(data$ProfitALag3) & !is.na(data$ProfitALag4)] = M3A$fitted.values

# M2 was better, M3 showed that lag 4 and 5 are significant

for(i in 85:90){
  data$ProfitALag1[i] = ifelse(!is.na(data$profitA[i-1]),data$profitA[i-1],data$M3A[i-1])
  data$ProfitALag2[i] = ifelse(!is.na(data$profitA[i-2]),data$profitA[i-2],data$M3A[i-2])
  data$ProfitALag3[i] = ifelse(!is.na(data$profitA[i-3]),data$profitA[i-3],data$M3A[i-3])
  data$ProfitALag4[i] = ifelse(!is.na(data$profitA[i-4]),data$profitA[i-4],data$M3A[i-4])
  data$M3A[i] = predict(M3A,newdata = data[i,])
}
tail(data,8)

# data, predicted/fitted values on the pre-COVID set and forecast on the testing set
data %>% ggplot(aes(x=Time,y=ActualA)) + geom_line() + geom_point(size=2) + theme_bw() + 
  geom_line(aes(x=Time,y=M3A),color="red") + geom_point(aes(x=Time,y=M3A),color="red",size=2)


# just pre-COVID set
data[data$Year<2020,] %>% ggplot(aes(x=Time,y=ActualA)) + geom_line() + geom_point(size=2) + theme_bw() + 
  geom_line(aes(x=Time,y=M3A),color="red") + geom_point(aes(x=Time,y=M3A),color="red",size=2)

# just COVID set
data[data$Year>=2020,] %>% ggplot(aes(x=Time,y=ActualA)) + geom_line() + geom_point(size=2) + theme_bw() + 
  geom_line(aes(x=Time,y=M3A),color="red") + geom_point(aes(x=Time,y=M3A),color="red",size=2)


# RMSE calculations: 

# errors for model 3
data$M3Aerrors = NA
data$M3Aerrors = data$Actual - data$M3A

# RMSE on pre-COVID set:
sqrt(mean(data[data$Year<2020,"M3Aerrors"]^2,na.rm= TRUE))

# CV on pre-COVID set, we want <10%
paste0(round(100*sqrt(mean(data[data$Year<2020,"M3Aerrors"]^2,na.rm= TRUE)) / mean(data$ActualA[data$Year<2020]),2),"%")
# "4.13%" so it is good

# RMSE on testing set:
sqrt(mean(data[data$Year>=2020,"M3errors"]^2,na.rm= TRUE))

# MAPE calculations: 

# Column of Absolute Percentage errors for M3
data$M3AAPerrors = NA
data$M3AAPerrors = abs((data$ActualA - data$M3A)/data$ActualA)*100

# MAPE on the pre-COVID set:
paste0(round(mean(data$M3AAPerrors[data$Year<2020],na.rm = TRUE),2),"%")
# "3.73%" so it is good

# MAPE on the testing set:
paste0(round(mean(data$M3AAPerrors[data$Year>=2020],na.rm = TRUE),2),"%")
# "19.98%" is greater than 10% sso the MAPE during test years over the time period of COVID is too large

# COVID impact in $
sum(data$M3Aerrors[data$Year>=2020])
# -1078093

# COVID impact in %
paste0(round(100 * sum(data$M3Aerrors[data$Year>=2020]) / sum(data$ActualA[data$Year>=2020]),2),"%")
# "-4.28%"


#ARIMA

y.a = ts(na.omit(profitA),start=c(1999,1),frequency = 4)

y.a.train = window(y.a,end = c(2019,4))
y.a.test = window(y.a,start = c(2020,1))

tsdisplay(y.a.train,lag.max = 16)
tsdisplay(diff(y.a.train),lag.max = 16)
tsdisplay(diff(diff(y.a.train),lag=4,frequency=1),lag.max = 16)

# After playing with models, the following  though it is a bit complex but produces residuals that resemble noise
M = Arima(y.a.train,order = c(3,1,0),seasonal = c(3,1,0),fixed=c(0,0,NA,NA,NA,NA))
# y(t-1), y(t-2),y(t-3),y(t-4),y(t-8),y(t-12) fixed= 0 means lag is not included, while NA means lag is included
# fixed=c(0,0,NA,NA,NA,NA)
M
tsdisplay(M$residuals,lag.max = 16)

autoplot(y.a.train,size=2) + geom_point(size=5) +  theme_bw() + autolayer(M$fitted,size=2)

MF= forecast(M,h=length(y.a.test),level=95)
# To extract future forecasts specified by h from MF object: MF$mean
# MF$mean gives h future forecasts

autoplot(MF)

autoplot(y.a,size=2) + geom_point(size=5) +  theme_bw() + autolayer(MF$fitted,size=2,series="Fitted values") + 
  autolayer(MF$mean,size=2,series="Forecast during \n covid period")

# Covid impact in $
sum(y.a.test-MF$mean)

# Covid impact in %

paste0(round(100 * sum(y.a.test-MF$mean) / sum(y.a.test),2),"%")
