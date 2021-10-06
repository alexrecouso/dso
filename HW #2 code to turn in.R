# HW #2 R code template
# Below List teammates official names(each person in a new line): 
#
#
#
# Create code to answer HW #2 questions and submit this file in blackboard
# if the question does not require to write any code just add # see word file

##################################################
#                    Case 1
##################################################

#install.packages("stringr")
#install.packages("tidyr")
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(forecast)

#1
#setwd("~/Desktop/DSO HW2")
nenana1=read.table('Nenana1.txt',header=T,sep="\t",check.names=FALSE)
x1.string="1917 April 28 11:30 AM"
x1.string
x2.string = "1917 April 30 11:45 AM"
x2.string
?as.POSIXlt
x1.time=as.POSIXlt(x1.string, format="%Y %B %e %H:%M %p")
x1.time
x2.time=as.POSIXlt(x2.string, format="%Y %B %e %H:%M %p")
x2.time

difftime(as.POSIXlt(x2.time),as.POSIXlt(x1.time),unit="days")
difftime(as.POSIXlt(x2.time),as.POSIXlt(x1.time),unit="hours")
difftime(as.POSIXlt(x2.time),as.POSIXlt(x1.time),unit="mins")

head(nenana1)
nenana1$'Date & Time'=str_remove(string=nenana1$'Date & Time', pattern="at ")
head(nenana1)

nenana1= nenana1 %>% mutate(IceBreak=str_c(Year,`Date & Time`, sep=' '))
head(nenana1)

nenana1= nenana1 %>% mutate(January1=str_c(Year,'January 1 00:00 AM', sep=' '))
head(nenana1)

glimpse(nenana1)
nenana1$IceBreak=as.POSIXlt(nenana1$IceBreak, format="%Y %B %e %H:%M %p")
glimpse(nenana1)
nenana1$January1 = as.POSIXlt(nenana1$January1, format="%Y %B %e %H:%M %p")
glimpse(nenana1)
head(nenana1)


#2
head(nenana1)
?difftime
nenana1$KPI=difftime(nenana1$IceBreak,nenana1$January1,units="days")
nenana1$KPI
head(nenana1)
glimpse(nenana1)

nenana1$KPI=as.numeric(nenana1$KPI)
head(nenana1)
glimpse(nenana1)


#3
nenana1 %>% ggplot(aes(x=Year,y=KPI))+geom_line()+ theme_bw() + geom_point() # plot KPI over the years


#4 
x=1:5
x
lag(x,n=1)
data.frame(x,xlag1 = lag(x,n=1), xlag2 = lag(x,n=2))

nenana1$Trend = 1:(dim(nenana1)[1])
nenana1$Lag1 =  lag(nenana1$KPI,1)
head(nenana1)
nenana1$Lag2 =  lag(nenana1$KPI,2)
head(nenana1)

nenana1 = nenana1 %>% mutate(Trend = 1:87) #add variable trend

MLag = lm(KPI ~ Trend + Lag1+Lag2, data=nenana1)
summary(MLag)

nenana1$MLag = c(NA,NA,MLag$fitted.values)


nenana1$MLagResiduals =c(NA,NA,MLag$residuals)
Acf(nenana1$MLagResiduals)
Pacf(nenana1$MLagResiduals)
plot(nenana1$MLagResiduals,type="l") # residuals look like white noise
abline(h=0)


# 5


dim(nenana1)
head(nenana1)
M1 = lm(nenana1$KPI ~ Trend,data = nenana1) #run linear model
summary(M1)

nenana1$M1 = M1$fitted.values
head(nenana1)

nenana1$Baseline = mean(nenana1$KPI)
nenana1 %>% ggplot(aes(x=Year,y=KPI))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x=Year,y=M1),col="red")+
  geom_line(aes(x=Year,y=Baseline),col="blue")


#6 

#7


#8


#9


#10


nenana2=read.table('Nenana2.txt',header=T,sep="\t",check.names=FALSE)

x1.string="1917 April 28 11:30 AM"
x1.string
x2.string = "1917 April 30 11:45 AM"
x2.string
?as.POSIXlt
x1.time=as.POSIXlt(x1.string, format="%Y %B %e %H:%M %p")
x1.time
x2.time=as.POSIXlt(x2.string, format="%Y %B %e %H:%M %p")
x2.time

difftime(as.POSIXlt(x2.time),as.POSIXlt(x1.time),unit="days")
difftime(as.POSIXlt(x2.time),as.POSIXlt(x1.time),unit="hours")
difftime(as.POSIXlt(x2.time),as.POSIXlt(x1.time),unit="mins")

head(nenana2)
nenana2$'Date & Time'=str_remove(string=nenana2$'Date & Time', pattern="at ")
head(nenana1)

nenana2= nenana2 %>% mutate(IceBreak=str_c(Year,`Date & Time`, sep=' '))
head(nenana2)

nenana2 = nenana2 %>% mutate(January1=str_c(Year,'January 1 00:00 AM', sep=' '))
head(nenana2)

glimpse(nenana2)
nenana2$IceBreak=as.POSIXlt(nenana2$IceBreak, format="%Y %B %e %H:%M %p")
glimpse(nenana2)
nenana2$January1 = as.POSIXlt(nenana2$January1, format="%Y %B %e %H:%M %p")
glimpse(nenana2)
head(nenana2)

head(nenana2)
nenana2$KPI=difftime(nenana2$IceBreak,nenana2$January1,units="days")
nenana2$KPI
head(nenana2)
glimpse(nenana2)

nenana2$KPI=as.numeric(nenana2$KPI)
head(nenana2)
glimpse(nenana2)

# Assessing abrupt change
nenana2 %>% ggplot(aes(x=Year,y=KPI))+geom_line()+ theme_bw() + geom_point()

x=1:5
lag(x,n=1)
data.frame(x,xlag1 = lag(x,n=1), xlag2 = lag(x,n=2))

nenana2$Lag1 =  lag(nenana2$KPI,1)
nenana2$Lag2 =  lag(nenana2$KPI,2)
nenana2 = nenana2 %>% mutate(Trend = 1:87) 
head(nenana2)

nenana2$Trend = 1:(dim(nenana2)[1])
MLag = lm(KPI ~ Trend + Lag1+Lag2, data=nenana2)
summary(MLag)
nenana2$MLag = c(NA,NA,MLag$fitted.values)
nenana2$MLagResiduals =c(NA,NA,MLag$residuals)

Acf(nenana2$MLagResiduals) 
Pacf(nenana2$MLagResiduals)
plot(nenana2$MLagResiduals,type="l") 
abline(h=0)


# assessing gradual change

M1 = lm(nenana2$KPI ~ Trend,data = nenana2) #run linear model
summary(M1)

nenana2$M1 = M1$fitted.values
nenana2$Baseline = mean(nenana2$KPI)
nenana2 %>% ggplot(aes(x=Year,y=KPI))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x=Year,y=M1),col="red")+
  geom_line(aes(x=Year,y=Baseline),col="blue")



