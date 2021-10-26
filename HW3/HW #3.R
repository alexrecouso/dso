#Case 1

#Business Understanding:
#the project objective is to build a predictive model
#to forecast future passenger demand for next 12 months for supporting 
#management decisions on a variety of application areas.

#####################################################################################

#Data Preparation:
#first, I downloaded data as .xls from the URL provided. I eliminated non-desired
#rows in MSExcel, and saved it as Passengers.xlsx to import it to RStudio

#setwd("C:/Users/alexrecouso/Desktop") #set desired working directory
#now load the libraries needed
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(dygraphs)
library(forecast)
#and load data
data = read_xlsx('Passengers.xlsx')
data %>% head

#let's transform columns DOMESTIC and INTERNATIONAL to numeric
data$DOMESTIC = data %>% select(DOMESTIC) %>% unlist %>%  
  str_replace_all(",", "") %>% as.numeric
data$INTERNATIONAL = data %>% select(INTERNATIONAL) %>% unlist %>%  
  str_replace_all(",", "") %>% as.numeric
str(data) #done;)
#now rename columns for consistency
colnames(data)=c('Year', 'Month', 'Domestic', 'International', 'Total')

#####################################################################################

#Data Understanding:
#create subsets to make plotting easier
data_yearly = subset(data, Month=='TOTAL',
                  select=c(Year, Domestic, International)) #only total data per year
data_monthly = subset(data, Month!='TOTAL',
                     select=c(Year, Month, Domestic, International)) #only monthly data
#we create a Date feature
data_monthly = data_monthly %>% mutate(Date = str_c(Year, Month, '1', sep='-'))
data_monthly$Date = as.POSIXlt(data_monthly$Date, format="%Y-%m-%d")
#convert Month to factor & Date to date
data_monthly$Month = factor(data_monthly$Month)
data_monthly$Date = as.Date(data_monthly$Date)
data_monthly %>% head #check tibble is good
#divide by 1M to make graphs more appealing
data_yearly$Domestic = data_yearly$Domestic/1000000
data_yearly$International = data_yearly$International/1000000
data_monthly$Domestic = data_monthly$Domestic/1000000
data_monthly$International = data_monthly$International/1000000

#####################################################################################

#DATA VISUALIZATION 1
#plot domestic flights per year
data_monthly %>% ggplot(aes(x = Date, y = Domestic)) +
  geom_line(color = 'blue') + geom_point() + scale_x_date(date_labels = "%Y-%m-%d") +
  ggtitle('Domestic Flights per Year (in Millions)')
#plot international flights per year
data_monthly %>% ggplot(aes(x = Date, y = International)) +
  geom_line(color = 'blue') + geom_point() + scale_x_date(date_labels = "%Y-%m-%d") +
  ggtitle('International Flights per Year (in Millions)')
#The first thing we see is that number of flights, both domestic and international,
# experienced an enormous decrease in 2020 with respect to 2019 and their previous
# trend, which was bullish. However, in late 2020 and 2021 we see a quick recover
# is starting. However, it is still far below the point before the crash.
#Around 2008&2009 we also see a much smaller disruption of growth in the trend, 
# mostly for domestic flights, but which recovered in the following years.
#Last, we appreciate some kind of seasonality, which could be perceived better
# if we had colored the months.


#DATA VISUALIZATION 2
#so let's color the months...
#plot domestic flights per year
data_monthly %>% ggplot(aes(x = Date, y = Domestic, color = Month)) +
  geom_line(color = 'blue') + geom_point() + scale_x_date(date_labels = "%Y-%m-%d") +
  ggtitle('Domestic Flights per Year (in Millions)')
#plot international flights per year
data_monthly %>% ggplot(aes(x = Date, y = International, color = Month)) +
  geom_line(color = 'blue') + geom_point() + scale_x_date(date_labels = "%Y-%m-%d") +
  ggtitle('International Flights per Year (in Millions)')
#After coloring the months, we see that every year the pattern followed by 
# flights is the same. We hit the lowest of the season in month2, then increase
# in month3 to decrease again again in month4, and roughly increase until 
# the maximum in month7. Then decrease until month9, recover a little in month10
# and stay more or less steady until the decrease of month1 that leads again 
# to the minimum in month2.
#This is true for both domestic and international flights.
#Also it's interesting that the minimum hit in month2 of 2020 was already below 
# the trend expected (even though covid19 was noticed around month3).


#DATA VISUALIZATION 3
#Recreate graph
data_monthly %>% head
data_dygraph = ts(data_monthly[,c('Domestic', 'International')], 
                           start = c(2002,10), frequency = 12)
dygraph(data_dygraph) %>% 
  dySeries("International", axis = 'y2') %>% 
  dyAxis("y", label = "DOMESTIC, in mln passengers") %>% 
  dyRangeSelector()

#####################################################################################

#Data Modeling:
#let's create the monthly dataset to recover decimal places lost in division by 1M
data_monthly = subset(data, Month!='TOTAL',
                      select=c(Year, Month, Domestic, International))
data_monthly$Total = data_monthly$Domestic + data_monthly$International
data_monthly = data_monthly %>% mutate(Date = str_c(Year, Month, '1', sep='-'))
data_monthly$Date = as.POSIXlt(data_monthly$Date, format="%Y-%m-%d")
#convert Month to factor & Date to date
data_monthly$Month = factor(data_monthly$Month)
data_monthly$Date = as.Date(data_monthly$Date)
data_monthly %>% head

#Let's decide over which variable we are going to model
colnames(data_monthly) = c('Year', 'Month', 'Variable', 'International', 'Total')
data_monthly %>% head

#first let's create the sets for the 3 scenarios
train1 = subset(data_monthly, Year<2008,
                select=c(Year, Month, Date, Variable))
test1 = subset(data_monthly, Year>=2008 & Year<2009,
               select=c(Year, Month, Date, Variable))
train2 = subset(data_monthly, Year<2019,
                select=c(Year, Month, Date, Variable))
test2 = subset(data_monthly, Year>=2019 & Year<2020,
               select=c(Year, Month, Date, Variable))
train3 = subset(data_monthly, Year<2020,
                select=c(Year, Month, Date, Variable))
test3 = subset(data_monthly, Year>=2020 & Year<2021,
                     select=c(Year, Month, Date, Variable))


#M0: a baseline model without explanatory variables, 
# built using the simplest regression model
# y = b0 + epsilon
# & using Variable as the KPI
M0S1 = lm(Variable ~ 1, data = train1) #S1 stands for Scenario 1
summary(M0S1)
train1$M0 = M0S1$fitted.values
train1 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")

M0S2 = lm(Variable ~ 1, data = train2) #S2 stands for Scenario 2
summary(M0S2)
train2$M0 = M0S2$fitted.values
train2 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")

M0S3 = lm(Variable ~ 1, data = train3) #S3 stands for Scenario 3
summary(M0S3)
train3$M0 = M0S3$fitted.values
train3 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")


#M1
dim(train1)
train1 = train1 %>% mutate(Trend = 1:63) #let's capture trend
M1S1 = lm(Variable ~ Trend, data = train1)
summary(M1S1)
train1$M1 = M1S1$fitted.values
train1 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  scale_x_date(date_labels = "%Y-%m-%d")

dim(train2)
train2 = train2 %>% mutate(Trend = 1:3) #let's capture trend
M1S2 = lm(Variable ~ Trend, data = train2)
summary(M1S2)
train2$M1 = M1S2$fitted.values
train2 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  scale_x_date(date_labels = "%Y-%m-%d")

dim(train3)
train3 = train3 %>% mutate(Trend = 1:207) #let's capture trend
M1S3 = lm(Variable ~ Trend, data = train3)
summary(M1S3)
train3$M1 = M1S3$fitted.values
train3 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  scale_x_date(date_labels = "%Y-%m-%d")


#M2
# let's capture and model seasonality - this model will capture only seasonal component
# we have monthly data with annual seasonality (12 points per cycle)
# let's use dummy variable
head(train1)
train1$JAN = 0
train1$JAN[train1$Month=="1"] = 1
train1$FEB = 0
train1$FEB[train1$Month=="2"] = 1
train1$MAR = 0
train1$MAR[train1$Month=="3"] = 1
train1$APR = 0
train1$APR[train1$Month=="4"] = 1
train1$MAY = 0
train1$MAY[train1$Month=="5"] = 1
train1$JUN = 0
train1$JUN[train1$Month=="6"] = 1
train1$JUL = 0
train1$JUL[train1$Month=="7"] = 1
train1$AUG = 0
train1$AUG[train1$Month=="8"] = 1
train1$SEP = 0
train1$SEP[train1$Month=="9"] = 1
train1$OCT = 0
train1$OCT[train1$Month=="10"] = 1
train1$NOV = 0
train1$NOV[train1$Month=="11"] = 1
train1$DEC = 0
train1$DEC[train1$Month=="12"] = 1
head(train1)
train1 %>% group_by(Month) %>% summarise(mean(Variable))
M2S1 = lm(Variable ~ JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
        data = train1) #to include intercept one month should not be included
summary(M2S1)
train1$M2 = M2S1$fitted.values
train1 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  scale_x_date(date_labels = "%Y-%m-%d")

head(train2)
train2$JAN = 0
train2$JAN[train1$Month=="1"] = 1
train2$FEB = 0
train2$FEB[train1$Month=="2"] = 1
train2$MAR = 0
train2$MAR[train1$Month=="3"] = 1
train2$APR = 0
train2$APR[train1$Month=="4"] = 1
train2$MAY = 0
train2$MAY[train1$Month=="5"] = 1
train2$JUN = 0
train2$JUN[train1$Month=="6"] = 1
train2$JUL = 0
train2$JUL[train1$Month=="7"] = 1
train2$AUG = 0
train2$AUG[train1$Month=="8"] = 1
train2$SEP = 0
train2$SEP[train1$Month=="9"] = 1
train2$OCT = 0
train2$OCT[train1$Month=="10"] = 1
train2$NOV = 0
train2$NOV[train1$Month=="11"] = 1
train2$DEC = 0
train2$DEC[train1$Month=="12"] = 1
head(train2)
train2 %>% group_by(Month) %>% summarise(mean(Variable))
M2S2 = lm(Variable ~ JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = train2) #to include intercept one month should not be included
summary(M2S2)
train2$M2 = M2S2$fitted.values
train2 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  scale_x_date(date_labels = "%Y-%m-%d")

head(train3)
train3$JAN = 0
train3$JAN[train1$Month=="1"] = 1
train3$FEB = 0
train3$FEB[train1$Month=="2"] = 1
train3$MAR = 0
train3$MAR[train1$Month=="3"] = 1
train3$APR = 0
train3$APR[train1$Month=="4"] = 1
train3$MAY = 0
train3$MAY[train1$Month=="5"] = 1
train3$JUN = 0
train3$JUN[train1$Month=="6"] = 1
train3$JUL = 0
train3$JUL[train1$Month=="7"] = 1
train3$AUG = 0
train3$AUG[train1$Month=="8"] = 1
train3$SEP = 0
train3$SEP[train1$Month=="9"] = 1
train3$OCT = 0
train3$OCT[train1$Month=="10"] = 1
train3$NOV = 0
train3$NOV[train1$Month=="11"] = 1
train3$DEC = 0
train3$DEC[train1$Month=="12"] = 1
head(train3)
train3 %>% group_by(Month) %>% summarise(mean(Variable))
M2S3 = lm(Variable ~ JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = train3) #to include intercept one month should not be included
summary(M2S3)
train3$M2 = M2S3$fitted.values
train3 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  scale_x_date(date_labels = "%Y-%m-%d")


#M3: a model that captures both trend and seasonal components
M3S1 = lm(Variable ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
        data = train1)
summary(M3S1)
train1$M3 = M3S1$fitted.values
train1 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  geom_line(aes(x = Date, y = M3), col = "orange") +
  scale_x_date(date_labels = "%Y-%m-%d")

M3S2 = lm(Variable ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = train2)
summary(M3S2)
train2$M3 = M3S2$fitted.values
train2 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  geom_line(aes(x = Date, y = M3), col = "orange") +
  scale_x_date(date_labels = "%Y-%m-%d")

M3S3 = lm(Variable ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = train3)
summary(M3S3)
train3$M3 = M3S3$fitted.values
train3 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  geom_line(aes(x = Date, y = M3), col = "orange") +
  scale_x_date(date_labels = "%Y-%m-%d")


#Evaluation Metrics -------- here we have to use testing datasets???
RMSE_S1 = c(sqrt(mean(M0S1$residuals^2))*100,
            sqrt(mean(M1S1$residuals^2))*100,
            sqrt(mean(M2S1$residuals^2))*100,
            sqrt(mean(M3S1$residuals^2))*100)
MAPE_S1 = c(mean(abs((train1$Variable - train1$M0)/train1$Variable),na.rm=TRUE)*100,
         mean(abs((train1$Variable - train1$M1)/train1$Variable),na.rm=TRUE)*100,
         mean(abs((train1$Variable - train1$M2)/train1$Variable),na.rm=TRUE)*100,
         mean(abs((train1$Variable - train1$M3)/train1$Variable),na.rm=TRUE)*100)

RMSE_S2 = c(sqrt(mean(M0S2$residuals^2))*100,
            sqrt(mean(M1S2$residuals^2))*100,
            sqrt(mean(M2S2$residuals^2))*100,
            sqrt(mean(M3S2$residuals^2))*100)
MAPE_S2 = c(mean(abs((train2$Variable - train2$M0)/train2$Variable),na.rm=TRUE)*100,
            mean(abs((train2$Variable - train2$M1)/train2$Variable),na.rm=TRUE)*100,
            mean(abs((train2$Variable - train2$M2)/train2$Variable),na.rm=TRUE)*100,
            mean(abs((train2$Variable - train2$M3)/train2$Variable),na.rm=TRUE)*100)

RMSE_S3 = c(sqrt(mean(M0S3$residuals^2))*100,
            sqrt(mean(M1S3$residuals^2))*100,
            sqrt(mean(M2S3$residuals^2))*100,
            sqrt(mean(M3S3$residuals^2))*100)
MAPE_S3 = c(mean(abs((train3$Variable - train3$M0)/train3$Variable),na.rm=TRUE)*100,
            mean(abs((train3$Variable - train3$M1)/train3$Variable),na.rm=TRUE)*100,
            mean(abs((train3$Variable - train3$M2)/train3$Variable),na.rm=TRUE)*100,
            mean(abs((train3$Variable - train3$M3)/train3$Variable),na.rm=TRUE)*100)

# now create evaluation table
accuracy.table = data.frame('S1 MAPE' = MAPE_S1,
                            'S2 MAPE' = MAPE_S2,
                            'S3 MAPE' = MAPE_S3)
at = as.matrix(accuracy.table)
MAPE_AVG = c(rowMeans(at))
MAPE_AVG

ModelID = paste("M",0:3,sep="")
accuracy.table = data.frame(ModelID = ModelID, 'S1 RMSE' = RMSE_S1, 'S1 MAPE' = MAPE_S1,
                            'S2 RMSE' = RMSE_S2, 'S2 MAPE' = MAPE_S2,
                            'S3 RMSE' = RMSE_S3, 'S3 MAPE' = MAPE_S3,
                            MAPE_AVG = MAPE_AVG)
# sort model from most accurate to least accurate based on MAPE
accuracy.table %>% arrange(MAPE_AVG)

#####################################################################################

#Evaluation:
