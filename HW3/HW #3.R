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
library(rvest)
#and load data
url = "https://www.transtats.bts.gov/Data_Elements.aspx?Qn6n=E"
data = url %>% read_html() %>% html_nodes(xpath = '//*[@id="GridView1"]') %>% 
  html_table() 
typeof(data)
data = data[[1]] # overwrite data with data frame
data

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
colnames(data_monthly) = c('Year', 'Month', 'Variable', 'International', 'Total', 'Date')
data_monthly %>% head

#Let's capture trend
data_monthly = data_monthly %>% mutate(Trend = 1:226) 

#And last, create seasonal dummies
data_monthly$JAN = 0
data_monthly$JAN[data_monthly$Month=="1"] = 1
data_monthly$FEB = 0
data_monthly$FEB[data_monthly$Month=="2"] = 1
data_monthly$MAR = 0
data_monthly$MAR[data_monthly$Month=="3"] = 1
data_monthly$APR = 0
data_monthly$APR[data_monthly$Month=="4"] = 1
data_monthly$MAY = 0
data_monthly$MAY[data_monthly$Month=="5"] = 1
data_monthly$JUN = 0
data_monthly$JUN[data_monthly$Month=="6"] = 1
data_monthly$JUL = 0
data_monthly$JUL[data_monthly$Month=="7"] = 1
data_monthly$AUG = 0
data_monthly$AUG[data_monthly$Month=="8"] = 1
data_monthly$SEP = 0
data_monthly$SEP[data_monthly$Month=="9"] = 1
data_monthly$OCT = 0
data_monthly$OCT[data_monthly$Month=="10"] = 1
data_monthly$NOV = 0
data_monthly$NOV[data_monthly$Month=="11"] = 1
data_monthly$DEC = 0
data_monthly$DEC[data_monthly$Month=="12"] = 1

#now let's create the sets for the 3 scenarios
scenario1 = subset(data_monthly, Year<=2008)
scenario1$Train = scenario1$Variable
scenario1$Train[(dim(scenario1)[1]-12+1) : dim(scenario1)[1]] = NA

scenario2 = subset(data_monthly, Year<=2019)
scenario2$Train = scenario2$Variable
scenario2$Train[(dim(scenario2)[1]-12+1) : dim(scenario2)[1]] = NA

scenario3 = subset(data_monthly, Year<=2020)
scenario3$Train = scenario3$Variable
scenario3$Train[(dim(scenario3)[1]-12+1) : dim(scenario3)[1]] = NA


#M0: a baseline model without explanatory variables, 
# built using the simplest regression model
# y = b0 + epsilon
# & using Variable as the KPI
M0S1 = lm(Train ~ 1, data = scenario1) #S1 stands for Scenario 1
summary(M0S1)
scenario1$M0 = predict(M0S1, newdata = scenario1)
scenario1$M0residuals = scenario1$Variable - scenario1$M0
scenario1 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")

M0S2 = lm(Train ~ 1, data = scenario2) #S2 stands for Scenario 2
summary(M0S2)
scenario2$M0 = predict(M0S2, newdata = scenario2)
scenario2$M0residuals = scenario2$Variable - scenario2$M0
scenario2 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")

M0S3 = lm(Train ~ 1, data = scenario3) #S3 stands for Scenario 3
summary(M0S3)
scenario3$M0 = predict(M0S3, newdata = scenario3)
scenario3$M0residuals = scenario3$Variable - scenario3$M0
scenario3 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")


#M1
M1S1 = lm(Train ~ Trend, data = scenario1)
summary(M1S1)
scenario1$M1 = predict(M1S1, newdata = scenario1)
scenario1$M1residuals = scenario1$Variable - scenario1$M1
scenario1 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  scale_x_date(date_labels = "%Y-%m-%d")

M1S2 = lm(Train ~ Trend, data = scenario2)
summary(M1S2)
scenario2$M1 = predict(M1S2, newdata = scenario2)
scenario2$M1residuals = scenario2$Variable - scenario2$M1
scenario2 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  scale_x_date(date_labels = "%Y-%m-%d")

M1S3 = lm(Train ~ Trend, data = scenario3)
summary(M1S3)
scenario3$M1 = predict(M1S3, newdata = scenario3)
scenario3$M1residuals = scenario3$Variable - scenario3$M1
scenario3 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  scale_x_date(date_labels = "%Y-%m-%d")


#M2
# let's capture and model seasonality - this model will capture only seasonal component
# we have monthly data with annual seasonality (12 points per cycle)
# let's use dummy variable
scenario1 %>% group_by(Month) %>% summarise(mean(Variable))
M2S1 = lm(Train ~ JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
        data = scenario1) #to include intercept one month should not be included
summary(M2S1)
scenario1$M2 = predict(M2S1, newdata = scenario1)
scenario1$M2residuals = scenario1$Variable - scenario1$M2
scenario1 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  scale_x_date(date_labels = "%Y-%m-%d")

scenario2 %>% group_by(Month) %>% summarise(mean(Variable))
M2S2 = lm(Train ~ JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = scenario2) #to include intercept one month should not be included
summary(M2S2)
scenario2$M2 = predict(M2S1, newdata = scenario2)
scenario2$M2residuals = scenario2$Variable - scenario2$M2
scenario2 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  scale_x_date(date_labels = "%Y-%m-%d")

scenario3 %>% group_by(Month) %>% summarise(mean(Variable))
M2S3 = lm(Train ~ JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = scenario3) #to include intercept one month should not be included
summary(M2S3)
scenario3$M2 = predict(M2S1, newdata = scenario3)
scenario3$M2residuals = scenario3$Variable - scenario3$M2
scenario3 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  scale_x_date(date_labels = "%Y-%m-%d")


#M3: a model that captures both trend and seasonal components
M3S1 = lm(Train ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
        data = scenario1)
summary(M3S1)
scenario1$M3 = predict(M3S1, newdata = scenario1)
scenario1$M3residuals = scenario1$Variable - scenario1$M3
scenario1 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  geom_line(aes(x = Date, y = M3), col = "orange") +
  scale_x_date(date_labels = "%Y-%m-%d")

M3S2 = lm(Train ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = scenario2)
summary(M3S2)
scenario2$M3 = predict(M3S2, newdata = scenario2)
scenario2$M3residuals = scenario2$Variable - scenario2$M3
scenario2 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  geom_line(aes(x = Date, y = M3), col = "orange") +
  scale_x_date(date_labels = "%Y-%m-%d")

M3S3 = lm(Train ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = scenario3)
summary(M3S3)
scenario3$M3 = predict(M3S3, newdata = scenario3)
scenario3$M3residuals = scenario3$Variable - scenario3$M3
scenario3 %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "green") +
  geom_line(aes(x = Date, y = M3), col = "orange") +
  scale_x_date(date_labels = "%Y-%m-%d")


#Evaluation Metrics for Training Sets
RMSE_S1_train = c(sqrt(mean((scenario1$M0residuals[1:(dim(scenario1)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M1residuals[1:(dim(scenario1)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M2residuals[1:(dim(scenario1)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M3residuals[1:(dim(scenario1)[1]-12)])^2, na.rm = TRUE)))
MAPE_S1_train = c(mean(abs((scenario1$Variable[1:(dim(scenario1)[1]-12)] - 
                      scenario1$M0[1:(dim(scenario1)[1]-12)]) / 
                      scenario1$Variable[1:(dim(scenario1)[1]-12)])),
                  mean(abs((scenario1$Variable[1:(dim(scenario1)[1]-12)] - 
                      scenario1$M1[1:(dim(scenario1)[1]-12)]) / 
                      scenario1$Variable[1:(dim(scenario1)[1]-12)])),
                  mean(abs((scenario1$Variable[1:(dim(scenario1)[1]-12)] - 
                      scenario1$M2[1:(dim(scenario1)[1]-12)]) / 
                      scenario1$Variable[1:(dim(scenario1)[1]-12)])),
                  mean(abs((scenario1$Variable[1:(dim(scenario1)[1]-12)] - 
                      scenario1$M3[1:(dim(scenario1)[1]-12)]) / 
                      scenario1$Variable[1:(dim(scenario1)[1]-12)])))

RMSE_S2_train = c(sqrt(mean((scenario2$M0residuals[1:(dim(scenario2)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M1residuals[1:(dim(scenario2)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M2residuals[1:(dim(scenario2)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M3residuals[1:(dim(scenario2)[1]-12)])^2, na.rm = TRUE)))
MAPE_S2_train = c(mean(abs((scenario2$Variable[1:(dim(scenario2)[1]-12)] - 
                      scenario2$M0[1:(dim(scenario2)[1]-12)]) / 
                      scenario2$Variable[1:(dim(scenario2)[1]-12)])),
                  mean(abs((scenario2$Variable[1:(dim(scenario2)[1]-12)] - 
                      scenario2$M1[1:(dim(scenario2)[1]-12)]) / 
                      scenario2$Variable[1:(dim(scenario2)[1]-12)])),
                  mean(abs((scenario2$Variable[1:(dim(scenario2)[1]-12)] - 
                      scenario2$M2[1:(dim(scenario2)[1]-12)]) / 
                      scenario2$Variable[1:(dim(scenario2)[1]-12)])),
                  mean(abs((scenario2$Variable[1:(dim(scenario2)[1]-12)] - 
                      scenario2$M3[1:(dim(scenario2)[1]-12)]) / 
                      scenario2$Variable[1:(dim(scenario2)[1]-12)])))

RMSE_S3_train = c(sqrt(mean((scenario3$M0residuals[1:(dim(scenario3)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M1residuals[1:(dim(scenario3)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M2residuals[1:(dim(scenario3)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M3residuals[1:(dim(scenario3)[1]-12)])^2, na.rm = TRUE)))
MAPE_S3_train = c(mean(abs((scenario3$Variable[1:(dim(scenario3)[1]-12)] - 
                      scenario3$M0[1:(dim(scenario3)[1]-12)]) / 
                      scenario3$Variable[1:(dim(scenario3)[1]-12)])),
                  mean(abs((scenario3$Variable[1:(dim(scenario3)[1]-12)] - 
                      scenario3$M1[1:(dim(scenario3)[1]-12)]) / 
                      scenario3$Variable[1:(dim(scenario3)[1]-12)])),
                  mean(abs((scenario3$Variable[1:(dim(scenario3)[1]-12)] - 
                      scenario3$M2[1:(dim(scenario3)[1]-12)]) / 
                      scenario3$Variable[1:(dim(scenario3)[1]-12)])),
                  mean(abs((scenario3$Variable[1:(dim(scenario3)[1]-12)] - 
                      scenario3$M3[1:(dim(scenario3)[1]-12)]) / 
                      scenario3$Variable[1:(dim(scenario3)[1]-12)])))

#create evaluation table
accuracy.table0 = data.frame('S1 MAPE' = MAPE_S1_train,
                            'S2 MAPE' = MAPE_S2_train,
                            'S3 MAPE' = MAPE_S3_train)
at0 = as.matrix(accuracy.table0)
MAPE_AVG_train = c(rowMeans(at0))
MAPE_AVG_train

ModelID = paste("M", 0:3, sep = "")
accuracy.table0 = data.frame(ModelID = ModelID, 'S1 RMSE' = RMSE_S1_train, 
                            'S1 MAPE' = MAPE_S1_train,
                            'S2 RMSE' = RMSE_S2_train, 'S2 MAPE' = MAPE_S2_train,
                            'S3 RMSE' = RMSE_S3_train, 'S3 MAPE' = MAPE_S3_train,
                            MAPE_AVG_train = MAPE_AVG_train)
#sort model from most accurate to least accurate based on MAPE
accuracy.table0 %>% arrange(MAPE_AVG_train)


#Evaluation Metrics for Testing Sets
RMSE_S1 = c(sqrt(mean((scenario1$M0residuals[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M1residuals[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M2residuals[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M3residuals[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])^2, na.rm = TRUE)))
MAPE_S1 = c(mean(abs((scenario1$Variable[(dim(scenario1)[1]-12+1):dim(scenario1)[1]] - 
                            scenario1$M0[(dim(scenario1)[1]-12+1):dim(scenario1)[1]]) / 
                            scenario1$Variable[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])),
                  mean(abs((scenario1$Variable[(dim(scenario1)[1]-12+1):dim(scenario1)[1]] - 
                              scenario1$M1[(dim(scenario1)[1]-12+1):dim(scenario1)[1]]) / 
                             scenario1$Variable[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])),
                  mean(abs((scenario1$Variable[(dim(scenario1)[1]-12+1):dim(scenario1)[1]] - 
                              scenario1$M2[(dim(scenario1)[1]-12+1):dim(scenario1)[1]]) / 
                             scenario1$Variable[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])),
                  mean(abs((scenario1$Variable[(dim(scenario1)[1]-12+1):dim(scenario1)[1]] - 
                              scenario1$M3[(dim(scenario1)[1]-12+1):dim(scenario1)[1]]) / 
                             scenario1$Variable[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])))

RMSE_S2 = c(sqrt(mean((scenario2$M0residuals[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M1residuals[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M2residuals[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M3residuals[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])^2, na.rm = TRUE)))
MAPE_S2 = c(mean(abs((scenario2$Variable[(dim(scenario2)[1]-12+1):dim(scenario2)[1]] - 
                              scenario2$M0[(dim(scenario2)[1]-12+1):dim(scenario2)[1]]) / 
                             scenario2$Variable[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])),
                  mean(abs((scenario2$Variable[(dim(scenario2)[1]-12+1):dim(scenario2)[1]] - 
                              scenario2$M1[(dim(scenario2)[1]-12+1):dim(scenario2)[1]]) / 
                             scenario2$Variable[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])),
                  mean(abs((scenario2$Variable[(dim(scenario2)[1]-12+1):dim(scenario2)[1]] - 
                              scenario2$M2[(dim(scenario2)[1]-12+1):dim(scenario2)[1]]) / 
                             scenario2$Variable[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])),
                  mean(abs((scenario2$Variable[(dim(scenario2)[1]-12+1):dim(scenario2)[1]] - 
                              scenario2$M3[(dim(scenario2)[1]-12+1):dim(scenario2)[1]]) / 
                             scenario2$Variable[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])))

RMSE_S3 = c(sqrt(mean((scenario3$M0residuals[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M1residuals[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M2residuals[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M3residuals[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])^2, na.rm = TRUE)))
MAPE_S3 = c(mean(abs((scenario3$Variable[(dim(scenario3)[1]-12+1):dim(scenario3)[1]] - 
                              scenario3$M0[(dim(scenario3)[1]-12+1):dim(scenario3)[1]]) / 
                             scenario3$Variable[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])),
                  mean(abs((scenario3$Variable[(dim(scenario3)[1]-12+1):dim(scenario3)[1]] - 
                              scenario3$M1[(dim(scenario3)[1]-12+1):dim(scenario3)[1]]) / 
                             scenario3$Variable[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])),
                  mean(abs((scenario3$Variable[(dim(scenario3)[1]-12+1):dim(scenario3)[1]] - 
                              scenario3$M2[(dim(scenario3)[1]-12+1):dim(scenario3)[1]]) / 
                             scenario3$Variable[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])),
                  mean(abs((scenario3$Variable[(dim(scenario3)[1]-12+1):dim(scenario3)[1]] - 
                              scenario3$M3[(dim(scenario3)[1]-12+1):dim(scenario3)[1]]) / 
                             scenario3$Variable[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])))

#create evaluation table
accuracy.table1 = data.frame('S1 MAPE' = MAPE_S1,
                            'S2 MAPE' = MAPE_S2,
                            'S3 MAPE' = MAPE_S3)
at1 = as.matrix(accuracy.table1)
MAPE_AVG = c(rowMeans(at1))
MAPE_AVG

ModelID = paste("M", 0:3, sep = "")
accuracy.table = data.frame(ModelID = ModelID, 'S1 RMSE' = RMSE_S1, 
                            'S1 MAPE' = MAPE_S1,
                            'S2 RMSE' = RMSE_S2, 'S2 MAPE' = MAPE_S2,
                            'S3 RMSE' = RMSE_S3, 'S3 MAPE' = MAPE_S3,
                            MAPE_AVG = MAPE_AVG)
#sort model from most accurate to least accurate based on MAPE
accuracy.table %>% arrange(MAPE_AVG)

#####################################################################################

#Evaluation:
