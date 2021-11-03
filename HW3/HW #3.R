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

#Let's decide over which variable we are going to model, in this case DOMESTIC
#For International, just overwrite it with Variable (and revert Variable to Domestic)
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
scenario1$Set = 'Train'
scenario1$Set[(dim(scenario1)[1]-12+1) : dim(scenario1)[1]] = 'Test'

scenario2 = subset(data_monthly, Year<=2019)
scenario2$Train = scenario2$Variable
scenario2$Train[(dim(scenario2)[1]-12+1) : dim(scenario2)[1]] = NA
scenario2$Set = 'Train'
scenario2$Set[(dim(scenario2)[1]-12+1) : dim(scenario2)[1]] = 'Test'

scenario3 = subset(data_monthly, Year<=2020)
scenario3$Train = scenario3$Variable
scenario3$Train[(dim(scenario3)[1]-12+1) : dim(scenario3)[1]] = NA
scenario3$Set = 'Train'
scenario3$Set[(dim(scenario3)[1]-12+1) : dim(scenario3)[1]] = 'Test'


#M0: a baseline model without explanatory variables, 
# built using the simplest regression model
# y = b0 + epsilon
# & using Variable as the KPI
M0S1 = lm(Train ~ 1, data = scenario1) #S1 stands for Scenario 1
summary(M0S1)
scenario1$M0 = predict(M0S1, newdata = scenario1)
scenario1$M0residuals = scenario1$Variable - scenario1$M0
scenario1 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario1 %>% ggplot(aes(x = Date, y = M0residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M0residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario1$M0residuals,lag.max=60)
#EVALUATION: M0 is just a baseline model without explanatory variables,
# so in the graph if residuals we can see no trend, seasons, nor cycles are explained by the model
#same is observed for international instead of domestic

M0S2 = lm(Train ~ 1, data = scenario2) #S2 stands for Scenario 2
summary(M0S2)
scenario2$M0 = predict(M0S2, newdata = scenario2)
scenario2$M0residuals = scenario2$Variable - scenario2$M0
scenario2 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario2 %>% ggplot(aes(x = Date, y = M0residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M0residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario2$M0residuals,lag.max=60)
#EVALUATION: M0 is just a baseline model without explanatory variables,
# so in the graph if residuals we can see no trend, seasons, nor cycles are explained by the model
#same is observed for international instead of domestic

M0S3 = lm(Train ~ 1, data = scenario3) #S3 stands for Scenario 3
summary(M0S3)
scenario3$M0 = predict(M0S3, newdata = scenario3)
scenario3$M0residuals = scenario3$Variable - scenario3$M0
scenario3 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario3 %>% ggplot(aes(x = Date, y = M0residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M0residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario3$M0residuals,lag.max=60)
#EVALUATION: M0 is just a baseline model without explanatory variables,
# so in the graph if residuals we can see no trend, seasons, nor cycles are explained by the model
#same is observed for international instead of domestic


#M1
M1S1 = lm(Train ~ Trend, data = scenario1)
summary(M1S1)
scenario1$M1 = predict(M1S1, newdata = scenario1)
scenario1$M1residuals = scenario1$Variable - scenario1$M1
scenario1 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario1 %>% ggplot(aes(x = Date, y = M1residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M1residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario1$M1residuals,lag.max=60)
#We see there are systematic patterns that can be modeled by including seasonality
#EVALUATION: this model now captures trend, but as seen in the residuals graph we
# can't explain still the seasonality nor what seems to be a cycle in the data
#same is observed for international instead of domestic

M1S2 = lm(Train ~ Trend, data = scenario2)
summary(M1S2)
scenario2$M1 = predict(M1S2, newdata = scenario2)
scenario2$M1residuals = scenario2$Variable - scenario2$M1
scenario2 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario2 %>% ggplot(aes(x = Date, y = M1residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M1residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario2$M1residuals,lag.max=60)
#We see there are systematic patterns that can be modeled by including seasonality
#EVALUATION: this model now captures trend, but as seen in the residuals graph we
# can't explain still the seasonality nor what seems to be a cycle in the data
#same is observed for international instead of domestic

M1S3 = lm(Train ~ Trend, data = scenario3)
summary(M1S3)
scenario3$M1 = predict(M1S3, newdata = scenario3)
scenario3$M1residuals = scenario3$Variable - scenario3$M1
scenario3 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario3 %>% ggplot(aes(x = Date, y = M1residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M1residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario3$M1residuals,lag.max=60)
#We see there are systematic patterns that can be modeled by including seasonality
#EVALUATION: this model now captures trend, but as seen in the residuals graph we
# can't explain still the seasonality nor what seems to be a cycle in the data
#same is observed for international instead of domestic


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
scenario1 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "orange") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario1 %>% ggplot(aes(x = Date, y = M2residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M2residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario1$M2residuals,lag.max=60)
#We see there are systematic patterns that can be modeled by including seasonality
#EVALUATION: this model now captures seasonality, but as seen in the residuals graph we
# can't explain the trend nor what seems to be a cycle in the data
#same is observed for international instead of domestic

scenario2 %>% group_by(Month) %>% summarise(mean(Variable))
M2S2 = lm(Train ~ JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = scenario2) #to include intercept one month should not be included
summary(M2S2)
scenario2$M2 = predict(M2S1, newdata = scenario2)
scenario2$M2residuals = scenario2$Variable - scenario2$M2
scenario2 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "orange") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario2 %>% ggplot(aes(x = Date, y = M2residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M2residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario2$M2residuals,lag.max=60)
#We see there are systematic patterns that can be modeled by including seasonality
#EVALUATION: this model now captures seasonality, but as seen in the residuals graph we
# can't explain the trend nor what seems to be a cycle in the data
#same is observed for international instead of domestic

scenario3 %>% group_by(Month) %>% summarise(mean(Variable))
M2S3 = lm(Train ~ JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = scenario3) #to include intercept one month should not be included
summary(M2S3)
scenario3$M2 = predict(M2S1, newdata = scenario3)
scenario3$M2residuals = scenario3$Variable - scenario3$M2
scenario3 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M0), col = "blue") +
  geom_line(aes(x = Date, y = M1), col = "red") +
  geom_line(aes(x = Date, y = M2), col = "orange") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario3 %>% ggplot(aes(x = Date, y = M2residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M2residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario3$M2residuals,lag.max=60)
#We see there are systematic patterns that can be modeled by including seasonality
#EVALUATION: this model now captures seasonality, but as seen in the residuals graph we
# can't explain the trend nor what seems to be a cycle in the data
#same is observed for international instead of domestic


#M3: a model that captures both trend and seasonal components
M3S1 = lm(Train ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
        data = scenario1)
summary(M3S1)
scenario1$M3 = predict(M3S1, newdata = scenario1)
scenario1$M3residuals = scenario1$Variable - scenario1$M3
scenario1 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M3), col = "black") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario1 %>% ggplot(aes(x = Date, y = M3residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M3residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario1$M3residuals,lag.max=60)
#There are significant lags in the ACF graphs.
#This means that there are systematic patterns that can be modeled by including lags.
#EVALUATION: as we can see now, this model fits much better the data. However, as we see
# on the residuals, we still miss a cyclical component (the rest looks a lot like noise,
# but further analysis will be done to demonstrate it).
#same is observed for international instead of domestic

M3S2 = lm(Train ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = scenario2)
summary(M3S2)
scenario2$M3 = predict(M3S2, newdata = scenario2)
scenario2$M3residuals = scenario2$Variable - scenario2$M3
scenario2 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M3), col = "black") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario2 %>% ggplot(aes(x = Date, y = M3residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M3residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario2$M3residuals,lag.max=60)
#There are significant lags in the ACF graphs.
#This means that there are systematic patterns that can be modeled by including lags.
#EVALUATION: as we can see now, this model fits much better the data. However, as we see
# on the residuals, we still miss a cyclical component (the rest looks a lot like noise,
# but further analysis will be done to demonstrate it).
#same is observed for international instead of domestic

M3S3 = lm(Train ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV,
          data = scenario3)
summary(M3S3)
scenario3$M3 = predict(M3S3, newdata = scenario3)
scenario3$M3residuals = scenario3$Variable - scenario3$M3
scenario3 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M3), col = "black") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario3 %>% ggplot(aes(x = Date, y = M3residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M3residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
tsdisplay(scenario3$M3residuals,lag.max=60)
#There are significant lags in the ACF graphs.
#This means that there are systematic patterns that can be modeled by including lags.
#EVALUATION: as we can see now, this model fits much better the data. However, as we see
# on the residuals, we still miss a cyclical component (the rest looks a lot like noise,
# but further analysis will be done to demonstrate it).
#same is observed for international instead of domestic


#M4: a model that captures trend and seasonal components, and takes lags into account
scenario1$TrainLag1 = lag(scenario1$Train, 1)
scenario1$TrainLag2 = lag(scenario1$Train, 2)
scenario1$TrainLag10 = lag(scenario1$Train, 10)
scenario1$TrainLag12 = lag(scenario1$Train, 12)
scenario1$TrainLag24 = lag(scenario1$Train, 24)
M4S1 = lm(Train ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + 
            TrainLag1 + TrainLag2 + TrainLag10 + TrainLag12 + TrainLag24,
          data = scenario1)
summary(M4S1)
tsdisplay(M4S1$residuals,lag.max=60)
Box.test(M4S1$residuals)
#Since p-value = 0.8403 > 0.05, we fail to reject Ho and conclude that there is
# no statistically significant evidence that data are not independent
#for international instead of domestic we would need further analysis to demonstrate that 
#data are not independent. However since it's 0.49 we would not perform it and adapt to domestic instead
#Since the last model is the final model I am going to store fitted/predicted values 
# on training set + forecast on testing sets in column M4 and residuals in column M4residuals
#Now we create columns M4 and M4residuals and fill them with NAs
scenario1$M4 = NA
scenario1$M4residuals = NA
scenario1$M4[!is.na(scenario1$Train) &!is.na(scenario1$TrainLag1) & !is.na(scenario1$TrainLag2) &
         !is.na(scenario1$TrainLag10) & !is.na(scenario1$TrainLag12)
         & !is.na(scenario1$TrainLag24)] = M4S1$fitted.values
scenario1$M4residuals[!is.na(scenario1$Train) &!is.na(scenario1$TrainLag1) & !is.na(scenario1$TrainLag2) &
                        !is.na(scenario1$TrainLag10) & !is.na(scenario1$TrainLag12)
                      & !is.na(scenario1$TrainLag24)] = M4S1$residuals
#Since lags are included we need to create a loop to calculate predictions
i = dim(scenario1)[1]-12+1 # First testing observation number 
scenario1$M4[i] = predict(M4S1, newdata = scenario1[i,])
for(i in (dim(scenario1)[1]-12+1):(dim(scenario1)[1])){
  scenario1$TrainLag1[i] = ifelse(is.na(scenario1$Train[i-1]), scenario1$M4[i-1], scenario1$Train[i-1]) 
  scenario1$TrainLag2[i] = ifelse(is.na(scenario1$Train[i-2]), scenario1$M4[i-2], scenario1$Train[i-2]) 
  scenario1$TrainLag10[i] = ifelse(is.na(scenario1$Train[i-10]), scenario1$M4[i-10], scenario1$Train[i-10])
  scenario1$TrainLag12[i] = ifelse(is.na(scenario1$Train[i-12]), scenario1$M4[i-12], scenario1$Train[i-12])
  scenario1$TrainLag24[i] = ifelse(is.na(scenario1$Train[i-24]), scenario1$M4[i-24], scenario1$Train[i-24])
  scenario1$M4[i] = predict(M4S1, newdata = scenario1[i,])
}
scenario1$M4residuals = scenario1$Variable - scenario1$M4
scenario1 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M4), col = "black") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario1 %>% ggplot(aes(x = Date, y = M4residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M4residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
#EVALUATION: as we have demonstrated, the residuals of this model are noise. Hence,
# before digging in the metrics, this model is accurate enough

scenario2$TrainLag1 = lag(scenario2$Train, 1)
scenario2$TrainLag2 = lag(scenario2$Train, 2)
scenario2$TrainLag10 = lag(scenario2$Train, 10)
scenario2$TrainLag12 = lag(scenario2$Train, 12)
scenario2$TrainLag24 = lag(scenario2$Train, 24)
M4S2 = lm(Train ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + 
            TrainLag1 + TrainLag2 + TrainLag10 + TrainLag12 + TrainLag24,
          data = scenario2)
summary(M4S2)
tsdisplay(M4S2$residuals,lag.max=60)
Box.test(M4S2$residuals)
#Since p-value = 0.8141 > 0.05, we fail to reject Ho and conclude that there is
# no statistically significant evidence that data are not independent
#same for international instead of domestic (0.55)
#Since the last model is the final model I am going to store fitted/predicted values 
# on training set + forecast on testing sets in column M4 and residuals in column M4residuals
#Now we create columns M4 and M4residuals and fill them with NAs
scenario2$M4 = NA
scenario2$M4residuals = NA
scenario2$M4[!is.na(scenario2$Train) &!is.na(scenario2$TrainLag1) & 
                  !is.na(scenario2$TrainLag2) &
                  !is.na(scenario2$TrainLag10) & 
                  !is.na(scenario2$TrainLag12) & 
                  !is.na(scenario2$TrainLag24)] = M4S2$fitted.values
scenario2$M4residuals[!is.na(scenario2$Train) &!is.na(scenario2$TrainLag1) & 
                           !is.na(scenario2$TrainLag2) &
                           !is.na(scenario2$TrainLag10) & 
                           !is.na(scenario2$TrainLag12) & 
                           !is.na(scenario2$TrainLag24)] = M4S2$residuals
#Since lags are included we need to create a loop to calculate predictions
i = dim(scenario2)[1]-12+1 # First testing observation number 
scenario2$M4[i] = predict(M4S2, newdata = scenario2[i,])
for(i in (dim(scenario2)[1]-12+1):(dim(scenario2)[1])){
  scenario2$TrainLag1[i] = ifelse(is.na(scenario2$Train[i-1]), scenario2$M4[i-1], scenario2$Train[i-1]) 
  scenario2$TrainLag2[i] = ifelse(is.na(scenario2$Train[i-2]), scenario2$M4[i-2], scenario2$Train[i-2]) 
  scenario2$TrainLag10[i] = ifelse(is.na(scenario2$Train[i-10]), scenario2$M4[i-10], scenario2$Train[i-10])
  scenario2$TrainLag12[i] = ifelse(is.na(scenario2$Train[i-12]), scenario2$M4[i-12], scenario2$Train[i-12])
  scenario2$TrainLag24[i] = ifelse(is.na(scenario2$Train[i-24]), scenario2$M4[i-24], scenario2$Train[i-24])
  scenario2$M4[i] = predict(M4S2, newdata = scenario2[i,])
}
scenario2$M4residuals = scenario2$Variable - scenario2$M4
scenario2 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M4), col = "black") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario2 %>% ggplot(aes(x = Date, y = M4residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M4residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
#EVALUATION: as we have demonstrated, the residuals of this model are noise. Hence,
# before digging in the metrics, this model is accurate enough

scenario3$TrainLag1 = lag(scenario3$Train, 1)
scenario3$TrainLag2 = lag(scenario3$Train, 2)
scenario3$TrainLag10 = lag(scenario3$Train, 10)
scenario3$TrainLag12 = lag(scenario3$Train, 12)
scenario3$TrainLag24 = lag(scenario3$Train, 24)
M4S3 = lm(Train ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + 
            TrainLag1 + TrainLag2 + TrainLag10 + TrainLag12 + TrainLag24,
            data = scenario3)
summary(M4S3)
tsdisplay(M4S3$residuals,lag.max=60)
Box.test(M4S3$residuals)
#Since p-value = 0.8794 > 0.05, we fail to reject Ho and conclude that there is
# no statistically significant evidence that data are not independent
#for international instead of domestic we would need further analysis to demonstrate that 
#data are not independent. However since it's 0.3 we would not perform it and adapt to domestic instead
#Since the last model is the final model I am going to store fitted/predicted values 
# on training set + forecast on testing sets in column M4 and residuals in column M4residuals
#Now we create columns M4 and M4residuals and fill them with NAs
scenario3$M4 = NA
scenario3$M4residuals = NA
scenario3$M4[!is.na(scenario3$Train) &!is.na(scenario3$TrainLag1) & 
                  !is.na(scenario3$TrainLag2) &
                  !is.na(scenario3$TrainLag10) & 
                  !is.na(scenario3$TrainLag12) & 
                  !is.na(scenario3$TrainLag24)] = M4S3$fitted.values
scenario3$M4residuals[!is.na(scenario3$Train) &!is.na(scenario3$TrainLag1) & 
                           !is.na(scenario3$TrainLag2) &
                           !is.na(scenario3$TrainLag10) & 
                           !is.na(scenario3$TrainLag12) & 
                           !is.na(scenario3$TrainLag24)] = M4S3$residuals
#Since lags are included we need to create a loop to calculate predictions
i = dim(scenario3)[1]-12+1 # First testing observation number 
scenario3$M4[i] = predict(M4S3, newdata = scenario3[i,])
for(i in (dim(scenario3)[1]-12+1):(dim(scenario3)[1])){
  scenario3$TrainLag1[i] = ifelse(is.na(scenario3$Train[i-1]), scenario3$M4[i-1], scenario3$Train[i-1]) 
  scenario3$TrainLag2[i] = ifelse(is.na(scenario3$Train[i-2]), scenario3$M4[i-2], scenario3$Train[i-2])
  scenario3$TrainLag10[i] = ifelse(is.na(scenario3$Train[i-10]), scenario3$M4[i-10], scenario3$Train[i-10])
  scenario3$TrainLag12[i] = ifelse(is.na(scenario3$Train[i-12]), scenario3$M4[i-12], scenario3$Train[i-12])
  scenario3$TrainLag24[i] = ifelse(is.na(scenario3$Train[i-24]), scenario3$M4[i-24], scenario3$Train[i-24])
  scenario3$M4[i] = predict(M4S3, newdata = scenario3[i,])
}
scenario3$M4residuals = scenario3$Variable - scenario3$M4
scenario3 %>% ggplot(aes(x = Date, y = Variable, color = Set))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = M4), col = "black") +
  scale_x_date(date_labels = "%Y-%m-%d")
scenario3 %>% ggplot(aes(x = Date, y = M4residuals))+
  geom_line() + theme_bw() + 
  geom_line(aes(x = Date, y = M4residuals), col = "blue") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%Y-%m-%d")
#EVALUATION: as we have demonstrated, the residuals of this model are noise. Hence,
# before digging in the metrics, this model is accurate enough


#Evaluation Metrics for Training Sets
RMSE_S1_train = c(sqrt(mean((scenario1$M0residuals[1:(dim(scenario1)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M1residuals[1:(dim(scenario1)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M2residuals[1:(dim(scenario1)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M3residuals[1:(dim(scenario1)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M4residuals[1:(dim(scenario1)[1]-12)])^2, na.rm = TRUE)))
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
                      scenario1$Variable[1:(dim(scenario1)[1]-12)])),
                  mean(abs((scenario1$Variable[1:(dim(scenario1)[1]-12)] - 
                              scenario1$M4[1:(dim(scenario1)[1]-12)]) / 
                             scenario1$Variable[1:(dim(scenario1)[1]-12)]), na.rm = TRUE))

RMSE_S2_train = c(sqrt(mean((scenario2$M0residuals[1:(dim(scenario2)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M1residuals[1:(dim(scenario2)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M2residuals[1:(dim(scenario2)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M3residuals[1:(dim(scenario2)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M4residuals[1:(dim(scenario2)[1]-12)])^2, na.rm = TRUE)))
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
                      scenario2$Variable[1:(dim(scenario2)[1]-12)])),
                  mean(abs((scenario2$Variable[1:(dim(scenario2)[1]-12)] - 
                              scenario2$M4[1:(dim(scenario2)[1]-12)]) / 
                             scenario2$Variable[1:(dim(scenario2)[1]-12)]), na.rm = TRUE))

RMSE_S3_train = c(sqrt(mean((scenario3$M0residuals[1:(dim(scenario3)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M1residuals[1:(dim(scenario3)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M2residuals[1:(dim(scenario3)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M3residuals[1:(dim(scenario3)[1]-12)])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M4residuals[1:(dim(scenario3)[1]-12)])^2, na.rm = TRUE)))
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
                      scenario3$Variable[1:(dim(scenario3)[1]-12)])),
                  mean(abs((scenario3$Variable[1:(dim(scenario3)[1]-12)] - 
                              scenario3$M4[1:(dim(scenario3)[1]-12)]) / 
                             scenario3$Variable[1:(dim(scenario3)[1]-12)]), na.rm = TRUE))

#create evaluation table
accuracy.table0 = data.frame('S1 MAPE' = MAPE_S1_train,
                            'S2 MAPE' = MAPE_S2_train,
                            'S3 MAPE' = MAPE_S3_train)
at0 = as.matrix(accuracy.table0)
MAPE_AVG_train = c(rowMeans(at0))
MAPE_AVG_train

ModelID = paste("M", 0:4, sep = "")
accuracy.table0 = data.frame(ModelID = ModelID, 'S1 RMSE' = RMSE_S1_train, 
                            'S1 MAPE' = MAPE_S1_train,
                            'S2 RMSE' = RMSE_S2_train, 'S2 MAPE' = MAPE_S2_train,
                            'S3 RMSE' = RMSE_S3_train, 'S3 MAPE' = MAPE_S3_train,
                            MAPE_AVG_train = MAPE_AVG_train)
#sort model from most accurate to least accurate based on MAPE
accuracy.table0 %>% arrange(MAPE_AVG_train)
#As explained before, M4 is the most accurate one with respect to the trainning data.
#Metrics support this statement.
#same for international instead of domestic

#Evaluation Metrics for Testing Sets
RMSE_S1 = c(sqrt(mean((scenario1$M0residuals[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M1residuals[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M2residuals[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M3residuals[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario1$M4residuals[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])^2, na.rm = TRUE)))
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
                              scenario1$Variable[(dim(scenario1)[1]-12+1):dim(scenario1)[1]])),
                  mean(abs((scenario1$Variable[(dim(scenario1)[1]-12+1):dim(scenario1)[1]] - 
                            scenario1$M4[(dim(scenario1)[1]-12+1):dim(scenario1)[1]]) / 
                            scenario1$Variable[(dim(scenario1)[1]-12+1):dim(scenario1)[1]]), na.rm = TRUE))

RMSE_S2 = c(sqrt(mean((scenario2$M0residuals[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M1residuals[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M2residuals[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M3residuals[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario2$M4residuals[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])^2, na.rm = TRUE)))
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
                              scenario2$Variable[(dim(scenario2)[1]-12+1):dim(scenario2)[1]])),
                  mean(abs((scenario2$Variable[(dim(scenario2)[1]-12+1):dim(scenario2)[1]] - 
                              scenario2$M4[(dim(scenario2)[1]-12+1):dim(scenario2)[1]]) / 
                              scenario2$Variable[(dim(scenario2)[1]-12+1):dim(scenario2)[1]]), na.rm = TRUE))

RMSE_S3 = c(sqrt(mean((scenario3$M0residuals[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M1residuals[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M2residuals[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M3residuals[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])^2, na.rm = TRUE)),
                  sqrt(mean((scenario3$M4residuals[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])^2, na.rm = TRUE)))
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
                              scenario3$Variable[(dim(scenario3)[1]-12+1):dim(scenario3)[1]])),
                  mean(abs((scenario3$Variable[(dim(scenario3)[1]-12+1):dim(scenario3)[1]] - 
                              scenario3$M4[(dim(scenario3)[1]-12+1):dim(scenario3)[1]]) / 
                              scenario3$Variable[(dim(scenario3)[1]-12+1):dim(scenario3)[1]]), na.rm = TRUE))

#create evaluation table
accuracy.table1 = data.frame('S1 MAPE' = MAPE_S1,
                            'S2 MAPE' = MAPE_S2,
                            'S3 MAPE' = MAPE_S3)
at1 = as.matrix(accuracy.table1)
MAPE_AVG = c(rowMeans(at1))
MAPE_AVG

ModelID = paste("M", 0:4, sep = "")
accuracy.table = data.frame(ModelID = ModelID, 'S1 RMSE' = RMSE_S1, 
                            'S1 MAPE' = MAPE_S1,
                            'S2 RMSE' = RMSE_S2, 'S2 MAPE' = MAPE_S2,
                            'S3 RMSE' = RMSE_S3, 'S3 MAPE' = MAPE_S3,
                            MAPE_AVG = MAPE_AVG)
#sort model from most accurate to least accurate based on MAPE
accuracy.table %>% arrange(MAPE_AVG)
#M4 losses the first position on the testing sets, in my opinion because in S3 
#it misses the prediction by a lot due to the huge impact of covid-19 (while simpler models
#fit less the data and hence the size of the miss is smaller). However, if we look
# at S1 and S2, M4 keeps winning as in the training (so I think this refutes a possible overfitting).
# So I will choose as our champion M4 since it has the lowest Mape on average for training
# and for S1 and S2 for testing (only loses on S3)
#same for international instead of domestic

#####################################################################################

#Deployment:

#Change formats to allow appending of rows
data_monthly$Month = as.numeric(data_monthly$Month)
data_monthly$Date = data_monthly %>% select(Date) %>% unlist %>%  
  str_replace_all("-", "") %>% as.numeric
#Create rows to store predictions
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 8, NA, NA, NA, 20210801, 227,
                                                  0,0,0,0,0,0,0,1,0,0,0,0))
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 9, NA, NA, NA, 20210901, 228,
                                                  0,0,0,0,0,0,0,0,1,0,0,0))
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 10, NA, NA, NA, 20211001, 229,
                                                  0,0,0,0,0,0,0,0,0,1,0,0))
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 11, NA, NA, NA, 20211101, 230,
                                                  0,0,0,0,0,0,0,0,0,0,1,0))
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 12, NA, NA, NA, 20211201, 231,
                                                  0,0,0,0,0,0,0,0,0,0,0,1))
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 1, NA, NA, NA, 20220101, 232,
                                                  1,0,0,0,0,0,0,0,0,0,0,0))
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 2, NA, NA, NA, 20220201, 233,
                                                  0,1,0,0,0,0,0,0,0,0,0,0))
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 3, NA, NA, NA, 20220301, 234,
                                                  0,0,1,0,0,0,0,0,0,0,0,0))
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 4, NA, NA, NA, 20220401, 235,
                                                  0,0,0,1,0,0,0,0,0,0,0,0))
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 5, NA, NA, NA, 20220501, 236,
                                                  0,0,0,0,1,0,0,0,0,0,0,0))
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 6, NA, NA, NA, 20220601, 237,
                                                  0,0,0,0,0,1,0,0,0,0,0,0))
data_monthly[nrow(data_monthly) + 1,] = as.list(c(2021, 7, NA, NA, NA, 20220701, 238,
                                                  0,0,0,0,0,0,1,0,0,0,0,0))
#Change formats to revert original state of the dataframe
data_monthly = data_monthly %>% mutate(Date = str_c(Year, Month, '1', sep='-'))
data_monthly$Date = as.POSIXlt(data_monthly$Date, format="%Y-%m-%d")
data_monthly$Date = as.Date(data_monthly$Date)
data_monthly$Month = as.factor(data_monthly$Month)
data_monthly %>% tail(13)

data_monthly$VariableLag1 = lag(data_monthly$Variable, 1)
data_monthly$VariableLag2 = lag(data_monthly$Variable, 2)
data_monthly$VariableLag10 = lag(data_monthly$Variable, 10)
data_monthly$VariableLag12 = lag(data_monthly$Variable, 12)
data_monthly$VariableLag24 = lag(data_monthly$Variable, 24)
Model = lm(Variable ~ Trend + JAN + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + 
            VariableLag1 + VariableLag2 + VariableLag10 + VariableLag12 + VariableLag24,
          data = data_monthly)
summary(Model)

#Since the last model is the final model I am going to store fitted/predicted values 
# on training set + forecast on testing sets in column Model and residuals in column Modelresiduals
#Now we create columns M4 and M4residuals and fill them with NAs
data_monthly$Model = NA
data_monthly$Modelresiduals = NA
data_monthly$Model[!is.na(data_monthly$Variable) &!is.na(data_monthly$VariableLag1) & !is.na(data_monthly$VariableLag2) &
               !is.na(data_monthly$VariableLag10) & !is.na(data_monthly$VariableLag12)
             & !is.na(data_monthly$VariableLag24)] = Model$fitted.values
data_monthly$Modelresiduals[!is.na(data_monthly$Variable) &!is.na(data_monthly$VariableLag1) & !is.na(data_monthly$VariableLag2) &
                        !is.na(data_monthly$VariableLag10) & !is.na(data_monthly$VariableLag12)
                      & !is.na(data_monthly$VariableLag24)] = Model$residuals

data_monthly$LowerBound = NA
data_monthly$UpperBound = NA

#Since lags are included we need to create a loop to calculate predictions
i = dim(data_monthly)[1]
data_monthly$Model[i] = predict(Model, newdata = data_monthly[i,])
for(i in (dim(data_monthly)[1]-12+1):(dim(data_monthly)[1])){
  data_monthly$VariableLag1[i] = ifelse(is.na(data_monthly$Variable[i-1]), data_monthly$Model[i-1], data_monthly$Variable[i-1]) 
  data_monthly$VariableLag2[i] = ifelse(is.na(data_monthly$Variable[i-2]), data_monthly$Model[i-2], data_monthly$Variable[i-2]) 
  data_monthly$VariableLag10[i] = ifelse(is.na(data_monthly$Variable[i-10]), data_monthly$Model[i-10], data_monthly$Variable[i-10])
  data_monthly$VariableLag12[i] = ifelse(is.na(data_monthly$Variable[i-12]), data_monthly$Model[i-12], data_monthly$Variable[i-12])
  data_monthly$VariableLag24[i] = ifelse(is.na(data_monthly$Variable[i-24]), data_monthly$Model[i-24], data_monthly$Variable[i-24])
  data_monthly$Model[i] = predict(Model, newdata = data_monthly[i,])
  data_monthly$LowerBound[i] = predict(Model, newdata = data_monthly[i,], interval='prediction')[2]
  data_monthly$UpperBound[i] = predict(Model, newdata = data_monthly[i,], interval='prediction')[3]
}

#create table
forecast.table = data.frame('Month' = c('2021 August', '2021 September',
                                        '2021 October', '2021 November',
                                        '2021 December', '2022 January',
                                        '2022 February', '2022 March',
                                        '2022 April', '2022 May',
                                        '2022 June', '2022 July'),
                            'Champion_Model_Forecast' = c(data_monthly$Model[227:238]),
                            '95_lower_bound' = c(data_monthly$LowerBound[227:238]),
                            '95_upper_bound' = c(data_monthly$UpperBound[227:238]))
forecast.table

data_monthly %>% ggplot(aes(x = Date, y = Variable))+
  geom_line()+ theme_bw() + 
  geom_line(aes(x = Date, y = LowerBound), col = "red") +
  geom_line(aes(x = Date, y = UpperBound), col = "green") +
  geom_line(aes(x = Date, y = Model), col = "orange") +
  scale_x_date(date_labels = "%Y-%m-%d")

#we can see it models pretty well the v-shaped recovery, however it incorporates a lot of volatility
#probably due to the huge drop and rapid recovery in the recent data
#as we can see in recent data, the impact of covid in airlines has been notable, that's why
#our model incorporates a lot of uncertainty expressed as volatility. After such an
#abrupt change, it is very difficult to predict the future not only in a quantitative manner but
#also qualitatively. Recent events, evolution of the pandemic, and government regulation
#will all play an essential role in determining the future demand for airplane travel,
#hence effectively affecting our forecasts. Our recommendation is that airlines
#stay flexible and prioritize their ability to manneuver fast and adapt to sudden changes
#in demand, since the current environment is quite volatile and recent news and events such as travel restrictions
#can strongly affect the company's operations.
#In terms of the recession impact, as we have explained we can clearly expect a V-shaped recovery,
# with demand going back to its previous levels relatively easy once the restrictions
#are lifted and airlines are allowed to operate again. However, we should take into account
# the balance sheets of airlines, since they are highly leveraged low-cash companies
# we should note that if the situation prolongates more than usual
# this companies may have cash insufficiency that may lead to bankruptcy.
# However, our quantitative model and qualitative analysis expect the situation to get back
# to normal quite fast, despite current times being uncertain and volatility be expected.