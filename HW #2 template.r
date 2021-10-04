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


nenana1$Lag1 =  lag(nenana1$KPI,1)
head(nenana1)
nenana1$Lag2 =  lag(nenana1$KPI,2)
head(nenana1)

nenana1 = nenana1 %>% mutate(Trend = 1:87) #add variable trend

MLag = lm(KPI ~ Trend + Lag1+Lag2, data=nenana1)
summary(MLag)

nenana1$MLag = c(NA,NA,MLag$fitted.values) # plot data and overlay fitted values


nenana1$MLagResiduals =c(NA,NA,MLag$residuals)
Acf(nenana1$MLagResiduals) # model is significant because none of the lines crosses the blue line
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
head(nenana2)

nenana2$'Date & Time'=str_remove(string=nenana2$'Date & Time', pattern="at ")
head(nenana2)

nenana2 = nenana2 %>% mutate(IceBreak=str_c(Year,`Date & Time`, sep=' '))
head(nenana2)

nenana2 = nenana2 %>% mutate(January1=str_c(Year,'January 1 00:00 AM', sep=' '))
head(nenana2)

glimpse(nenana2)
nenana2$IceBreak=as.POSIXlt(nenana2$IceBreak, format="%Y %B %e %H:%M %p")
glimpse(nenana2)
nenana2$January2 = as.POSIXlt(nenana2$January1, format="%Y %B %e %H:%M %p")
glimpse(nenana2)
head(nenana2)

nenana2$KPI=difftime(nenana2$IceBreak,nenana2$January1,units="days")
nenana2$KPI
head(nenana2)
glimpse(nenana2)

nenana2$KPI=as.numeric(nenana2$KPI)
head(nenana2)
glimpse(nenana2)


##################################################
#                    Case 2
##################################################

# Import libraries again just in case
library(tidyr)
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)

# Remember to set desired working dir
setwd("~/Desktop/DSO") #in my case

#1
ozone = read.table('dtwnLAozone.txt', skip=2, header=TRUE)
head(ozone)

ozone = ozone %>% as.matrix %>% melt %>% 
  rename(Year=Var1, MonthShort=Var2, ozone=value) %>% arrange(Year, MonthShort)
ozone %>% head
ozone %>% tail

#2 Graph the data and describe the patterns you see in the data set.

ozone %>% ggplot(aes(x=Year,y=ozone))+
  geom_point()+ theme_bw() + geom_vline(xintercept=1960, color="red", size=0.2)

ozone %>% ggplot(aes(group=Year, x=Year,y=ozone, fill=Year)) + 
  geom_boxplot(outlier.colour = 'Green') + theme(legend.position = 'none') + 
  geom_vline(xintercept=1960, color="red", size=0.2)

#3
#Does the data provide statistically significant evidence that the opening of 
#the Golden State Freeway and the implementation of Rule 63 in 1960 reduced the 
#pollution statistically significantly?

#Ha: Intervention was effective and reduced pollution
#Ho: Intervention was not effective and did not reduce pollution
#Tolerance level alpha = 0.05 = 5%
#Carry out 2 sample t-test to assess whether the intervention was effective
before1960 = ozone %>% filter(Year < 1960)
after1960 = ozone %>% filter(Year >= 1960 & Year < 1966) #because in 1966 new
#regulations were introduced and we don't want them to contaminate the sample
t.test(x= before1960$ozone, y=after1960$ozone, alternative="greater")
#since p-value = 0.00000002391 < alpha = 0.05
# the data provides statistically significant evidence 
# against Ho, hence the intervention is effective.
#Let's visualize the difference
ozone1 = ozone[1:132,] #again we leave data after 1966
ozone1 %>% ggplot(aes(group=Year, x=Year, y=ozone, fill=Year)) + 
  geom_boxplot(outlier.colour = 'Green') + theme(legend.position = 'none') + 
  geom_vline(xintercept=1960, color="red", size=0.2) +
  geom_hline(yintercept=mean(before1960$ozone), color="blue")+
  geom_hline(yintercept=mean(after1960$ozone), color="magenta")+
  theme_bw()

#Evaluate both the abrupt and the gradual changes
#Report and interpret the estimates of both the abrupt and the gradual changes in ozone.

# Let's implement the abrupt change using regression model
# We create a dummy variable 0/1 and call it 'Dummy'
ozone1$Dummy = 0
ozone1$Dummy[61:132] = 1
M1 = lm(ozone ~ Dummy, data=ozone1)
summary(M1)
# since in lm output p-values are two sided...
# p-value = 2.4e-8/2 = 0.0000000024
# Create a graph to show how regression fits the data
ozone1$M1 = M1$fitted.values
ozone1 %>% ggplot(mapping=aes(x=Year,y=ozone)) + geom_point() + 
  geom_vline(xintercept=1960, color="red", size=0.2) +
  geom_point(mapping=aes(x=Year, y=M1, col=as.factor(Dummy)))+
  theme_bw()

# Let's implement the gradual change using regression model
# Ho: Intervention was NOT effective (slope of Ramp is NOT negative)
# Ha: Intervention is effective (slope of Ramp is negative)
# create ramp for gradual change
ozone1$Ramp = c(rep(0,60),1:72)
ozone1
# now capture Ramp using regression model
M2 = lm(ozone ~ Ramp, data=ozone1)
summary(M2)
# since in lm output p-values are two sided...
# p-value = 0.000156/2 = 0.000078
# Create a graph to show how regression fits the data
ozone1$M2 = M2$fitted.values
ozone1 %>% ggplot(mapping=aes(x=Year,y=ozone)) + geom_point() + 
  geom_vline(xintercept=1960, color="red", size=0.2) +
  geom_point(mapping=aes(x=Year, y=M1, col=as.factor(Dummy))) +
  theme_bw() +
  geom_point(mapping=aes(x=Year, y=M2), col="magenta")

# I think a mix of abrupt and gradual change is more appropriate
M3 = lm(ozone ~ Dummy + Ramp, data=ozone1)
summary(M3)
# Create a graph to show how regression fits the data
ozone1$M3 = M3$fitted.values
ozone1 %>% ggplot(mapping=aes(x=Year, y=ozone)) + geom_point() + 
  geom_vline(xintercept=1960, color="red", size=0.2) +
  geom_point(mapping=aes(x=Year, y=M3, col="magenta"))+
  theme_bw()


#4 Does the data provide statistically significant evidence that the regulations 
# implemented in 1966 requiring engine changes in new cars  reduced the pollution 
# statistically significantly?

#Ha: Intervention was effective and reduced pollution
#Ho: Intervention was not effective and did not reduce pollution
#Tolerance level alpha = 0.05 = 5%
#Carry out 2 sample t-test to assess whether the intervention was effective
before1966 = ozone %>% filter(Year < 1966)
after1966 = ozone %>% filter(Year >= 1966)
t.test(x= before1966$ozone, y=after1966$ozone, alternative="greater")
#since p-value = 3.695e-08 = 0.00000003695 < alpha = 0.05
# the data provides statistically significant evidence 
# against Ho, hence the intervention in 1966 is effective.

#Let's visualize the difference
ozone %>% ggplot(aes(group=Year, x=Year,y=ozone, fill=Year)) + 
  geom_boxplot(outlier.colour = 'Green') + theme(legend.position = 'none') + 
  geom_vline(xintercept=1966, color="red", size=0.2) +
  geom_hline(yintercept=mean(before1966$ozone), color="blue")+
  geom_hline(yintercept=mean(after1966$ozone), color="magenta")+
  theme_bw()

#Evaluate both the abrupt and the gradual changes
#Report and interpret the estimates of both the abrupt and the gradual changes in ozone.

# Let's implement the abrupt change using regression model
# We create a dummy variable 0/1 and call it 'dummy'
ozone2 = ozone
ozone2$Dummy = 0
ozone2 %>% tail
ozone2$Dummy[133:216] = 1
ozone2 %>% filter(Year >= 1965 & Year < 1967)
M1 = lm(ozone ~ Dummy, data=ozone2)
summary(M1)
# since in lm output p-values are two sided...
# p-value = 2.38e-07/2 = 0.000000119
# Create a graph to show how regression fits the data
ozone2$M1 = M1$fitted.values
ozone2 %>% ggplot(mapping=aes(x=Year,y=ozone)) + geom_point() + 
  geom_vline(xintercept=1966, color="red", size=0.2) +
  geom_point(mapping=aes(x=Year, y=M1, col=as.factor(Dummy)))+
  theme_bw()

#Let's do the same but taking only into account after 1960 intervention happened
ozone2b = ozone %>% filter(Year >= 1960)
ozone2b$Dummy = 0
ozone2b
ozone2b$Dummy[73:156] = 1
ozone2b %>% filter(Year >= 1965 & Year < 1967)
M1 = lm(ozone ~ Dummy, data=ozone2b)
summary(M1)
# Create a graph to show how regression fits the data
ozone2b$M1 = M1$fitted.values
ozone2b %>% ggplot(mapping=aes(x=Year,y=ozone)) + geom_point() + 
  geom_vline(xintercept=1966, color="red", size=0.2) +
  geom_point(mapping=aes(x=Year, y=M1, col=as.factor(Dummy)))+
  theme_bw()
#We see the significance is much smaller comparing this two periods

# Let's implement the gradual change using regression model
# create ramp for gradual change
ozone2$Ramp = c(rep(0,132),1:84)
ozone2 %>% filter(Year >= 1965 & Year < 1967)
ozone2 %>% tail
# now capture Ramp using regression model
M2 = lm(ozone ~ Ramp, data=ozone2)
summary(M2)
# since in lm output p-values are two sided...
# p-value = 1.6e-08/2 = 0.000000008
# Create a graph to show how regression fits the data
ozone2$M2 = M2$fitted.values
ozone2 %>% ggplot(mapping=aes(x=Year,y=ozone)) + geom_point() + 
  geom_vline(xintercept=1966, color="red", size=0.2) +
  geom_point(mapping=aes(x=Year, y=M2, col=as.factor(Dummy)))+
  theme_bw()

ozone2b$Ramp = c(rep(0,72),1:84)
ozone2b %>% filter(Year >= 1965 & Year < 1967)
# now capture Ramp using regression model
M2 = lm(ozone ~ Ramp, data=ozone2b)
summary(M2)
# since in lm output p-values are two sided...
# p-value = 1.6e-08/2 = 0.000000008
# Create a graph to show how regression fits the data
ozone2b$M2 = M2$fitted.values
ozone2 %>% ggplot(mapping=aes(x=Year,y=ozone)) + geom_point() + 
  geom_vline(xintercept=1966, color="red", size=0.2) +
  geom_point(mapping=aes(x=Year, y=M2, col=as.factor(Dummy)))+
  theme_bw()
#We see no relevant differences in using all the previous available data
#or only the interval 1960-66 after the first interventions took place


M3 = lm(ozone ~ Dummy + Ramp, data=ozone2)
summary(M3)
# Create a graph to show how regression fits the data
ozone2$M3 = M3$fitted.values
ozone2 %>% ggplot(mapping=aes(x=Year,y=ozone)) + geom_point() + 
  geom_vline(xintercept=1966, color="red", size=0.2) +
  geom_point(mapping=aes(x=Year,y=M1),col="green")+
  theme_bw() +geom_point(mapping=aes(x=Year,y=M2),col="magenta")+
  geom_point(mapping=aes(x=Year,y=M3),col="blue")


#5 Can you capture both interventions, the opening of the Golden State Freeway 
#and the implementation of Rule 63  in 1960, and the regulations for new car 
#engines implemented in 1966 in one model? Can you model both interventions 
#and both abrupt and gradual changes? 
# Let's implement the abrupt change using regression model
# We create a dummy variable 0/1 and call it 'dummy'
ozone$Dummy1 = 0
ozone$Dummy1[61:216] = 1
ozone$Dummy2 = 0
ozone$Dummy2[133:216] = 1
ozone %>% tail
M1 = lm(ozone ~ Dummy1 + Dummy2, data=ozone)
summary(M1)
# since in lm output p-values are two sided...
# p-value for Dummy0 = 3.85e-9/2 = 0.00000000001725
# p-value for Dummy = 0.0488/2 = 0.0244

# Create a graph to show how regression fits the data
ozone$M1 = M1$fitted.values
ozone %>% ggplot(mapping=aes(x=Year,y=ozone)) + geom_point() + 
  geom_vline(xintercept=1960, color="red", size=0.2) +
  geom_vline(xintercept=1966, color="red", size=0.2) +
  geom_point(mapping=aes(x=Year, y=M1, col=as.factor(Dummy1)))+
  geom_point(mapping=aes(x=Year, y=M1, col=as.factor(Dummy2)))+
  theme_bw()

# Let's implement the gradual change using regression model
# create ramp for gradual change
ozone$Ramp1 = c(rep(0,60),1:156)
ozone$Ramp2 = c(rep(0,132),1:84)
ozone
# now capture Ramp using regression model
M2 = lm(ozone ~ Ramp1 + Ramp2, data=ozone)
summary(M2)
# since in lm output p-values are two sided...
# p-value for Ramp0 = 0.0001/2 = 0.00005
# p-value for Ramp = 0.466/2 = 0.233
# Create a graph to show how regression fits the data
ozone$M2 = M2$fitted.values
ozone %>% ggplot(mapping=aes(x=Year,y=ozone)) + geom_point() + 
  geom_vline(xintercept=1966, color="red", size=0.2) +
  geom_vline(xintercept=1960, color="red", size=0.2) +
  theme_bw() + geom_point(mapping=aes(x=Year,y=M2), col="magenta")


#M3 = lm(ozone ~ Dummy1 + Dummy2 + Ramp1 + Ramp2, data=ozone)
M3 = lm(ozone ~ Dummy1 + Ramp1 + Dummy2 + Ramp2, data=ozone)
summary(M3)
# Create a graph to show how regression fits the data
ozone$M3 = M3$fitted.values
ozone %>% ggplot(mapping=aes(x=Year,y=ozone)) + geom_point() + 
  geom_vline(xintercept=1966, color="red", size=0.2) +
  geom_vline(xintercept=1960, color="red", size=0.2) +
  theme_bw() +
  geom_point(mapping=aes(x=Year,y=M3),col="blue")




