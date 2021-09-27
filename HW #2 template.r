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

# Import libraries
library(tidyr)
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)

# setwd
# Import file
nenana1 = read.table('Nenana1.txt', header=T, sep='\t', check.names=FALSE)
head(nenana1)

# Data cleaning
nenana1$'Date & Time' = str_remove(string=nenana1$'Date & Time', pattern='at ')
nenana1 = nenana1 %>% mutate(IceBreak=str_c(Year, nenana1$'Date & Time', sep=' '))
nenana1 = nenana1 %>% mutate(January1=str_c(Year, 'January 1 00:00 AM', sep=' '))

glimpse(nenana1)
nenana1$IceBreak = as.POSIXlt(nenana1$IceBreak, format = '%Y %B %e %H:$M %p')

glimpse(nenana1)
nenana1$January1 = as.POSIXlt(nenana1$January1, format = '%Y %B %e %H:$M %p')

glimpse(nenana1)
head(nenana1) #i don't know why the glimpse thing return <NA> values...

# set KPI
nenana1$KPI = difftime(nenana1$IceBreak, nenana1$January1, units='days')
nenana1$KPI = as.numeric(nenana1$KPI)

head(nenana1)
glimpse(nenana1)

#1


#2


#3


#4


#5


#6


#7


#8


#9


#10


##################################################
#                    Case 2
##################################################

#1
ozone = read.table('dtwnLAozone.txt', skip=2, header=TRUE)
head(ozone)

ozone = ozone %>% as.matrix %>% melt %>% 
  rename(Year=Var1, MonthShort=Var2, ozone=value) %>% arrange(Year, MonthShort)
ozone %>% head
ozone %>% tail

#2
#Graph the data and describe the patterns you see in the data set.

ozone %>% ggplot(aes(x=Year,y=ozone))+
  geom_point()+ theme_bw() + geom_vline(xintercept=1960, color="red", size=0.2)
#In this first graph we can appreciate the 12 data points we have for each month
# of each year. We can clearly see that after 1960, when the intervention took
# place, no measure is above level 6 of ozone (unlike before 1960, which was 
# quite common). We can also see how the lowest measures get even lower after
# the intervention.

ozone %>% ggplot(aes(group=Year, x=Year,y=ozone, fill=Year)) + 
  geom_boxplot(outlier.colour = 'Green') + theme(legend.position = 'none') + 
  geom_vline(xintercept=1960, color="red", size=0.2)
#In this second graph, we can get a better sense of the medians and quartiles,
# and outliers by using boxplots for each year. We can clearly see how after 
# 1960 the boxplots are placed on lower ozone levels.

#3
#Does the data provide statistically significant evidence that the opening of 
#the Golden State Freeway and the implementation of Rule 63 in 1960 reduced the 
#pollution statistically significantly?

#Ha: Intervention was effective and reduced pollution
#Ho: Intervention was not effective and did not reduce pollution
#Tolerance level alpha = 0.05 = 5%
#Carry out 2 sample t-test to assess whether the intervention was effective
before1960 = ozone %>% filter(Year < 1960)
after1960 = ozone %>% filter(Year >= 1960)
t.test(x= before1960$ozone, y=after1960$ozone, alternative="greater")

#since p-value = 0.00000000001472 < alpha = 0.05
# the data provides statistically significant evidence 
# against Ho, hence the intervention is effective.

#Let's visualize the difference
ozone %>% ggplot(aes(group=Year, x=Year,y=ozone, fill=Year)) + 
  geom_boxplot(outlier.colour = 'Green') + theme(legend.position = 'none') + 
  geom_vline(xintercept=1960, color="red", size=0.2) +
  geom_hline(yintercept=mean(before1960$ozone), color="blue")+
  geom_hline(yintercept=mean(after1960$ozone), color="magenta")+
  theme_bw()

#Evaluate both the abrupt and the gradual changes
#Report and interpret the estimates of both the abrupt and the gradual changes in ozone.

# Let's implement the abrupt change using regression model
# We create a dummy variable 0/1 and call it 'dummy'
ozone$Dummy = 0
ozone$Dummy[61:216] = 1
ozone %>% head
M1 = lm(ozone ~ Dummy,data=ozone)
summary(M1)
# since in lm output p-values are two sided...
# p-value = 3.12e-14/2 = 0.0000000000000156

#Which one do you think it is more appropriate and meaningful?


#4


#5



