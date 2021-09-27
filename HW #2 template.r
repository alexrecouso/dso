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

# setwd
# Import file
nenana1 = read.table('Nenana1.txt', header=T, sep='\t', check.names=FALSE)
head(nenana1)

# Data cleaning
nenana1$'Date & Time' = str_remove(string=nenana1$'Date & Time', pattern='at ')
nenana1 = nenana1 %>% mutate(IceBreak=str_c(Year, nenana1$'Date & Time', sep=' '))
nenana1 = nenana1 %>% mutate(January1=str_c(Year, 'January 1 00:00 AM', sep=' '))
nenana1$IceBreak = str(nenana1$IceBreak)
head(nenana1)
nenana1$IceBreak = as.POSIXlt(nenana1$IceBreak, format = '%Y %B %e %H:$M %p')
glimpse(nenana1)
nenana1$January1 = as.POSIXlt(nenana1$January1, format = '%Y %B %e %H:$M %p')
glimpse(nenana1)
head(nenana1)

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


#2


#3


#4


#5



