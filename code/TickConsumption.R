## analysis of experimental data for invertebrate feeding trials to red-backed 
## salamanders, in simple and complex mesocosms
## The purpose of this script is to _____________ 
## Written by ehgrant
## Questions: ehgrant at usgs dot gov
#########################
#1. import data
#########################

# preliminaries #
# load libraries, read data, setup matrices for output, specify priors #
#load library#
library(dplyr)
#read data
prey<-read.csv(file = "../Data/tickFeedingRd1.csv",header=TRUE)

#########################
#2. create summaries 
#########################
#calculate number eaten each day
#('eaten')

#Calculate mean daily consumption by individual#
feedRate<-prey %>%
  group_by(sal,Habitat_Type) %>%
  summarise_at(vars(eaten), mean)

#Calculate overall average and SD by Rx#
RxMean<-feedRate %>%
  group_by(Habitat_Type) %>%
  summarise(mean=mean(eaten), sd=sd(eaten))
#########################
#3. plot
#########################



#########################
#4. test for difference
#########################

owdiff <- aov(eaten ~ Habitat_Type, data = feedRate)
summary(owdiff)
