#uploading the necessary libraries

library(csv)
library(ggplot2)
library(echarts4r)
library(dplyr)



#########################################################
#uploading the data

Master<-read.csv(file = 'Master.csv', header = TRUE)


#########################################################
#trasforming the data
s
Master$sum<- recode(Master$sum,"other"="Other")


Pillars<-c("PillarI","PillarII","PillarIII","Budget_other","PillarI-III","PillarII-III","PillarI-II-III","Null") 
Values<-c(length(which(Master$sum==1)),length(which(Master$sum==2),length(which(Master$sum==3))