

library(ggplot2)

#Histogram
mtcars=mtcars
ggplot(mtcars,aes(mpg))+geom_histogram(binwidth =1,color="red",fill="green" )+ggtitle("Distribution of mpg")+
  ylab("number")+xlab("mileperGalon")+xlim(0,50)

#Scatterplot
ggplot(mtcars,aes(x=wt,y=qsec))+geom_point()+ggtitle("correlation of wt and qsec")

################################
DF<-data.frame(Pillars,Values) 
ggplot(DF,aes(Values))+geom_histogram(binwidth =1,color="red",fill="green" )+ggtitle("Distribution of Pillars")+
  ylab("number")+xlab("mileperGalon")+xlim(0,50)