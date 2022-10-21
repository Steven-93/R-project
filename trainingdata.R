df <- data.frame(
  var1 = c("mazda", "porsche", "ferrari"),
  var2 = c(12, 100, 46)
)
head(df)
barplot<-(df)
barplot
library(ggplot2)
# Barplot
bp<- ggplot(df, aes(x="var1", y="var2", fill=legend))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie

# Use custom color palettes
pie + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

pie + scale_fill_brewer(palette="Dark2")


Master<-c("pillarI","pillarII","pillarIII","budget other" )
PillarI<-c("PI","NA")
rv <- c("PI", "NA",)

# convert vector into factor
rf <- factor(rv)

# convert factor into numeric value
x <- as.numeric(rf)

# print the numeric value
x

# check if this is a numeric value
is.numeric(x)

Master<-Master.csv

data1<-data
data1[data1=="A"]<-"XXX"

master1<-master
master1[master1=="PI","PII","PIII"]<-
  "1"


master$budget_pillar_I(PI)


Master$Pillar<- names(c("budget_pillarI","Budget_pillarII","budget_pillarIII","budget_other")
                      [apply(c("budget_pillarI","Budget_pillarII","budget_pillarIII","budget_other") == 1, 1, which)]
                      Master(c("budget_pillarI","Budget_pillarII","budget_pillarIII","budget_other")
                             [apply(c("budget_pillarI","Budget_pillarII","budget_pillarIII","budget_other"))]
                             
                             df$column[which(df$colum == "variable name")] <- new name
                             master$budget_pillar_I[which(master$budget_pillar_I=="PI")]<-1
                             master$budget_pillar_II[which(master$budget_pillar_II=="PII")]<-1
                             master$budget_pillar_III[which(master$budget_pillar_III=="PIII")]<-1
                             master$budget_other[which(master$budget_other=="other")]<-1
                             
                             df$colum[is.na(df$column)] <- 0
                             master$budget_pillar_I[is.na(master$budget_pillar_I)]<-0
                             master$budget_pillar_I[is.na(master$budget_pillar_I)]<-0
                             data2<-data
                             data2$x4<-as.character(data2$x4)
                             data2[data2=="f2"]<-"YYY"
                             data2$x4<-as.factor(data2$x4)
                             master$budget_pillar_I<-as.character(master$budget_pillar_I)
                             
                             
                             master$budget_pillar_I[master$budget_pillar_I=="NA"]<-0
                             master$budget_pillar_II[master$budget_pillar_II=="NA"]<-0
                             master$budget_pillar_III[master$budget_pillar_III=="NA"]<-0
                             master$budget_other[master$budget_other==""]<-0
                             master$budget_pillar_I[is.na(master$budget_pillar_I)]<-0
                             master$budget_pillar_II[is.na(master$budget_pillar_II)]<-0
                             master$budget_pillar_III[is.na(master$budget_pillar_III)]<-0
                             master$budget_other[is.na(master$budget_other)]<-0
                             
                             sum(master$budget_pillar_I)
                             sum(master$budget_pillar_I_NA, na.rm = TRUE)
                             str(master$budget_pillar_I)
                             sum(master$budget_pillar_I)
                             master$budget_pillar_I<-as.character(master$budget_pillar_I)
                             sum(master$budget_pillar_I)
                             master$budget_pillar_I<-as.numeric(master$budget_pillar_I)
                             sum(master$budget_pillar_I)
                             sum(is.na(master$budget_pillar_I))
                             sum(master$budget_pillar_I)
                             sum(master$budget_pillar_II) 
                             
                             
                             
#new Master file 
library (csv)
library(ggplot2)
Master<-read.csv(file = 'Master.csv', header = TRUE)

                
                             Master$budget_pillar_I[which(Master$budget_pillar_I=="PI")]<-1
                             Master$budget_pillar_II[which(Master$budget_pillar_II=="PII")]<-1
                             Master$budget_pillar_III[which(Master$budget_pillar_III=="PIII")]<-1
                             Master$budget_other[which(Master$budget_other=="other")]<-1
                             
                      
                             Master$budget_pillar_I<-as.integer(Master$budget_pillar_I)
                             Master$budget_pillar_II<-as.integer(Master$budget_pillar_II)
                             Master$budget_pillar_III<-as.integer(Master$budget_pillar_III)
                             Master$budget_other<-as.integer(Master$budget_other)
                             
                             
Pillars<-c("PillarI","PillarII","PillarIII","Pillar Other")                             
Values<-c(sum(Master$budget_pillar_I),sum(Master$budget_pillar_II),  
          sum(Master$budget_pillar_III),  
          sum(Master$budget_other) )                             
                                                          
                                 
DF<-data.frame(Pillars,Values) 





# Barplot
library(ggplot2)

bp<- ggplot(DF, aes(x=Pillars, y=Values,fill=Pillars ))+
  geom_bar(width = 2, stat = "identity")
bp

bd<- ggplot(DF, aes(x=Pillars, y=Values,fill=Pillars ))+
geom_dotplot(binwidth =20)
bd

position=stuck


pie <- ggplot(DF, aes(x="", y=Values,fill=Pillars ))+
geom_bar(width = 2, stat = "identity") + coord_polar("y", start=0)
pie


#exercise

library(echarts4r)
DF<-data.frame(Pillars,Values)

  DF |> 
  e_charts(Pillars) |> 
  e_pie(Values)|> 
 e_title("Pie chart")|>
  e_tooltip() # tooltip


############################################################
  
  
  
  Master<-read.csv(file = 'Master.csv', header = TRUE)
  
  
  Master$budget_pillar_I[which(Master$budget_pillar_I=="1")]<-1
  Master$budget_pillar_II[which(Master$budget_pillar_II=="2")]<-1
  Master$budget_pillar_III[which(Master$budget_pillar_III=="3")]<-1
  Master$budget_other[which(Master$budget_other=="other")]<-1
  
  Master$budget_pillar_I<-as.integer(Master$budget_pillar_I)
  Master$budget_pillar_II<-as.integer(Master$budget_pillar_II)
  Master$budget_pillar_III<-as.integer(Master$budget_pillar_III)
  Master$budget_other<-as.integer(Master$budget_other)
  
  ###################################################################
  
  
  
  
  Master$budget_pillar_I[is.na(Master$budget_pillar_I)]<-0
  Master$budget_pillar_II[is.na(Master$budget_pillar_II)]<-0
  Master$budget_pillar_III[is.na(Master$budget_pillar_III)]<-0
  Master$budget_other[is.na(Master$budget_other)]<-0
  
  
  ##################################################
  
  Trial<-read.csv(file = "Trial.csv", header = TRUE)
  
  
  Trial$budget_pillar_I[which(Trial$budget_pillar_I=="1")]<-1
  Trial$budget_pillar_II[which(Trial$budget_pillar_II=="2")]<-1
  Trial$budget_pillar_III[which(Trial$budget_pillar_III=="3")]<-1
  Trial$budget_other[which(Trial$budget_other=="other")]<-1
  Trial$more_pillars[which(Master$more_pillars=="1")]<-1
  
  
  
  Trial$budget_pillar_I[is.na(Trial$budget_pillar_I)]<-0
  Trial$budget_pillar_II[is.na(Trial$budget_pillar_II)]<-0
  Trial$budget_pillar_III[is.na(Trial$budget_pillar_III)]<-0
  Trial$budget_other[is.na(Trial$budget_other)]<-0
  Trial$more_pillars[is.na(Trial$more_pillars)]<-0
  
  Trial$budget_other[Trial$budget_other==""]<-0
  
  Trial$budget_other<-as.integer(Trial$budget_other)
  
  
  Pillars<-c("PillarI","PillarII","PillarIII","Pillar Other","More Pillars")                             
  Values<-c(sum(Trial$budget_pillar_I),sum(Trial$budget_pillar_II),  
            sum(Trial$budget_pillar_III),  
            sum(Trial$budget_other),sum(Trial$more_pillars) )  
  
  sum(Trial$budget_other)
  
  Trial$budget_other<-as.integer(Trial$budget_other)
  
  GG<-data.frame(Pillars,Values) 
  
  
  
  
  library(echarts4r)
  GG<-data.frame(Pillars,Values)
  
  GG |> 
    e_charts(Pillars) |> 
    e_pie(Values)|> 
    e_title("Pie chart")|>
    e_tooltip() # tooltip
  
  
  
  GG |> 
    e_charts(Pillars) |> 
    e_scatter(Values) |> 
    e_visual_map(Values, scale = e_scale) |> # scale color
   
    
########################################################################
 
   
GG   = [{
        coordinateSystem: 'polar',
        name: this.legends[1],
        type: 'line',
        color: '#D9B100',
        data: this.datasetCylinder.data,
        smooth: true,
        showSymbol: false,
        lineStyle: {
          show: true,
          width: 3,
          shadowBlur: 10,
          shadowColor: 'gold'
        },
        areaStyle: {
          color: 'gold',
          origin: 'end',
          opacity: 0.1
        }
      }]
    
##########################################################################
    
    GG |> 
      e_charts(Values) |> 
      e_candle(GS.Open, GS.Close, GS.Low, GS.High, name = "Goldman Sachs") |> 
      e_datazoom(type = "slider") |> 
      e_title("Candlestick chart", "Quantmod data")

    
##########################################################################
    
    GG |> 
      e_charts() |> 
      e_funnel(Values,Pillars) |> 
      e_title("Funnel")

###########################################################################
    
    
    
    library(quantmod)
    
    getSymbols("GS") #Goldman Sachs
    GS <- as.data.frame(GS)
    GG$Values <- row.names(GS)    

    GS |> 
      e_charts(date) |> 
      e_candle(GS.Open, GS.Close, GS.Low, GS.High, name = "Goldman Sachs") |> 
      e_datazoom(type = "slider") |> 
      e_title("Candlestick chart", "Quantmod data")
    
    
    
    
####################################################################
    
    
    water <- data.frame(val = c(1, 2, 0.1))
    
    liquid |> 
      e_charts() |> 
      e_liquid(val) 
    
############################################################
    


      