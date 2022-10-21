install.packages("ggmap")
library(ggmap)
library(tidyverse)
library(dplyr) 
install.packages("readxl")
rm(mapData)
rm(Combined)
library("readxl", lib.loc=("C:/Users/dalsavio/Desktop/Training dashboard"))
mapData = read.csv("C:/Users/dalsavio/Desktop/Training dashboard")

Combined = WorldData[mapData$Country %in% mapData$Country, ]
Combined$value = mapData$Sales[match(Combined$region, mapData$Country)]

Countries = unique(Combined$region)
CDF = data.frame(label1=Countries)
for(i in CDF){
  Combined$value = ifelse(Combined$region %in% CDF$label1[i], (mapData$Sales), Combined$value)
}

ggplot(Combined, aes(x=long, y=lat, group = group, fill = value)) + 
  geom_polygon(colour = "white") +
  scale_fill_continuous(low = "blue",
                        high = "red",
                        guide="colorbar") +
  theme_bw()  +
  labs(fill = "Sales Amt" ,title = "International Furniture Part Sales", x="", y="") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())








