

rm(list=ls())

library(dplyr)
library(ggplot2)
library(reshape2)
library(colorspace)
library(grid)
library(stringr)

#Change your working directory to wherever you've saved these files. Code ideas from Alan Roberts tutorial to SAU staff 
setwd("C:/Users/t.cashion/Google Drive/Desktop/R")


#Example 1
catchdata <- read.csv("SAU EEZ 925 v46-0.csv")
catchdata$year <- as.numeric(catchdata$year)
catchdata.agg <- aggregate(catchdata$tonnes, by =list(catchdata$area_name, catchdata$year, catchdata$common_name), FUN=sum)
colnames(catchdata.agg) <- c("area_name", "year", "common_name", "tonnes")
catch <- spread(catchdata.agg, common_name, tonnes , fill=0)

p <- ggplot(catch)
p <- p + geom_line(aes(catch$year, catch$`Pink salmon`), color='pink', alpha=0.5, size=1)
print(p)
p <- p + geom_line(aes(catch$year, catch$`Sockeye salmon`), color='red', alpha=0.5, size=3)
print(p)
p <- p + theme_bw()
print(p)
p <- p + xlab("Year")
print(p)
p <- p + ylab("Catch (tonnes)")
print(p)
p <- p + scale_x_continuous(breaks = seq(1950, 2014, by=10)) 
print(p)

#Other options:
p <- p + ggtitle("Catches of Pink and Sockeye Salmon in BC")
p <- p + theme(plot.title= element_text(hjust = 0.5, vjust=1, family = "sans", face="bold"))
p <- p + theme(strip.background = element_rect(fill="white", colour="white"))
p <- p + theme(axis.ticks = element_line(colour = "white"))
p <- p + theme(axis.text.y = element_text(size=6))
p <- p + theme(axis.text.x = element_text(size=6))
print(p)



#Example 2

tonnage <- read.csv("Tonnage_export.csv")
CategoryNames.tonnage <- unique(tonnage$gear_category)



#Figure 1 # Thanks to Angie Coulter for providing original version of this code which I adapted here for analysis of global fisheries by gear
#A)Stacked area plot of landings by gear category
#CategoryNames <- c("bottom trawl", "gillnet", "pelagic trawl", "purse seine", "longline", "other", "unknown", "small scale")
#CategoryNames <- rev(CategoryNames)

A1 <- ggplot()
#stacked area with reported line
A1 <- A1 + geom_area(data=tonnage[tonnage$catch_type_id==1,], aes(x=Year,y=Catch,group=Factor, fill=Factor))
A1 <- A1 + scale_fill_brewer(palette="Spectral",name='Gear category',labels=CategoryNames.tonnage) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background =  element_blank())
#label the y axis
A1 <- A1 + labs(y="Landings (million metric tons)")
A1 <- A1 + theme(axis.title.y= element_text(size = 8), axis.text.x = element_text(size = 8),axis.text.y = element_text(size = 8))
A1 <- A1 + theme(axis.title.x = element_blank())
#adjust y axis labels
#A1 <- A1 + scale_y_continuous(breaks=seq(0,6.5,1), limits = c(0,6.5))
#adjust x axis labels
#A1 <- A1 +  scale_x_continuous(breaks=seq(1950, 2014, 10))
A1 <- A1 + scale_y_continuous(expand = c(0,0),breaks=seq(0,110,10), limits = c(0,110)) + scale_x_continuous(expand = c(0,0))
#Make axes black
A1 <- A1 + theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))
#ignore 
#A1 <- A1 + theme(plot.margin = unit(c(0.5,1,0.5,0.5),"cm"))
A1 <- A1 + theme(legend.title=element_text(size=9),legend.text=element_text(size=8))
A1 <- A1 + guides(fill=guide_legend(ncol=2))
#colour the reported catch line black and name it
#A1 <- A1 + scale_color_manual(name='Reported Catch', labels = c("Landings"), values=c("black"))
A1

#B)Stacked area plot of discards by gear category
#CategoryNames <- c("bottom trawl", "gillnet", "pelagic trawl", "purse seine", "longline", "other", "unknown", "small scale")
B1 <- ggplot()
#stacked area with reported line
B1 <- B1 + geom_area(data=tonnage[tonnage$catch_type_id==2,], aes(x=Year,y=Catch,group=Factor, fill=Factor))
B1 <- B1 + scale_fill_brewer(palette="Spectral",name='Gear category',labels=CategoryNames.tonnage) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background =  element_blank())
#label the y axis
B1 <- B1 + labs(y="Discards (million metric tons)")
B1 <- B1 + theme(axis.title.y= element_text(size = 8), axis.text.x = element_text(size = 8),axis.text.y = element_text(size = 8))
B1 <- B1 + theme(axis.title.x = element_blank())
#adjust y axis labels
#B1 <- B1 + scale_y_continuous(breaks=seq(0,6.5,1), limits = c(0,6.5))
#adjust x axis labels
#B1 <- B1 +  scale_x_continuous(breaks=seq(1950, 2014, 10))
B1 <- B1 + scale_y_continuous(expand = c(0,0),breaks=seq(0,15,5), limits = c(0,15)) + scale_x_continuous(expand = c(0,0))
#Make axes black
B1 <- B1 + theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))
#ignore 
#B1 <- B1 + theme(plot.margin = unit(c(0.5,1,0.5,0.5),"cm"))
B1 <- B1 + theme(legend.title=element_text(size=9),legend.text=element_text(size=8))
B1 <- B1 + guides(fill=guide_legend(ncol=2))
#colour the reported catch line black and name it
#B1 <- B1 + scale_color_manual(name='Reported Catch', labels = c("Landings"), values=c("black"))
B1


Fig1 <- plot_grid(A1, B1, labels = c("A", "B"),label_size = 8, ncol = 1, align = "hv",hjust=0)
Fig1
