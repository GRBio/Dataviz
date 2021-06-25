
#### Load packages 
library(dplyr)
library(survival)
library(data.table)
library(ggplot2)
library(questionr)  
library(networkD3)   
library(htmlwidgets)
library( RColorBrewer)

#### Read data 
load("datos.RData")

#################
### Rose diagram
#################
a <- table(d_patients$f_ingreso)
x<- a[1:length(a)]
dd <-as.data.frame(names(a)[10:34])
dd$value <- a[10:34]
dd$group <- "Total"
names(dd)[1] <-"date"
dd$date <- paste0(substr(dd$date,9,10),"-",substr(dd$date,6,7))

pat <- subset(d_patients, sexo=="FEMALE")
b <- table(pat$f_ingreso)
x<- b[1:length(b)]
dd2 <-as.data.frame(names(b)[7:31])
dd2$value <- b[7:31]
dd2$group <- "Dones"
names(dd2)[1] <-"date"
dd2$date <- paste0(substr(dd2$date,9,10),"-",substr(dd2$date,6,7))
dd3 <- rbind(dd,dd2)

#################
### A. March
#################
mypal<- brewer.pal(10,"BrBG")[c(4,2)]
mypal <- rep(brewer.pal(10,"BrBG")[c(4,2)],each=25)
cxc1 <- ggplot(dd3 ,aes(x = date, y=value, fill = grup)) +
  # do it as a stacked bar chart first
  geom_bar(width = 1, position="identity", stat="identity", color="black", fill=mypal) 
# set scale so area ~ Deaths	
#scale_y_sqrt() 
# A coxcomb plot = bar chart + polar coordinates
windows(20,16)
cxc1 + coord_polar(start=4*pi/2) + scale_y_sqrt()+theme_bw()+
  ggtitle("March 2020") + 
  xlab("")

#################
### B. April
#################

a <- table(d_patients$f_ingreso)
x<- a[1:length(a)]
dd <-as.data.frame(names(a)[35:length(a)])
dd$value <- a[35:length(a)]
dd$group <- "Total"
names(dd)[1] <-"date"
dd$date <- paste0(substr(dd$date,9,10),"-",substr(dd$date,6,7))

pat <- subset(d_patients, sexo=="FEMALE")
b <- table(pat$f_ingreso)
x<- b[1:length(b)]
dd2 <-as.data.frame(names(b)[32:length(b)])
dd2$value <- b[32:length(b)]
dd2$group <- "Dones"
names(dd2)[1] <-"date"
dd2$date <- paste0(substr(dd2$date,9,10),"-",substr(dd2$date,6,7))

dd3 <- rbind(dd,dd2)
dd3$value[dd3$value==60 & dd3$date=="05-04"] <- 110
library(ggplot2)
library( RColorBrewer)
mypal <- rep(brewer.pal(10,"BrBG")[c(4,2)],each=20)
cxc1 <- ggplot(dd3 ,aes(x = date, y=value, fill = grup)) +
  geom_bar(width = 1, position="identity", stat="identity", color="black", fill=mypal) 

windows(20,16)
cxc1 + coord_polar(start=4*pi/2) + scale_y_sqrt()+ theme_bw()+
  ggtitle("April 2020") + 
  xlab("")