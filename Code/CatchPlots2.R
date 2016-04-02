## this script will create all the subsetted data .csv's for Matlab for bivariate 'Capelin' & 'Sandlance' and Univariate analyses

## libraries
library(ggplot2)
library(reshape2)
library(dplyr)

## Source functions 
source("c:/Users/rystanley/Documents/GitHub/Groundfish/Code/CatchPlots.R")
source("c:/Users/rystanley/OneDrive/PhD/R/Functions/recode.R")

## Get data for analysis
dfodata <-  read.csv("Data/TrawlSurveyData_2012.csv")
CoreData <- names(dfodata)[1:13]
years <- 1995:2012

#Subset out the species of interest

#shrimp is the sum of shrimp
dfodata$Shrimp=rowSums(dfodata[,c("PANDALUS..SP_","PANDALUS.BOREALIS",
                            "PANDALUS.MONTAGUI","PANDALUS.PROPINQUUS")],na.rm = TRUE)
data <- dfodata[,c(CoreData,"GADUS.MORHUA","Shrimp","CHIONOECETES.OPILIO")]
colnames(data)[c((length(data)-2),length(data))]=c("Cod","Crab") # fix column names.

pdata <- melt(data[,c("surveyyear","DIV","Temp_at_fishing","Cod","Shrimp","Crab")],
                   id=c("surveyyear","DIV","Temp_at_fishing"))

pdata2 <- as.data.frame(pdata%>%group_by(surveyyear,variable)%>%
                          summarise(meancatch=mean(value,na.rm=T),
                                    sdcatch=sd(value,na.rm=T),
                                    secatch=sd(value,na.rm=T)/sqrt(sum(!is.na(value))),
                                    meantemp=mean(Temp_at_fishing,na.rm=T),
                                    sdtemp=sd(Temp_at_fishing,na.rm=T),
                                    setemp=sd(Temp_at_fishing,na.rm=T)/sqrt(sum(!is.na(Temp_at_fishing))))%>%
                          ungroup())

pdata3 <- as.data.frame(pdata%>%group_by(surveyyear,variable,DIV)%>%
                          summarise(meancatch=mean(value,na.rm=T),
                                    sdcatch=sd(value,na.rm=T),
                                    secatch=sd(value,na.rm=T)/sqrt(sum(!is.na(value))),
                                    meantemp=mean(Temp_at_fishing,na.rm=T),
                                    sdtemp=sd(Temp_at_fishing,na.rm=T),
                                    setemp=sd(Temp_at_fishing,na.rm=T)/sqrt(sum(!is.na(Temp_at_fishing))))%>%
                          ungroup())

pdata$variable=recoderFunc(pdata$variable,"Cod","Gadus morhua")
pdata$variable=recoderFunc(pdata$variable,"Crab","Chionoecetes opilio")
pdata$variable=recoderFunc(pdata$variable,"Shrimp","Pandalus sp")

pdata$variable=factor(pdata$variable,levels=c("Gadus morhua","Chionoecetes opilio","Pandalus sp"))

p1=ggplot(pdata,aes(x=factor(surveyyear),y=value))+
  scale_y_log10()+
  annotation_logticks(sides="l")+
  geom_boxplot()+
  facet_grid(variable~.,scales="free")+
  labs(x="Survey year",y="Catch per trawl")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"));p1

p2=ggplot(pdata,aes(x=factor(surveyyear),y=value))+
  scale_y_log10()+
  annotation_logticks(sides="l")+
  geom_boxplot()+
  facet_grid(variable~DIV,scales="free_y")+
  labs(x="Survey year",y="Catch per trawl")+
  scale_x_discrete(limits=as.factor(1993:2014),
                     breaks=c("1995","1997","1999","2001","2003","2005","2007","2009","2011"),
                     labels=c("1995","1997","1999","2001","2003","2005","2007","2009","2011"))+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust = 1));p2

# Catch vs temperature 
#on anomolously low point for shrimp
pdata2[which(pdata2$variable=="Pandalus sp" & pdata2$meancatch<100),c("meancatch","sdcatch","secatch")]=NA

p3 <- ggplot(pdata2,aes(x=meantemp,y=meancatch,group=variable))+
    stat_smooth(method="lm",se=FALSE,lty=2,lwd=0.75,col="black")+
    geom_errorbarh(aes(xmin=meantemp-setemp,xmax=meantemp+setemp),height=0)+
    geom_errorbar(aes(ymin=meancatch-secatch,ymax=meancatch+secatch),width=0)+
    geom_point()+
    theme_bw()+
    scale_y_log10(breaks=c(5,25,50,100,250,500,1000,5000,10000),
                  labels=as.character(c(5,25,50,100,250,500,1000,5000,10000)))+
    annotation_logticks(sides="l")+
    facet_grid(variable~.,scales="free")+
    labs(x=expression(paste("Temperature ",degree,"C "%+-%" SE",sep="")),
         y=expression(paste("Catch per trawl "%+-%" SE",sep="")))+
      theme(strip.background = element_rect(fill="white"));p3

pdata3[which(pdata3$variable=="Pandalus sp" & pdata3$meancatch<100),c("meancatch","sdcatch","secatch")]=NA

p4 <- ggplot(pdata3,aes(x=meantemp,y=meancatch,group=variable))+
  stat_smooth(method="lm",se=TRUE,lty=2,lwd=0.75,col="black")+
  geom_errorbarh(aes(xmin=meantemp-setemp,xmax=meantemp+setemp),height=0)+
  geom_errorbar(aes(ymin=meancatch-secatch,ymax=meancatch+secatch),width=0)+
  geom_point()+
  theme_bw()+
  scale_y_log10(breaks=c(5,50,100,250,1000,10000),
                labels=as.character(c(5,50,100,250,1000,10000)))+
  annotation_logticks(sides="l")+
  facet_grid(variable~DIV,scales="free_y")+
  labs(x=expression(paste("Temperature ",degree,"C "%+-%" SE",sep="")),
       y=expression(paste("Catch per trawl "%+-%" SE",sep="")))+
  theme(strip.background = element_rect(fill="white"));p4
  

ggplot(pdata,aes(x=Temp_at_fishing,y=value,group=variable))+
  geom_point()+
  stat_smooth(method="lm",lty=2,lwd=0.75,col="black")+
  facet_grid(variable~DIV,scales="free_y")+
  labs(x=expression(paste("Temperature at fishing (",degree,"C)",sep="")),
       y="Catch per trawl")+
  theme_bw()+
  scale_y_log10()+
  annotation_logticks()+
  theme(strip.background = element_rect(fill="white"))


  


filter(pdata,value>0)


ggplot(pdata2,aes(x=surveyyear,y=mean))+
  geom_errorbar(aes(ymin=mean-stdev,ymax=mean+stdev),width=0.2)+
  geom_point(size=2)+
  #scale_y_log10()+
  #annotation_logticks(sides="l")+
  facet_grid(variable~.,scales="free")+
  theme_bw()
