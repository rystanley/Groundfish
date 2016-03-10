## Libraries
library(dplyr)

## This code is to divide up the data into Cod and Prey items
dfodata <- read.csv("Data/TrawlSurveyData_2012.csv")

CoreData <- names(dfodata)[1:13]
years <- 1995:2012

#create a list of different subsets of the NAFO divisons
div <- list()
div[[1]] <- c("2J","3K","3L")
div[[2]] <- c("3N","3O")
div[[3]] <- c("2J","3K","3L","3N","30")


##Cod Shrimp Data ------------------
CodShrimp <- dfodata[,c(CoreData,"GADUS.MORHUA")]
colnames(CodShrimp)[length(CodShrimp)]="Cod"

CodShrimp$Shrimp=rowSums(dfodata[,c("PANDALUS..SP_","PANDALUS.BOREALIS",
                                  "PANDALUS.MONTAGUI","PANDALUS.PROPINQUUS")],na.rm = TRUE)

CodShrimp <- filter(CodShrimp,Strat_Type == "Core")

CodShrimp[is.na(CodShrimp)]=0 #convert the NAs to 0

for (d in 1:3){
  for (i in years){
    temp=filter(CodShrimp,YEAR==i,DIV%in%div[[d]])%>%select(.,one_of(c("LAT_DEC","LONG_DEC","Shrimp","Cod")))
    Name=paste(div[[d]],collapse="")
    if(sum(temp$Cod,na.rm=T)>0 & sum(temp$Shrimp,na.rm=T)>0)
      { # make sure there is data first
        if(d==1){write.table(temp,file=paste0("Data/Shrimp/2J3KL/2J3KL__Cod_Shr_",i,".dat"),col.names = FALSE,row.names = FALSE)} #matlab format with equal number of file characters
        if(d==2){write.table(temp,file=paste0("Data/Shrimp/3NO/3NO_____Cod_Shr_",i,".dat"),col.names = FALSE,row.names = FALSE)}
        if(d==3){write.table(temp,file=paste0("Data/Shrimp/2J3KLNO/2J3KLNO_Cod_Shr_",i,".dat"),col.names = FALSE,row.names = FALSE)}
      }
    }
  }


##Cod Crab Data ------------------
CodCrab <- dfodata[,c(CoreData,"GADUS.MORHUA","CHIONOECETES.OPILIO")]
colnames(CodCrab)[c(length(CodCrab)-1,length(CodCrab))]=c("Cod","Crab")
CodCrab <- filter(CodCrab,Strat_Type == "Core")

CodCrab[is.na(CodCrab)]=0 #convert the NAs to 0

for (d in 1:3){
  for (i in years){
    temp=filter(CodCrab,YEAR==i,DIV%in%div[[d]])%>%select(.,one_of(c("LAT_DEC","LONG_DEC","Crab","Cod")))
    Name=paste(div[[d]],collapse="")
    if(sum(temp$Cod,na.rm=T)>0 & sum(temp$Crab,na.rm=T))
      {
        if(d==1){write.table(temp,file=paste0("Data/Crab/2J3KL/2J3KL___Cod_Crb_",i,".dat"),col.names = FALSE,row.names = FALSE)} #matlab format with equal number of file characters
        if(d==2){write.table(temp,file=paste0("Data/Crab/3NO/3NO_____Cod_Crb_",i,".dat"),col.names = FALSE,row.names = FALSE)}
        if(d==3){write.table(temp,file=paste0("Data/Crab/2J3KLNO/2J3KLNO_Cod_Crb_",i,".dat"),col.names = FALSE,row.names = FALSE)}
      }
    }
  }


#Univariate Data -------------------
## Univariate Cod Data Function requires a 4 column input which the 4th is ignored. So I just create a replicate
#  of the species of interest

#
CodUni <- dfodata[,c(CoreData,"GADUS.MORHUA","GADUS.MORHUA")]
colnames(CodUni)[c(length(CodUni)-1,length(CodUni))] = c("Cod","Cod1")

CrabUni <- dfodata[,c(CoreData,"CHIONOECETES.OPILIO","CHIONOECETES.OPILIO")]
colnames(CrabUni)[c(length(CrabUni)-1,length(CrabUni))] = c("Crab","Crab1")

ShrimpUni <- dfodata[,CoreData]
ShrimpUni$Shrimp=rowSums(dfodata[,c("PANDALUS..SP_","PANDALUS.BOREALIS",
                                    "PANDALUS.MONTAGUI","PANDALUS.PROPINQUUS")],na.rm = TRUE)
ShrimpUni$Shrimp1=ShrimpUni$Shrimp


## Cod Data Generate
for (d in 1:3){
  for (i in years){
    temp=filter(CodUni,YEAR==i,DIV%in%div[[d]])%>%select(.,one_of(c("LAT_DEC","LONG_DEC","Cod","Cod1")))
    Name=paste(div[[d]],collapse="")
    if(sum(temp$Cod,na.rm=T)>0)
    {
      if(d==1){write.table(temp,file=paste0("Data/Univariate/Cod/2J3KL/2J3KL___Cod_",i,".dat"),col.names = FALSE,row.names = FALSE)} #matlab format with equal number of file characters
      if(d==2){write.table(temp,file=paste0("Data/Univariate/Cod/3NO/3NO_____Cod_",i,".dat"),col.names = FALSE,row.names = FALSE)}
      if(d==3){write.table(temp,file=paste0("Data/Univariate/Cod/2J3KLNO/2J3KLNO_Cod_",i,".dat"),col.names = FALSE,row.names = FALSE)}
    }
  }
}

## Crab Data Generate
for (d in 1:3){
  for (i in years){
    temp=filter(CrabUni,YEAR==i,DIV%in%div[[d]])%>%select(.,one_of(c("LAT_DEC","LONG_DEC","Crab","Crab1")))
    Name=paste(div[[d]],collapse="")
    if(sum(temp$Crab,na.rm=T)>0)
    {
      if(d==1){write.table(temp,file=paste0("Data/Univariate/Crab/2J3KL/2J3KL___Crab_",i,".dat"),col.names = FALSE,row.names = FALSE)} #matlab format with equal number of file characters
      if(d==2){write.table(temp,file=paste0("Data/Univariate/Crab/3NO/3NO_____Crab_",i,".dat"),col.names = FALSE,row.names = FALSE)}
      if(d==3){write.table(temp,file=paste0("Data/Univariate/Crab/2J3KLNO/2J3KLNO_Crab_",i,".dat"),col.names = FALSE,row.names = FALSE)}
    }
  }
}

## Shrimp Data Generate
for (d in 1:3){
  for (i in years){
    temp=filter(ShrimpUni,YEAR==i,DIV%in%div[[d]])%>%select(.,one_of(c("LAT_DEC","LONG_DEC","Shrimp","Shrimp1")))
    Name=paste(div[[d]],collapse="")
    if(sum(temp$Shrimp,na.rm=T)>0)
    {
      if(d==1){write.table(temp,file=paste0("Data/Univariate/Shrimp/2J3KL/2J3KL___Shrimp_",i,".dat"),col.names = FALSE,row.names = FALSE)} #matlab format with equal number of file characters
      if(d==2){write.table(temp,file=paste0("Data/Univariate/Shrimp/3NO/3NO_____Shrimp_",i,".dat"),col.names = FALSE,row.names = FALSE)}
      if(d==3){write.table(temp,file=paste0("Data/Univariate/Shrimp/2J3KLNO/2J3KLNO_Shrimp_",i,".dat"),col.names = FALSE,row.names = FALSE)}
    }
  }
}