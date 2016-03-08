## Libraries
library(dplyr)

## This code is to divide up the data into Cod and Prey items
dfodata <- read.csv("c:/Users/rystanley/OneDrive/SPERA/TrawlSurveyData_2012.csv")

CoreData <- names(dfodata)[1:13]

##Cod Shrimp
CodShrimp <- dfodata[,c(CoreData,"GADUS.MORHUA")]
colnames(CodShrimp)[length(CodShrimp)]="Cod"

CodShrimp$Shrimp=rowSums(dfodata[,c("PANDALUS..SP_","PANDALUS.BOREALIS",
                                  "PANDALUS.MONTAGUI","PANDALUS.PROPINQUUS")],na.rm = TRUE)

CodShrimp <- filter(CodShrimp,Strat_Type == "Core")

##Cod Crab
CodCrab <- dfodata[,c(CoreData,"GADUS.MORHUA","CHIONOECETES.OPILIO")]
colnames(CodCrab)[c(length(CodCrab)-1,length(CodCrab))]=c("Cod","Crab")

