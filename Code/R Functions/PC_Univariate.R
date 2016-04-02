PC_Univariate<-function(x,path=NULL,years,nafo,Species,age,convert=24950.144,log_y=FALSE,SaveIm=TRUE){
  
#This function will create the plot for Potential contact using the fall survey data

  #x = the directory holding each matlab output
  #path = is the folder you want the files. Defulats to NULL
  #years = is the years of data available. Note that missing years must be accounted for. 
  #nafo = is the nafo divisions for the species/stock of interest
  #age= is the age of the focal fish data (1, 2, 3, 4 or a)
  #convert= is the converison factor to convert the data into fish per m2 (From previous contract)
  #log_y= a TRUE-FALSE command which determines whether the plot has the y axis plotted on a log10 scale 
  #SaveIm = a TRUE-FALSE command which determines whether the plot is saved (TRUE) or it is printed (FALSE)

#Set working directory to source data files location
setwd(x)

#load required packages
require(scales)
require(grid)
require(ggplot2)

# radius in km
levs=c(5,10,20,35,40,60)

#Age File deliminators 
#Here we used the _age_ denominator in the output from matlab and will use this to loop the analysis
AgeDelims=c("_1_","_2_","_3_","_4_","_a_")
AgeNames=c("One","Two","Three","Four","Adult")

#Which age to use and which age to name the files with
AgeDelim=AgeDelims[grep(age,AgeDelims)]
AgeName=AgeNames[grep(age,AgeDelims)]

#Pre-allocate array for recording in loop
SummaryData=NULL

          for (i in 1:NROW(years)){
              FileNames=list.files(pattern=as.character(years[i])) # Identify all data froma  given year
              rawdata=list.files(pattern='csv') # Find all the data files with .csv tags which are not used in this analysis
              FileNames=FileNames[!FileNames %in% rawdata] # Remove the .csv file from the file list so that the proper data will always be assinged 
              
              fn=FileNames[grep(AgeDelim,x = FileNames)] # Subset for the right age
                  
              BiData=read.csv(fn[2],sep=",",header=FALSE,encoding="native.enc")
              colnames(BiData)[1:9]=c("Radius_t","PC_t","L_CL","U_CL","Rand_PC_t","p","XC_t","L95_CL","U95_CL")
              erData=read.csv(fn[1],sep=",",header=FALSE,encoding="native.enc")
              colnames(erData)[1:3]=c("Radius_t","PC_t","XC_t")
              # turn the radius increment into a factor
              erData$Radius_t=as.factor(erData$Radius_t)
              
              #Some data  is an NaN  - if during a randomization there was no data (all zeros) or
              #there are no fish captured in a given age class, division or species
              #If there is no data at all (all NaNs) then this code will assign zeros but
              #if there is some data and some Nans then it will just delete the NaNs
              if(sum(is.nan(erData$PC_t))==NROW(erData)){erData[,2:length(erData)]=0}
              if(sum(is.nan(BiData$PC_t))==NROW(BiData)){BiData[,2:length(BiData)]=0}
              
              erData=erData[complete.cases(erData),] # some bootstraps have NAN as a results probably there were no stations at the at a specified distance in that bootstrap
              
                       
                      ### now summarize the data to get the standard error from the Bootstrapped distribution for Potential Contact PC_t
                      PC.sd=aggregate(erData$PC_t,by=list(erData$Radius_t),FUN=sd)
                      # Coerse the Standard error
                      PC.se=1:NROW(PC.sd)
                      for (l in 1:NROW(PC.sd)){
                        PC.se[l]=PC.sd[l,2]/sqrt((NROW(erData)/6)) # divide by six (number of radius intervals) for the station number but in this case I think the amount of calculations is appropraite
                      #NROW erData=6000 (1000 replicates for each radius)
                        }
                                                    
              ## Convert the distance in meters to km
              BiData$Radius_t=BiData$Radius_t/1000    
          
              ## get the row numbers corresponding to the radius values so I can extract the XC_t and the PC_t for each each year at a give radius (t)
              five=which(BiData$Radius_t==5)
              ten=which(BiData$Radius_t==10)
              twenty=which(BiData$Radius_t==20)
              thirtyfive=which(BiData$Radius_t==35)
              forty=which(BiData$Radius_t==40)
              sixty=which(BiData$Radius_t==60)
          
              PC=BiData[c(five,ten,twenty,thirtyfive,forty,sixty),2]   
              Yeartag=rep(years[i],6)
              
              #Combine all the variables together for the plotting matrix
              Combo=cbind(Yeartag,levs,PC,PC.se,PC.sd[,2])
              colnames(Combo)[1:5]=c("Yr","Radius","PC","PC.se","PC.sd")
              SummaryData=rbind(SummaryData,Combo)#Each loop (year) stack the data together  
              
              #remove variables not be used again            
              rm(PC.sd,Combo,PC,five,ten,twenty,thirtyfive,forty,sixty,BiData,erData,PC.se,FileNames,Yeartag)
          } # end of year loop

SummaryData=as.data.frame(SummaryData)
SummaryData[,3:5]=SummaryData[,3:5]/(1000000*convert) # Set for millions of Prey species 24950.144 is a denominator needed for the code for fish per m2
SummaryData$Radius=as.factor(SummaryData$Radius) # make the radius a factor variable

SummaryData[is.na(SummaryData)] <- 0 # Since some times there is a NA produced beacuse there are no fish at that spatial associations (therefore /0) the NA values will be repalced with o's

BottomLim=SummaryData$PC-SummaryData$PC.sd
TopLim=SummaryData$PC+SummaryData$PC.sd

SummaryData$Radius=as.numeric(as.character(SummaryData$Radius))

      # row numbers for each radius factor
    five=which(SummaryData$Radius==5)
    ten=which(SummaryData$Radius==10)
    twenty=which(SummaryData$Radius==20)
    forty=which(SummaryData$Radius==40)
    sixty=which(SummaryData$Radius==60)
    radii=c("five","ten","twenty","forty","sixty")

      ####### NOW PLOT #################
      
      rads=cbind(five,ten,twenty,forty,sixty) # row id's for given radii
      Ra=c(5,10,20,40,60) # A number value to be added for a the facet wrap
      
      # this is essentially a custom melt function to create the dataframe needed by ggplot
      Mdata=NULL
      for (a in 1:5){
        hold=cbind(SummaryData$PC[rads[,a]],BottomLim[rads[,a]],TopLim[rads[,a]],SummaryData$Yr[rads[,a]],rep(Ra[a],length(BottomLim[rads[,a]])))
        Mdata=rbind(Mdata,hold)
        rm(hold)
      }
      
      Mdata=data.frame(Mdata)
      colnames(Mdata)[1:5]=c("PC","lc","uc","Year","radius")
      Mdata$radius=factor(Mdata$radius,levels=c("60","40","20","10","5")) # set the levels so it will plot it with 10 on the bottom and 150 at the top
      
      #YearRange - this is the range of possible years and will create a blank spot in the plot if data is missing
      YearRange=min(years):max(years)

      if (log_y){ p=ggplot(Mdata,aes(x=Year,y=PC))+geom_point()+geom_errorbar(aes(ymin=lc,ymax=uc,width=0.2))+geom_line()
                  p=p+facet_grid(radius~.,scales="free_y")+theme_bw()+scale_x_continuous(breaks=YearRange)+scale_y_log10()+
                    theme(axis.text.x=element_text(angle=90))+labs(y=paste("PC(t) millions of ",Species,"(± SD)"))} else {
                      p=ggplot(Mdata,aes(x=Year,y=PC))+geom_point()+geom_errorbar(aes(ymin=lc,ymax=uc,width=0.2))+geom_line()
                      p=p+facet_grid(radius~.,scales="free_y")+theme_bw()+scale_x_continuous(breaks=YearRange)+
                        theme(axis.text.x=element_text(angle=90))+labs(y=paste("PC(t) millions of ",Species,"(± SD)"))
                    } 
  
      Species=gsub(" ","",Species)#Remove any spaces in the common species name for the file name of the plot
      
#Save or print image
if(SaveIm){
      # save image
      png(filename = paste(path,"Univariate_PC_",Species,"_",AgeName,"yrs_NAFO_",nafo,"_facet.png",sep=""), 
          width = 2400, height = 2400, res = 300, bg="transparent")
      
          print(p)
          grid.text(unit(0.95,"npc"),0.5,label = "calculation radius (km)", rot = 270,vjust=-2.1,gp=gpar(fontsize=12))
      
      dev.off()

#Function results
print(paste("PC(T) [10,20,50,100,150km] ~ Year saved:",path,"Univariate_PC_",Species,"_",AgeName,"yrs_NAFO_",nafo,"_facet.png",sep=""))
} else print(p)

} # End of function
