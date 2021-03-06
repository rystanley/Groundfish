PC_UnivariateT_two<-function(x,path,years,nafo,Species,age,convert=24950.144,TempData,log_y=FALSE,Print.cor=FALSE,SaveIm=TRUE){
  
  #This function will create the plot for Potential contact using the fall survey data
    #Note that this will only produce the second plot of the PC_UniveraiateT.R two plot code
  #x = the directory holding each matlab output
  #path = is the folder you want the files. Defulats to NULL
  #years = is the years of data available. Note that missing years must be accounted for. 
  #nafo = is the nafo divisions for the species/stock of interest
  #age= is the age of the focal fish data (1, 2, 3, 4 or a)
  #convert= is the converison factor to convert the data into fish per m2 (From previous contract)
  #TempData = is the averaged temperature data subsetted to the survey of interest (averaged across years with assc. variability)
  #Print.cor = shold the plots include the correlation (cor) and signifance (p) directly on the plot
  #log_y= a TRUE-FALSE command which determines whether the plot has the y axis plotted on a log10 scale 
  #SaveIm = a TRUE-FALSE command which determines whether the plot is saved (TRUE) or it is printed (FALSE)
    
  #required packages
  require(scales)
  require(grid)
  require(ggplot2)
  
  #Set working directory to data source location (x)
  setwd(x)
  
  #Source permutational correlation function  
              corPerm3 <- function(x,y,nperm=999,tail=2){     
              # Pierre Legendre, October 2005 :
              # see http://adn.biol.umontreal.ca/~numericalecology/labo/fonctions_r/corPerm.R.zip for details
            
                x <- as.matrix(x)
                y <- as.matrix(y)
                n <- nrow(x)
                  
                if((tail != -1) & (tail != 1) & (tail != 2)) 
                {stop ("Incorrect value for parameter 'tail'")}
                if(tail == -1) temp <- cor.test(x, y, alternative = "less")
                if(tail ==  1) temp <- cor.test(x, y, alternative = "greater")
                if(tail ==  2) temp <- cor.test(x, y, alternative = "two.sided")
                r.ref <- temp$estimate
                t.ref <- temp$statistic
                
                nGT <- 1
                for(i in 1:nperm)
                {
                  y.perm  <-  sample(y,n)
                  temp.perm <- cor.test(x,y.perm)
                  t.perm <- temp.perm$statistic
                  if(tail == -1) if(t.perm <= t.ref) nGT <- nGT+1
                  if(tail ==  1) if(t.perm >= t.ref) nGT <- nGT+1
                  if(tail ==  2) if( abs(t.perm) >= abs(t.ref) ) nGT <- nGT+1
                }
                P <- nGT/(nperm+1)
                return(list(Correlation=r.ref, tStat=t.ref, No.perm=nperm, P.perm=P, rUp=temp$conf.int[2],rLow=temp$conf.int[1]))
              }


# radius in km
levs=c(10,20,50,60,100,150)

#Age File deliminators 
#Here we used the _age_ denominator in the output from matlab and will use this to loop the analysis
AgeDelims=c("_1_","_2_","_3_","_4_","_a_")
AgeNames=c("One","Two","Three","Four","Adult")

#Which age to use and which age to name the files with
AgeDelim=AgeDelims[grep(age,AgeDelims)]
AgeName=AgeNames[grep(age,AgeDelims)]

#Pre-allocate array for recording in loop (yearly grabs)
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
      ten=which(BiData$Radius_t==10)
      twenty=which(BiData$Radius_t==20)
      fifty=which(BiData$Radius_t==50)
      sixty=which(BiData$Radius_t==60)
      hund=which(BiData$Radius_t==100)
      hundfifty=which(BiData$Radius_t==150)
      
      PC=BiData[c(ten,twenty,fifty,sixty,hund,hundfifty),2]
       
      Yeartag=rep(years[i],6)
      
      #Combine all the variables together for the plotting matrix
      Combo=cbind(Yeartag,levs,PC,PC.se,PC.sd[,2])
      colnames(Combo)[1:5]=c("Yr","Radius","PC","PC.se","PC.sd")
      #Each loop (year) stack the data together  
      SummaryData=rbind(SummaryData,Combo)
      
      #remove variables not be used again            
      rm(PC.sd,Combo,PC,ten,twenty,fifty,sixty,hund,hundfifty,BiData,erData,PC.se,FileNames,Yeartag)
    }

SummaryData=as.data.frame(SummaryData)
SummaryData[,3:5]=SummaryData[,3:5]/(1000000*convert) # Set for millions of Prey species 24950.144 is a denominator needed for the code for fish per m2
SummaryData$Radius=as.factor(SummaryData$Radius) # make the radius a factor variable

SummaryData[is.na(SummaryData)] <- 0 # Since some times there is a NA produced beacuse there are no fish at that spatial associations (therefore /0) the NA values will be repalced with o's

BottomLim=SummaryData$PC-SummaryData$PC.sd
TopLim=SummaryData$PC+SummaryData$PC.sd

#Clean workspace
rm(rawdata,levs)

# row numbers for each radius factor
ten=which(SummaryData$Radius==10)
twenty=which(SummaryData$Radius==20)
fifty=which(SummaryData$Radius==50)
sixty=which(SummaryData$Radius==60)
hund=which(SummaryData$Radius==100)
hundfifty=which(SummaryData$Radius==150)
radii=c("ten","twenty","fifty","sixty","hund","hundfifty")


####### NOW PLOT #################

rads=cbind(ten,twenty,fifty,hund,hundfifty) # row id's for given radii
Ra=c(10,20,50,100,150) # A number value to be added for a the facet wrap

# this is essentially a custom melt function to create the dataframe needed by ggplot
Mdata=NULL
for (a in 1:5){
  hold=cbind(SummaryData$PC[rads[,a]],BottomLim[rads[,a]],TopLim[rads[,a]],SummaryData$Yr[rads[,a]],rep(Ra[a],length(BottomLim[rads[,a]])))
  Mdata=rbind(Mdata,hold)
  rm(hold)
}

Mdata=data.frame(Mdata)
colnames(Mdata)[1:5]=c("Value","lc","uc","Year","radius")
Mdata$radius=factor(Mdata$radius,levels=c("150","100","50","20","10")) # set the levels so it will plot it with 10 on the bottom and 150 at the top

TenData=subset(Mdata,radius==10,select=c("Year","Value","lc","uc"))
TenData$ID_ten="Potential contact 10km"
colnames(TenData)[5]="ID"

FiftyData=subset(Mdata,radius==50,select=c("Year","Value","lc","uc"))
FiftyData$ID_fifty="Potential contact 50km"
colnames(FiftyData)[5]="ID"

TempData=TempData[colnames(FiftyData)] #subset the required column names for the stack
TempData$ID="Mean temperature (�C)"

AllData=rbind(TenData,FiftyData,TempData) # Combine the data together
AllData$ID=factor(AllData$ID,levels=c(TenData$ID[1],FiftyData$ID[1],TempData$ID[1]))

# not lets plot the variance in temp vs. pc at two scales
TenData=subset(Mdata,radius==10,select=c("Year","Value","lc","uc"))
FiftyData=subset(Mdata,radius==50,select=c("Year","Value","lc","uc"))
HundData=subset(Mdata,radius==100,select=c("Year","Value","lc","uc"))

TempData=TempData[,-5]
colnames(TempData)[1:4]=c("Temp_Year","Temp_Mean","Temp_lc","Temp_uc")
TenData=cbind(TenData,TempData,rep("10",NROW(TempData)))
FiftyData=cbind(FiftyData,TempData,rep("50",NROW(TempData)))
HundData=cbind(HundData,TempData,rep("100",NROW(TempData)))
colnames(TenData)[9]="ID"
colnames(FiftyData)[9]="ID"
colnames(HundData)[9]="ID"

ComboData=rbind(TenData,FiftyData,HundData)
ComboData$ID=factor(ComboData$ID,levels=c("100","50","10"))

# statistical analysis for linear models R using Legendres permutational anova with a two tailed distributional test. 

test10=corPerm3(TempData$Temp_Mean,TenData$Value,10000,2)
test50=corPerm3(TempData$Temp_Mean,FiftyData$Value,10000,2)
test100=corPerm3(TempData$Temp_Mean,HundData$Value,10000,2)

# Construct a dataframe which has the coordates (x and y) facet variable (ID) and outputs from peruational correlation

# Set find the y coordinate near the top of the plot 
Mx=aggregate(ComboData$uc,by=list(ComboData$ID),FUN=max)
Mx=Mx[,-1]
Mx=as.numeric(as.character(Mx))
Mx=rev(Mx) # reverse so the values are in order with the facet panels in the plot

x1=rep(min(TempData$Temp_Mean)*1.25,3)
y1=Mx*0.97
ID=c("10","50","100")
P.value=c(paste("p=",round(test10$P.perm[[1]],3)," ","cor=",round(test10$Correlation[[1]],3),sep=""),
          paste("p=",round(test50$P.perm[[1]],3)," ","cor=",round(test50$Correlation[[1]],3),sep=""),
          paste("p=",round(test100$P.perm[[1]],3)," ","cor=",round(test100$Correlation[[1]],3),sep=""))
LabData=cbind(x1,y1,ID,P.value)
LabData=as.data.frame(LabData)
LabData$x1=as.numeric(as.character(LabData$x1))
LabData$y1=as.numeric(as.character(LabData$y1))
LabData$ID=factor(LabData$ID,levels=c("100","50","10"))

#Create a plot with the values of PC (radius(10,50,100 km) +/- SD and yearly average temp +/- SE with a linear smooth )
#The plot will contain the correlatoin and significance (permutational) if Print.cor=T

if (Print.cor){
p1=ggplot(ComboData,aes(x=Temp_Mean,y=Value))+
  facet_grid(ID~.,scales="free_y")+theme_bw()+geom_point()+geom_errorbarh(aes(xmax=Temp_uc,xmin=Temp_lc),width=0.1)+
  geom_errorbar(aes(ymax=uc,ymin=lc))+geom_smooth(method="lm",se=FALSE,size=0.5,colour="black",lty=2)+
  labs(y=paste("PC(t) millions of ",Species,"(� SD)"),x="Average temperature (� SE)")+
  geom_text(aes(x1,y1,label=P.value,group=NULL),data=LabData)} else
  
    {p1=ggplot(ComboData,aes(x=Temp_Mean,y=Value))+
     facet_grid(ID~.,scales="free_y")+theme_bw()+geom_point()+geom_errorbarh(aes(xmax=Temp_uc,xmin=Temp_lc),width=0.1)+
     geom_errorbar(aes(ymax=uc,ymin=lc))+geom_smooth(method="lm",se=FALSE,size=0.5,colour="black",lty=2)+
     labs(y=paste("PC(t) millions of ",Species,"(� SD)"),x="Average temperature (� SE)")}

    #Save or Print picture
    if(SaveIm){
    #Save picture
    png(filename = paste(path,"Univariate_Temperature_PCscales_",Speciesn,"_",AgeName,"yrs_NAFO_",nafo,".png",sep=""), 
        width = 2400, height = 2400, res = 300, bg="transparent")
    
    print(p1)
    grid.text(unit(0.95,"npc"),0.5,label = "calculation radius (km)", rot = 270,vjust=-2.1)
    
    dev.off()
    print(paste("PC(t)vs.Temp plot (10, 50, & 100 km) saved: ",path,"Univariate_Temperature_PCscales_",Speciesn,"_",AgeName,"yrs_NAFO_",nafo,".png",sep=""))
    } else print(p1)

} #End function
