XC_Uni_Plot<-function(x,path=NULL,years,nafo,Species,age,convert=24950.144,SaveIm=TRUE){
  
#This function will create a Yearly multi-panel plot for Extra contact (XC(t)) for a given focal species directory

  #x = the directory holding each matlab output
  #path = is the folder you want the files. Defulats to NULL
  #years = is the years of data available. Note that missing years must be accounted for. 
  #nafo = is the nafo divisions for the species/stock of interest
  #age= is the age of the focal fish data (1, 2, 3, 4 or a)
  #convert= is the converison factor to convert the data into fish per m2 (From previous contract)
  #SaveIm = a TRUE-FALSE command which determines whether the plot is saved (TRUE) or it is printed (FALSE)
  
  #Set working directory where root data is stored. 
  require(ggplot2)
  require(scales)
  require(Hmisc)
  require(lattice)
 
  #Source peaks function
  fPeaks<- function(x, y, w=1, ...) {
    #this function will find peaks in raw data * note do not apply to smooth data or loess filter will fail
    #x and y must be specified and x usually is a row indicator (say 1:length(y))
    # 'w' is a w is the half-width of the window used to compute the local maximum. 
    #   (Its value should be substantially less than half the length of the array of data.) 
    #   Small values will pick up tiny local bumps whereas larger values will pass right over those. 
    #'span' is not specified in the function but is part of the loess filter and can be used to tweak the output
    #    'span' ranges between 0 and 1 and is a window as a proportion of the x values (row filters) larvaer values will smooth the data more aggressively
    #making local bumps dissapear 
    #
    # row indicies from the data can be obtained via output$i i.e. peaks=fpeaks(x=x,y=y,w=40,span=0.25) peaks via peaks$i
    # the smoothed value of y is available via peaks$y.hat
    
    # this code is adapted from http://stats.stackexchange.com/questions/36309/how-do-i-find-peaks-in-a-dataset
    
    require(zoo)
    n <- length(y)
    y.smooth <- loess(y ~ x, ...)$fitted
    y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
    delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
    i.max <- which(delta <= 0) + w
    list(x=x[i.max], i=i.max, y.hat=y.smooth)
  }
  
  # Set working directory to data path (x)
  setwd(x)

  #Age File deliminators 
  #Here we used the _age_ denominator in the output from matlab and will use this to loop the analysis
  AgeDelims=c("_1_","_2_","_3_","_4_","_a_")
  AgeNames=c("One","Two","Three","Four","Adult")
  
  #Which age to use and which age to name the files with
  AgeDelim=AgeDelims[grep(age,AgeDelims)]
  AgeName=AgeNames[grep(age,AgeDelims)]
  
  #Loop each year and grab the needed data
    GroupData=NULL      
      for (i in 1:NROW(years)){
        FileNames=list.files(pattern=as.character(years[i])) # Identify all data froma  given year
        rawdata=list.files(pattern='csv') # Find all the data files with .csv tags which are not used in this analysis
        FileNames=FileNames[!FileNames %in% rawdata] # Remove the .csv file from the file list so that the proper data will always be assinged 
        
        fn=FileNames[grep(AgeDelim,x = FileNames)] # Subset for the right age

        BiData=read.csv(fn[2],sep=",",header=FALSE,,encoding="native.enc")
        colnames(BiData)[1:9]=c("Radius_t","PC_t","L_CL","U_CL","Rand_PC_t","p","XC_t","L95_CL","U95_CL")
        BiData$Radius_t=BiData$Radius_t/1000 # set the calculation radii to km
        BiData[,7:9]=BiData[,7:9]/(1000000*convert) # Set for millions of Prey species 24950.144 is a denominator needed for the code for fish per m2
        
        # If there is no fish for a given species, devision or year class assign values of zero as Matlab assigns NaN
        if(is.nan(sum(BiData$PC_t))){BiData[,2:length(BiData)]=0} 
        
        #identify the patch scale using fPeaks custom fucntion. I have played with w and span and these do a reasonible smoothing job
        peaks=fPeaks(x=1:length(BiData$XC_t),y=BiData$XC_t,w=20,span=0.1)
        # this first if statement will quickly decidie if there are any peaks in the data (some don't have peaks)
        
        if(NROW(peaks$i)==0){
          peakx=rep(BiData$Radius_t[which(BiData$XC_t==max(BiData$XC_t))],NROW(BiData))
        } else if(max(peaks$y.hat[peaks$i])<0){
          peakx=rep(BiData$Radius_t[which(BiData$XC_t==max(BiData$XC_t))],NROW(BiData))
        } else{peakx=rep(peaks$i[1],NROW(BiData))}
        
               
        if(NROW(peaks$i)==0){
          peaky=rep(BiData$XC_t[which(BiData$XC_t==max(BiData$XC_t))],NROW(BiData))
        } else if(max(peaks$y.hat[peaks$i])<0){
          peaky=rep(BiData$XC_t[which(BiData$XC_t==max(BiData$XC_t))],NROW(BiData))
        } else {peaky=rep(BiData$XC_t[peaks$i[1]],NROW(BiData))}
        
        
        sData=peaks$y.hat #smoothed data
        YearTag=rep(years[i],NROW(BiData))#create a year tag for subset plotting later
        BiData=cbind(YearTag,BiData,peakx,peaky,sData) # Add the year tag and peak location to the data
        colnames(BiData)[1]="Year"
        GroupData=rbind(GroupData,BiData) # Sequentially add on each year of data
        rm(BiData,YearTag,rawdata,FileNames,peakx,peaky)
      }
    GroupData=GroupData[complete.cases(GroupData),] # this will remove any NaN data for years where the focal species has no data. 
    
  GroupData$Year=factor(GroupData$Year)# convert year to a factor to be used in the trellis ID
  
  #Plot options
  opt <- options("scipen" = 20) # change to scienttific notation

  #Create plot
  pl=xyplot(cbind(XC_t, U95_CL, L95_CL) ~ Radius_t | Year, data = GroupData,
       as.table = TRUE, layout = c(3, 6, 1), 
       panel = function(..., subscripts){
         panel.xyplot(x = GroupData[subscripts, "Radius_t"], 
                      y = GroupData[subscripts, "XC_t"], 
                      type = "l", col.line = "black", lwd = 2)
         panel.xyplot(x = GroupData[subscripts, "Radius_t"], 
                      y = GroupData[subscripts, "U95_CL"], 
                      type = "l", col.line = "black", lty = 2)
         panel.xyplot(x = GroupData[subscripts, "Radius_t"], 
                      y = GroupData[subscripts, "L95_CL"], 
                      type = "l", col.line = "black", lty =2)
         panel.segments(x0=GroupData[subscripts[1],"peakx"],
                        y0=0,
                        x1=GroupData[subscripts[1],"peakx"],
                        y1=GroupData[subscripts[1],"peaky"],lty=3)
        
       },
          scales=list(y=list(relation="free", rot=0)), # use this to set weather you want a common y axis or not
          xlab = "Calculation radius km (t)", ylab = paste("XC(t) millions of", Species),
          options=opt) # use this to set how the axis is labeled. 

  Species=gsub(" ","",Species)#Remove any spaces in the common species name for the file name of the plot

  if(SaveIm){
  png(filename = paste(path,"Univariate_XC_",Species,"_",AgeName,"yrs_NAFO_",nafo,".png",sep=""), 
      width = 2400, height = 2400, res = 300, bg="transparent")
  print(pl)
  dev.off()
  
  #Function results
  print(paste("Extra Contact XC ~ radius | Year saved: ",path,"Univariate_XC_",Species,"_",AgeName,"yrs_NAFO_",nafo,".png",sep=""))
} else print(pl)
  
  } # End function


