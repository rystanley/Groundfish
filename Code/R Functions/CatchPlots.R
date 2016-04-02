CatchPlots<-function(data,Divisions,Species){
  
  require(ggplot2) # this fuction needs ggplot2
  #This function will creat a catch plot for a given species and year
  #data should be a subsetted catch for a given species (defined by 'Species') and set of NAFO divisions (defined by 'Divisions')
  #This funcution plots mean +/- Standard error. This can be swithched to confidence intervals or standard devaition with slight code alterations
    
  ### obtain discriptions of the focal species and NAFO divisions using ALL data
  Divisions=Divisions
  Species=Species
  Speciesf=gsub(" ","",Species)# remove any spaces for the file name
    
  C.mean=aggregate(data,by=list(data$Year),FUN=mean)
  C.sd=aggregate(data,by=list(data$Year),FUN=sd)
  C.n=aggregate(data,by=list(data$Year),FUN=NROW)
  # Coerse the Standard error
  C.se=1:NROW(C.mean)
  for (i in 1:NROW(C.mean)){
    C.se[i]=C.sd[i,3]/sqrt(C.n[i,3])
  }
  #Put it all together and relable colomns
  Dat=cbind(C.mean[,2:3],C.sd[,3],C.se)
  colnames(Dat)[1:4]=c("Year","Mean","StDev","Se")
  
  Dat[,5]=Dat[,2]-Dat[,4]
  Dat[,6]=Dat[,2]+Dat[,4]
  colnames(Dat)[5:6]=c("ymin","ymax")
  
  Dat1=Dat # Dat1 is the summary statistics for all stations
  
  rm(C.mean,C.sd,C.n,C.se,Dat)
  
  nz=which(data[,2]!=0) #row numbers of all stations above zero
  data=data[nz,]
  
  C.mean=aggregate(data,by=list(data$Year),FUN=mean)
  C.sd=aggregate(data,by=list(data$Year),FUN=sd)
  C.n=aggregate(data,by=list(data$Year),FUN=NROW)
  # Coerse the Standard error
  C.se=1:NROW(C.mean)
  for (i in 1:NROW(C.mean)){
    C.se[i]=C.sd[i,3]/sqrt(C.n[i,3])
  }
  #Put it all together and relable colomns
  Dat=cbind(C.mean[,2:3],C.sd[,3],C.se)
  colnames(Dat)[1:4]=c("Year","Mean","StDev","Se")
  
  Dat[,5]=Dat[,2]-Dat[,4]
  Dat[,6]=Dat[,2]+Dat[,4]
  colnames(Dat)[5:6]=c("ymin","ymax")
  
  Dat2=Dat # Dat2 is the summary statistics for all stations with the species (no zeros) 
  
  ID=rep("a",NROW(Dat1))
  Dat1=cbind(Dat1,ID)
  ID=rep("b",NROW(Dat2))
  Dat2=cbind(Dat2,ID)
  
  Dat=rbind(Dat1,Dat2) #stack them together so we can facet plot using "ID" as a facet variable
  
  #plot so all the stations (including stations with no focal species are plotted on top (a) and zeros removed on bottom (b))
  p=ggplot(data=Dat,aes(x=Year,y=Mean))+geom_point(cex=3)+geom_line()+geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.2)+labs(y="Abundance (mean per tow)")+theme_bw()+scale_x_continuous(breaks=1995:2011)+theme(axis.text.x=element_text(angle=-90))
  p=p + facet_grid(ID ~ .,scales="free_y")+theme(strip.text.y = element_text(angle = 0,size=20))
  
  png(filename = paste("Yearly_Catch_",Speciesf,"_NAFO_",Divisions,".png",sep=""), 
      width = 2400, height = 2400, res = 300, bg="transparent")
  print(p)
  dev.off()
  
}
