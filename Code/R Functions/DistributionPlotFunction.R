DistributionPlots=function(x,cex.scale=c(0,4),age,legend.class,path,species="Species"){
  
  #This function will create 8 plots of a given stock of a given year class
  #x: a dataframe with the plots
      #** x needs the column names c("Year","Latitutde","Longitude")+ a column with the age counts
  #cex.scale is the range of point sizes the data will be rescaled to defaults to c(0,4)
  #age is the age of the fish
  #legend.class is the levels of the legend usually 3 points which represent the range of data
  #path is a path target of the plots where you want them saved. 
  #species : is the species and is needed for the pathname
  
  colnames(x)[4]="Age"
  
  #Create the groupings for years every 6 years then the remainder
  AgeScale1=unique(x$Year)[1:6]
  AgeScale2=unique(x$Year)[7:12]
  AgeScale3=unique(x$Year)[13:length(unique(x$Year))]
  

data1=subset(x, Year %in% AgeScale1,select=c("Year","Latitude","Longitude","Age"))
data1$rScale=rescale(data1$Age,to=cex.scale)
data2=subset(x,Year %in% AgeScale2,select=c("Year","Latitude","Longitude","Age"))
data2$rScale=rescale(data2$Age,to=cex.scale)
data3=subset(x,Year %in% AgeScale3,select=c("Year","Latitude","Longitude","Age"))
data3$rScale=rescale(data3$Age,to=cex.scale)
data4=x[,c("Year","Latitude","Longitude","Age")]
data4$rScale=rescale(data4$Age,to=cex.scale) #rescale the values for a reasonible cex sizing

#Log10 Scaled
map1=bubble.maps(Latitude~Longitude|factor(Year),data=data1,cex.symbol=log10(data1$Age))
map2=bubble.maps(Latitude~Longitude|factor(Year),data=data2,cex.symbol=log10(data2$Age))
map3=bubble.maps(Latitude~Longitude|factor(Year),data=data3,cex.symbol=log10(data3$Age))
map4=bubble.maps.key(Latitude~Longitude|factor(Year),data=data4,cex.symbol=log10(data4$Age),
                     corner=c(0.80,0.07),title=expression(paste("Age"[age]," plaice ", "abundance")),
                     text=as.character(legend.class),
                     cex.key=log10(legend.class))

#Rescaled (min- 0 max 3 for cex)
map5=bubble.maps(Latitude~Longitude|factor(Year),data=data1,cex.symbol=data1$rScale)
map6=bubble.maps(Latitude~Longitude|factor(Year),data=data2,cex.symbol=data2$rScale)
map7=bubble.maps(Latitude~Longitude|factor(Year),data=data3,cex.symbol=data3$rScale)
map8=bubble.maps.key(Latitude~Longitude|factor(Year),data=data4,cex.symbol=data4$rScale,
                     corner=c(0.80,0.07),title=expression(paste("Age"[age]," plaice ", "abundance")),
                     text=as.character(legend.class),
                     cex.key=rescale(c(0,legend.class),to=cex.scale)[2:(length(legend.class)+1)])

#Save maps
PicSave(map1,paste(path,"LogScale/Log_",species,"_Age-",age,"_",AgeScale1[1],"-",AgeScale1[2],".png",sep=""))#LogScale
PicSave(map2,paste(path,"LogScale/Log_",species,"_Age-",age,"_",AgeScale2[1],"-",AgeScale2[2],".png",sep=""))
PicSave(map3,paste(path,"LogScale/Log_",species,"_Age-",age,"_",AgeScale3[1],"-",AgeScale3[2],".png",sep=""))
PicSave(map4,paste(path,"LogScale/Log_",species,"_Age-",age,"_",AgeScale1[1],"-",AgeScale3[length(AgeScale3)],".png",sep=""))
PicSave(map5,paste(path,"Scale/",species,"_Age-",age,"_",AgeScale1[1],"-",AgeScale1[2],".png",sep=""))#Relative Scale
PicSave(map6,paste(path,"Scale/",species,"_Age-",age,"_",AgeScale2[1],"-",AgeScale2[2],".png",sep=""))
PicSave(map7,paste(path,"Scale/",species,"_Age-",age,"_",AgeScale3[1],"-",AgeScale3[2],".png",sep=""))
PicSave(map8,paste(path,"Scale/",species,"_Age-",age,"_",AgeScale1[1],"-",AgeScale3[length(AgeScale3)],".png",sep=""))

#Clean workspace
rm(data1,data2,data3,data4,map1,map2,map3,map4,map5,map6,map7,map8,legend.class)