## This code will create the bathymetric map and trandition matrix used to calcualte distance between points


#Read in trawl survey data ----------
dfodata <- read.csv("Data/TrawlSurveyData_2012.csv")

#Establish limits with a 1 degree buffer  ----------
lat.range <- c(min(dfodata$LAT_DEC)-1,max(dfodata$LAT_DEC)+1)
long.range <- c(min(dfodata$LONG_DEC)-1,max(dfodata$LONG_DEC)+1)

## download & save bathymetric data ----------
bathydata <- getNOAA.bathy(lon1= long.range[2],
                           lon2= long.range[1],
                           lat1= lat.range[1],
                           lat2= lat.range[2],
                           resolution = 1,keep=TRUE)


#Calculate the depths of each station ----------
Site.depths <- get.depth(mat=bathydata,x=dfodata[which(dfodata$Strat_Type=="Core"),"LONG_DEC"],
                         y=dfodata[which(dfodata$Strat_Type=="Core"),"LAT_DEC"],locator = FALSE)

range(Site.depths$depths) # all below 0 and the deepest are ~3300 m on the shelf break

## build transition object (0 - 100 m than the deepest sample ~ 3500 m) ----------
translayer <- trans.mat(bathydata, min.depth = 0,max.depth=min(Site.depths$depth)-100) # land is a barrier


## map fill colours ----------
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2","lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))

## set map limits ----------
Lat.lim=c(lat.range[1],lat.range[2])
Long.lim=c(long.range[1],long.range[2])


#plot the map with bathymetry layer and site locations. Note the 1000 m isobath is also plottd. 
png(filename = "Figures/Maps/SurveyMap_bathymetry.png", 
    width = 1750, height = 2000, res = 300, bg="white")

#plot the sites and the map (1000m Isobath) ---------
plot(bathydata, deep=-1000,shallow=-1000, image = TRUE, land = TRUE, lwd = 0.8, bpal = list(c(0, max(bathydata), greys), c(min(bathydata), 0, blues)))
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="black", fill=FALSE, resolution=0,add=TRUE)
map.axes();map.scale(cex=0.8,ratio=FALSE)
#sites in water
points(dfodata[which(dfodata$Strat_Type=="Core"),"LONG_DEC"],
       dfodata[which(dfodata$Strat_Type=="Core"),"LAT_DEC"],
       pch=19,cex=0.1,col="black") #Add points

dev.off()

#Remove all objects except the bathy layer and transition object
rm(list=setdiff(ls(), c("translayer","bathydata")))

