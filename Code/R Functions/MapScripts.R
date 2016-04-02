#######################################################################################
library(mapdata)
library(lattice)
# Function for wraping lattic xyplot with color key to map response variable as selected color gradient
# Argument
# form = lattic "fromula" method; see xyplot() for more detail
# data = a data frame with longitude, latitude, response variable and grouping variable
# fill.color = background (fill) color for the open plot symbols given by pch=21:25.
# aspect = physical aspect ratio of the panels (vertical size/horizontal size); see xyplot() for more detail
# col = A specification of the colors to be assigned to each interval defined by at; see level.colors()
# at = A numeric variable of breakpoints defining intervals along the range of response variable.
# title = A character string or expression giving a title for the key.
# xlim = two element numeric vector giving a range of longitudes, expressed in degrees, 
#        to which drawing should be restricted.
# ylim = two element numeric vector giving a range of latitudes, expressed in degrees, 
#        to which drawing should be restricted.

# Bathymetric data for making isobath lines
contour.x = rep(seq(-70, -40, length = 181), 97)
contour.y = rep(seq(56, 40, length = 97), each = 181)
contour.z = -scan("c:/Users/RyanStanley/Google Drive/Contracts/NAFOLRES-5549_data/NAFOLRES-5549/NAFOLRES-5549.asc", skip = 6)

data(worldHiresMapEnv)
land<-map(database = "worldHires", xlim = c(-70, -40), ylim =c(40, 56), 
          plot = FALSE, fill = TRUE)


gradient.maps<-
  function(form = lat~long|factor(annee), data, fill.color, aspect = 0.7,
           at, col = cb.colors, title,...){     
    xyplot(form, data = data,
           par.settings = list(strip.background = list(col = "light green")),
           as.table = TRUE, scales = list(draw = TRUE), aspect = aspect,
           col = "black", fill.color = fill.color, main = title, 
           panel = function(x, y, fill.color, ..., subscripts) {
             panel.grid(h = -1, v = -1)
             panel.contourplot(contour.x, contour.y, contour.z,
                               subscripts = TRUE,
                               at = c(200, 500, 1000, 6000), 
                               labels = list(alpha = 0.8, cex = 0.5), 
                               label.style = "align",
                               contour = TRUE, col = "gray30", lwd = 0.8,
                               col.regions = c("azure", "azure1", "azure2")                               
             )
             panel.xyplot(x, y, pch = 21, fill = fill.color[subscripts],...)
             panel.polygon(land, col = "gray80")
           },
           legend = list(top = list(fun = draw.colorkey,
                                    args = list(key = list(space="top", col = col,
                                                           at = at, draw = FALSE,...))) ),
           xlab = "Longitude", ylab = "Latitude"
    )
  }

# Function for wraping lattic xyplot with color key to map response variable as selected color classes
# Argument
# form = lattic "fromula" method; see xyplot() for more detail
# data = a data frame with longitude, latitude, response variable and grouping variable
# fill.color = background (fill) color for the open plot symbols given by pch=21:25.
# aspect = physical aspect ratio of the panels (vertical size/horizontal size); see xyplot() for more detail
# space = location of the colorkey, can be one of "left", "right", "top" and "bottom".
# corner = Common values of cornerarec(0, 0), c(1, 0), c(1,1),andc(0,1), which denote the corners of the unit square, 
#          but fractional values are also allowed.
# title = A character string or expression giving a title for the key.
# test = character vector for the key
# fill = background (fill) color for the open plot symbols given by pch=21:25 in the key.
# xlim = two element numeric vector giving a range of longitudes, expressed in degrees, 
#        to which drawing should be restricted.
# ylim = two element numeric vector giving a range of latitudes, expressed in degrees, 
#        to which drawing should be restricted.

class.maps<-
  function(form = Latitude~Longitude|factor(Year), data, fill.color, aspect = 0.7,
           space = NULL, corner = NULL, title = "Cluster", text = as.character(1:5), 
           fill = cb.colors(5),...){ 
    xyplot(form, data = data,
           par.settings = list(strip.background = list(col = "light green")),
           as.table = TRUE, scales = list(draw = TRUE), aspect = aspect,
           col = "black", fill.color = fill.color,
           panel = function(x, y, fill.color, ..., subscripts) {
             panel.grid(h = -1, v = -1)
             panel.contourplot(contour.x, contour.y, contour.z,
                               subscripts = TRUE,
                               at = c(200, 500, 1000, 6000), 
                               labels = list(alpha = 0.8, cex = 0.5), 
                               label.style = "align",
                               contour = TRUE, col = "gray30", lwd = 0.8,
                               col.regions = c("azure", "azure1", "azure2")                               
             )
             panel.xyplot(x, y, pch = 21, fill = fill.color[subscripts],...)
             panel.polygon(land, col = "gray80")
           },
           key = list(space = space, corner = corner, title = title, text = list(text), 
                      points = list(pch = 21, fill = fill ),... ),
           xlab = "Longitude", ylab = "Latitude"
    )
  }

# Removing key
nokey.maps<-
  function(form = Latitude~Longitude|factor(Year), data, fill.color, aspect = 0.7,...){ 
    xyplot(form, data = data,
           par.settings = list(strip.background = list(col = "light green")),
           as.table = TRUE, scales = list(draw = TRUE), aspect = aspect,
           col = "black", fill.color = fill.color,
           panel = function(x, y, fill.color, ..., subscripts) {
             panel.grid(h = -1, v = -1)
             panel.contourplot(contour.x, contour.y, contour.z,
                               subscripts = TRUE,
                               at = c(200, 500, 1000, 6000), 
                               labels = list(alpha = 0.8, cex = 0.5), 
                               label.style = "align",
                               contour = TRUE, col = "gray30", lwd = 0.8,
                               col.regions = c("azure", "azure1", "azure2")                               
             )
             panel.xyplot(x, y, pch = 21, fill = fill.color[subscripts],...)
             panel.polygon(land, col = "gray80")
           },
           xlab = "Longitude", ylab = "Latitude"
    )
  }

# Function for wraping lattic xyplot to map response variable as bubbles
# Argument
# form = lattic "fromula" method; see xyplot() for more detail
# data = a data frame with longitude, latitude, response variable and grouping variable
# fill.color = background (fill) color for the open plot symbols given by pch=21:25.
# aspect = physical aspect ratio of the panels (vertical size/horizontal size); see xyplot() for more detail
# cex.symbol = controling symbol size of the map
# space = location of the colorkey, can be one of "left", "right", "top" and "bottom".
# corner = Common values of corner are c(0, 0), c(1, 0), c(1,1), andc(0,1), which denote the corners of the unit square, 
#          but fractional values are also allowed.
# title = A character string or expression giving a title for the key.
# test = character vector for the key
# fill = background (fill) color for the open plot symbols given by pch=21:25 in the key.
# cex.key = controlling symbol size of the key

bubble.maps<-
  function(form, data, fill.color = "transparent", cex.symbol = 1, aspect = "fill",
           space = NULL, corner = NULL, title, text, cex.zero=0.1,
           fill = "transparent", cex.key = 1,...){ 
    z=which(data[,4]==0)
    xyplot(form, data = data,
           par.settings = list(strip.background = list(col = "light green")),
           as.table = TRUE, scales = list(draw = TRUE,relation="same"), aspect = aspect,
           col = "black", fill.color = fill.color,
           panel = function(x, y, fill.color, ..., subscripts) {
             panel.grid(h = -1, v = -1)
             panel.contourplot(contour.x, contour.y, contour.z,
                               subscripts = TRUE,
                               at = c(200,500, 1000, 6000), 
                               labels = list(alpha = 0.8, cex = 0.5), 
                               label.style = "align",
                               contour = TRUE, col = "grey30", lwd = 0.8,
                               col.regions = c("white", "azure", "azure1") 
                               #col.regions = "transparent"
             )
             #plot the locations of stations scaled to the catch 
             panel.xyplot(x, y, pch = 21, col = "black", subscripts=TRUE,
                          fill = rgb(0.5,0.5,0.5,0.4), cex = cex.symbol[subscripts])
             #add the station locations where no 'species' were found
             panel.xyplot(x[z], y[z], pch = 3, col = "black", subscripts=TRUE,cex=cex.zero)
             panel.polygon(land, col = "azure4")
           },
           xlab = "Longitude °W", ylab = "Latitude °N"
    ) 
  }

#######################################################################################


bubble.maps.key<-
  function(form, data, fill.color = "transparent", cex.symbol = 1, aspect = "fill",
           space = NULL, corner = NULL, title, text, cex.zero=0.1,
           fill = "transparent", cex.key = 1,...){ 
    z=which(data[,4]==0)
    xyplot(form, data = data,
           par.settings = list(strip.background = list(col = "light green")),
           as.table = TRUE, scales = list(draw = TRUE,relation="same"), aspect = aspect,
           col = "black", fill.color = fill.color,
           panel = function(x, y, fill.color, ..., subscripts) {
             panel.grid(h = -1, v = -1)
             panel.contourplot(contour.x, contour.y, contour.z,
                               subscripts = TRUE,
                               at = c(200,500, 1000, 6000), 
                               labels = list(alpha = 0.8, cex = 0.5), 
                               label.style = "align",
                               contour = TRUE, col = "grey30", lwd = 0.8,
                               col.regions = c("white", "azure", "azure1") 
                               #col.regions = "transparent"
             )
             #plot the locations of stations scaled to the catch 
             panel.xyplot(x, y, pch = 21, col = "black",subscripts=TRUE,
                          fill = rgb(0.5,0.5,0.5,0.3),cex=cex.symbol[subscripts])
             #add the station locations where no 'species' were found
             panel.xyplot(x[z], y[z], pch = 3, col = "black",subscripts=TRUE,cex=cex.zero)
             panel.polygon(land, col = "azure4")
           },
           #key = list(space = space, corner = corner, title = title, text = list(text), 
           # points = list(pch = 21, fill = fill, cex = cex.key ),... ),
           key = list(space = space, corner = corner, title = title, text = list(text), 
                      points = list(pch = 21, fill = fill, cex = cex.key,lineheigth=10 ),... ),
           xlab = "Longitude °W", ylab = "Latitude °N"
    ) 
  }