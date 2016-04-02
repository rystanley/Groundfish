# function for saving pictures
PicSave=function(plot,pathname,width = 2400, height = 2400,axis=TRUE){
  if(axis==FALSE){plot=plot+theme(axis.text.x = element_blank())} # if you don't want the x axis
  png(filename = pathname, 
      width = width, height = height, res = 300, bg="transparent")
  print(plot)
  dev.off()
}

PicSave.L=function(plot,pathname,width = 3600, height = 2400,axis=TRUE){
  if(axis==FALSE){plot=plot+theme(axis.text.x = element_blank())} # if you don't want the x axis
  png(filename = pathname, 
      width = width, height = height, res = 300, bg="transparent")
  print(plot)
  dev.off()
}