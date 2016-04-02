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