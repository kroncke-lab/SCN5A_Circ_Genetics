# 2.21.18 doing a sliding window on Y axis
fakeX=rnorm(100)
fakeY=fakeX+rnorm(100)
fakeWeights=runif(100)
plot(fakeX,fakeY)
result=slidingWindow(fakeX,fakeY,fakeWeights)
plot(result$y,result$correlation)

#### FUNCTIONS ####
slidingWindow=function(x,y,weights,windowSize=0.5){
  library(wCorr)
  a=data.frame(x=x,y=y,weights=weights)
  a=a[!is.na(a$x) & !is.na(a$y),]
  yMin=min(y)
  yMax=max(y)
  yRange=yMax-yMin
  allCors=c()
  for(i in 1:100){
    yStart=(i-windowSize*50)*yRange/100+yMin
    yEnd=(i+windowSize*50)*yRange/100+yMin
    if(yStart<yMin){
      yStart=yMin
    }
    if(yEnd>yMax){
      yEnd=yMax
    }
    a2=a[a$y>=yStart & a$y<=yEnd,]
    currCor=weightedCorr(a2$x,a2$y,method='spearman',weights=a2$weights)
    allCors=c(allCors,currCor)
  }
  out=data.frame(y=(1:100)*yRange+yMin,correlation=allCors)
  return(out)
}
