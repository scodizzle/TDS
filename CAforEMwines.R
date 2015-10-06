####  
####
library(tidyr)
library(dplyr)
library(ca)
library(candisc)
library(ggplot2)
# start with the normalized TDS data.....  

load("~/Dropbox/Frost_Research/Extended Macer Wines/EM_Analysis/tds.Rdata")

## from the normalized EM dataset..

## list of all the frequecy of each att. by normalized time
normFreq <- lapply(norm.ready, FUN = function(dat, ...) {
  aa <- spread(dat[,-4], key=time, value=freq)
  rownames(aa) <- aa[,1]
  aa <- aa[,-1]
})

## CA on all the wine note the drop of the "all off option"
EM.ca <- lapply(normFreq, FUN = function(dat){
  ca(dat[-1,])
  })

## plot em.. ##  kinda usless...  but vectorized...  
lapply(EM.ca, plot)

## pull out for ANOVA..  
EM.out <- lapply(EM.ca, FUN = function(dat) {
  data.frame(sensation=rownames(dat$rowcoord), dat$rowcoord, Mass=dat$rowmass, ChiDist=dat$rowdist, Inertia=dat$rowinertia, row.names = 1:5)
})
EM.out <- do.call(rbind.data.frame, EM.out)
EM.out$trt <- do.call(rbind, strsplit(rownames(EM.out), split = "[_.]"))[,1]
EM.out$fermRep <- do.call(rbind, strsplit(rownames(EM.out), split = "[_.]"))[,2]
row.names(EM.out) <- 1:nrow(EM.out)
EM.out$trt <- factor(EM.out$trt) 
EM.out$fermRep <- factor(EM.out$fermRep)
EM.out <- EM.out[,c("trt","fermRep","sensation","Dim1","Dim2","Dim3","Dim4","Mass","ChiDist","Inertia")]



## ANOVA
summary.aov(lm(as.matrix(EM.out[,-c(1:3)]) ~ trt + sensation, data=EM.out))
summary(manova(as.matrix(EM.out[,-c(1:3)]) ~ trt + fermRep + fermRep/sensation, data=EM.out), test="Wilks")

##  Ugh....  nothing.....
##  try just the trt...
TRT <- spread(tdsNORMall[,-3], key=time, value=domRate)
TRT <- cbind(TRT[TRT$sensation == "Sweet",][,-c(1)], TRT[TRT$sensation == "Sour",][,-c(1:2)], 
      TRT[TRT$sensation == "Bitter",][,-c(1:2)], TRT[TRT$sensation == "Hot",][,-c(1:2)], 
      TRT[TRT$sensation == "Astringent",][,-c(1:2)])
rr <- c(paste("Sw",seq(1:100), sep=""), paste("So",seq(1:100), sep=""), paste("B",seq(1:100), sep=""), paste("H",seq(1:100), sep=""), 
      paste("A",seq(1:100), sep=""))
row.names(TRT) <- TRT[,1]
TRT <- TRT[,-1]
colnames(TRT) <- rr
caTRT <- ca(TRT)


ggplot(as.data.frame(caTRT$rowcoord), aes(x=Dim1, y=Dim2, label=row.names(caTRT$rowcoord))) + 
  geom_text()










