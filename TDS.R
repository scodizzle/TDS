#Scott

#setup space

library(plyr)
library(ggplot2)
library(splines)

#import data

setwd("C:/Users/greg.hirson/Desktop")
load("normalize.rda")

#make the judge-wine combos for splitting
groups <- paste(tds$CJ, tds$Wine, sep="-")
tds.split <- split(tds, groups)


#for each function

tdseach <- function(x){
#make the category names, drop the non-data columns, change out the number for the letter
  nm = c("B", "S", "H", "A", "So", "0")
  x.sub <- x[,-c(1:17)]

#find the row with the 1, or return 0  
  f = function(x){
    if(sum(x) > 0){
      return(which(x==1))
    } else {
      return(6)
    }
  }
  #substitute letters for numbers, could be done by factors instead
  x.letter <- nm[sapply(x.sub, f)]
  return(x.letter)
  
}

#replace the numbers with letters
tdsletters <- lapply(tds.split, tdseach)

#RLE each of the judge-wine combos
tdsrle <- lapply(tdsletters, rle)


trim0rle <- function(x){
#trim the leading and trailing 0s  
  if(x$values[1] == "0"){
    x$values <- x$values[-1]
    x$lengths <- x$lengths[-1]
  }
  
  if(x$values[length(x$values)] == "0"){
    x$values <- x$values[-length(x$values)]
    x$lengths <- x$lengths[-length(x$lengths)]
  }
  return(x)
}

#trimmed rles
tdsrle2 <- lapply(tdsrle, trim0rle)    

#unRLEs

untdsrle <- lapply(tdsrle2, inverse.rle)


#computeTDSTable <- function(listofsequences, nPoints = 100){
#  lookup = c(Bitter = "B", Sweet = "S", Hot = "Hot", Astringent = "Astringent",
#             Sour = "SO")
#  
#  len <- length(l)
#
#}

normalizeRLE <- function(x, nPoints = 100){
#Normalizes the RLEs to nPoints units long (default to 100)  
  lookup = c(Bitter = "B", Sweet = "S", Hot = "Hot", Astringent = "Astringent",
             Sour = "SO")
  
  len <- length(x)								#the length of the sequence
  frac <- ceiling(seq(nPoints)/nPoints * len)	#ceiling approximates part integer part
												#of the fraction of the time normalized
												#time interval
  x[frac]
}

#normalized list of TDS
normtds <- lapply(untdsrle, normalizeRLE)

#combined into a data frame
normFrame <- data.frame(do.call('rbind', normtds))

#description of conditions for splitting
normJW <- rownames(normFrame)
normJW <- gsub(".*, [A-Z]-", "", normJW)
winedesc <- data.frame(do.call("rbind", strsplit(normJW, "-")))
names(winedesc) <- c("Tannin", "Acid", "EtOH")

#split by wine
normFrame.wine <- split(normFrame, normJW)

preplotTDS <- function(data){
  
  lookup = c(Bitter = "B", Sweet = "S", Hot = "Hot", Astringent = "Astringent",
             Sour = "SO")
  #preallocate output matrix
  out <- data.frame(time = 0, sensation = "Z", freq = 0, domRate = 0)
  
  #for each column, count the number of each factor
  
  
  for(i in seq(ncol(data))){
    
    temp <- summary(factor(data[,i], levels = c("B", "S", "H", "A", "So", "0")))
    #grow list
    out <- rbind(out, 
                 data.frame(time = i, sensation = names(temp), 
				 freq = temp, domRate = temp/sum(temp)))

  }
  
  out <- out[-1,]
  out
}

norm.ready <- lapply(normFrame.wine, preplotTDS)


TDS.graph = function(data, cline, sline, title, ...){
  #cline -- chance line - this should be calculated automatically from the length(unique(sensations))
  #sline -- sigline
  
  ggplot(data, aes(x=time, y=domRate)) +
    geom_smooth(aes(linetype=sensation, fill=sensation), method="lm", formula= y~ns(x,18), se=TRUE) +
    ylim(0,1) + xlim(0,90) +
    geom_point(aes(shape=sensation, color = sensation)) +
    geom_segment(aes(x = 15, y = 0, xend = 15, yend = .50), linetype=5) +
    geom_abline(intercept=cline, slope=0, linetype=2) +
    geom_abline(intercept=sline, slope=0) +
    geom_polygon(data=data.frame(x=c(0,90,90,0), y=c(0,0,cline,cline)), 
                 aes(x,y), fill="green", alpha=0.3) +
    geom_polygon(data=data.frame(x=c(0,90,90,0), y=c(0,0,sline, sline)), 
                 aes(x,y), fill="orange", alpha=0.2) +
    ggtitle(title) +
    ylab("Dominance Rate (%)") + xlab("Time")
  
}

#pdf(file = "tdsgraphs.pdf")
#for(i in 1:length(norm.ready)){print(TDS.graph(norm.ready[[i]], 
#                                      cline=0.2, sline=0.285, title=names(norm.ready)[i]))}
#dev.off()
#TDS.graph(norm.ready[[1]], cline=0.2, sline=0.285, title = names(norm.ready)[1])
