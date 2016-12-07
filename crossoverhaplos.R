## assume 0 or 1 or 2 crossovers (no kosambi or haldam mapping functions)
## no mutation 
## no recombination hotspots

crossover.haps <- function(hapAB,probRec=c(0=0.10,1=0.75,2=0.15)){
  recomb.off <- sample(x=0:2,size=1,prob=probRec)
  nsnp.perchr <- nrow(hapAB)
  if(recomb.off==0){
    choosehap <- sample(x=1:2,size=1)
    usehap <- hapAB[,choosehap] 
  } else if(recomb.off==1){
    (recom.spot <- sort(sample(1:nsnp.perchr,recomb.off)))
    choosehap <- sample(x=1:2,size=1)
    usehap <- hapAB[,choosehap]
    (LRside <- sample(x=1:2,size=1))
    if(LRside==1){recom.hap <- 1:recom.spot
    } else {recom.hap <- recom.spot:nsnp.perchr}
    if(choosehap==1){usehap[recom.hap] <- hapAB[recom.hap,2]
    } else if(choosehap==2) {
      usehap[recom.hap] <- hapAB[recom.hap,1]
      }
  } else if(recomb.off==2){
    (recom.spot <- sort(sample(1:nsnp.perchr,recomb.off)))
    choosehap <- sample(x=1:2,size=1)
    usehap <- hapAB[,choosehap]
    (Lside <- 1:recom.spot[1])
    (Rside <- recom.spot[2]:nsnp.perchr)
    if(choosehap==1){
      usehap[Lside] <- hapAB[Lside,2]
      usehap[Rside] <- hapAB[Rside,2]
    } else if(choosehap==2){
      usehap[Lside] <- hapAB[Lside,1]
      usehap[Rside] <- hapAB[Rside,1]
    }
  }
  return(usehap)
}

  
