### a simulated example
## assuming you have 10 SNPs on chromosome 1
## with 10 sires and 100 dams

nsnps <- 100     ## number of snps
nsires <- 10    ## number of sire
ndams <- 100    ## number of dams
### generate ids for the sires and dams
sireids <- c(paste('sire00',1:9,sep=''),paste('sire0',10,sep=''))
damids <- c(paste('dam00',1:9,sep=''),paste('dam0',10:99,sep=''),paste('dam',100,sep=''))

### number of animals
nanim <- nsires + ndams

### just like the beagle output, generate haplotypes
### I just separate the 2 columns of haplotype 1 and 2 into different matrixes
haplos_A <- data.frame(matrix(rbinom(n=nanim,size=1,prob=0.5),ncol=nanim,nrow=nsnps))
haplos_B <- data.frame(matrix(rbinom(n=nanim,size=1,prob=0.5),ncol=nanim,nrow=nsnps))

### note that you can do the above with your data

### then give the names
colnames(haplos_A) <- c(sireids,damids)
colnames(haplos_B) <- c(sireids,damids)

### demonstration of how to generate offspring for sire001
### this can be looped to produce the desired number of offspring

sire='sire001'   ### using the side ID to get the haplotypes of the sire
sirecol=which(colnames(haplos_A)==sire)
hapABsire <- cbind.data.frame(A=haplos_A[,sirecol],B=haplos_B[,sirecol])
## t(hapABsire)
source('crossoverhaplos.R')
(datsire <- crossover.haps(hapABsire))

dam='dam001'    ### using the dam ID to get the haplotypes of the dam
damcol=which(colnames(haplos_A)==dam) 
hapABdam <- cbind.data.frame(A=haplos_A[,damcol],B=haplos_B[,damcol])
## t(hapABdam)
(datdam <- crossover.haps(hapABdam))


# for both sire and dam, after identifying the sire and dam of the offpsring
(dat <- cbind.data.frame(sire=crossover.haps(hapABsire),
                             dam=crossover.haps(hapABdam)))


(sirecont <- c(0.025,0.045,0.5,0.025,0.2,0.075,0.025,0.025,0.045,0.035))
sum(sirecont); length(sirecont)
nsnps <- 10000     ## number of snps
nsires <- 10    ## number of sire
ndams <- 1000    ## number of dams
(sirecont.off <- sirecont*ndams)

### generate ids for the sires and dams
sireids <- c(paste('sire00',1:9,sep=''),paste('sire0',10,sep=''))
damids <- c(paste('dam000',1:9,sep=''),
            paste('dam00',10:99,sep=''),
            paste('dam0',100:999,sep=''),
            paste('dam',1000,sep=''))

### number of animals
nanim <- nsires + ndams
haplos_A <- data.frame(matrix(rbinom(n=nanim,size=1,prob=0.5),ncol=nanim,nrow=nsnps))
haplos_B <- data.frame(matrix(rbinom(n=nanim,size=1,prob=0.5),ncol=nanim,nrow=nsnps))

### then give the names
colnames(haplos_A) <- c(sireids,damids)
colnames(haplos_B) <- c(sireids,damids)
source('crossoverhaplos.R')
(damalloc <- c(0,cumsum(sirecont.off)))

for(i in 1:length(sireids)){
  sire <- sireids[i]
  (sirecol=which(colnames(haplos_A)==sire))
  hapABsire <- cbind.data.frame(A=haplos_A[,sirecol],B=haplos_B[,sirecol])
  (damalloc.sire <- c((damalloc[i]+1):damalloc[i+1]))
  for(j in damalloc.sire){
    dam <- damids[j]    ### using the dam ID to get the haplotypes of the dam
    damcol=which(colnames(haplos_A)==dam) 
    hapABdam <- cbind.data.frame(A=haplos_A[,damcol],B=haplos_B[,damcol])
    (offproduced <- cbind.data.frame(sire=crossover.haps(hapABsire),
                                     dam=crossover.haps(hapABdam)))
    colnames(offproduced) <- c(paste('offhapA',j,sep=''),paste('offhapB',j,sep=''))
    
    if(j==1){
      offspring.gen <- offproduced
    } else {offspring.gen <- cbind.data.frame(offspring.gen,offproduced)
    }
  }
}
