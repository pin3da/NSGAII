rm(list=ls())
source("FunNGSA.R")
corde<-matrix(c(37,52,49,49,52,64,20,26,40,30,21,47,17,63,31,62,52,33,51,21,42,41,31,32,5,25,
     12,42,36,16,52,41,27,23,17,33,13,13,57,58),ncol=2,byrow = T)
numrow<-dim(corde)[1]
dist<-matrix(nrow=20,ncol=20)

for(i in 1:(numrow-1)){
  for(j in (i+1):numrow){
    dist[i,j]<-{{corde[j,]-corde[i,]}%*%{corde[j,]-corde[i,]}}^{1/2}
    dist[j,i]<-dist[i,j]
  }
}
dist[is.na(dist)==TRUE]<-0
poluci<-matrix(nrow=20,ncol=20) # polution.
poluci<-apply(poluci,1,runif)*100 # matix whit polution. 
genera<-1500 # numbers of generations
tm<-1 # mutation rate
tc<-0.8 # crosover rate
sizepob<-10 # poblation size
pob<-matrix(nrow=sizepob*2,ncol=numrow) # poblation two times more bigger

# Lo que se va a realizae es duplicar la población inicial.
pcity<-1:numrow
funObdis<-vector(length = numrow)
funObemi<-vector(length = numrow)
for (i in 1:(sizepob*2)){
   pcity<-sample(pcity)
   pob[i,]<-pcity
   funObdis[i]<-disre(pcity,dist)
   funObemi[i]<-disre(pcity,poluci)
  }



