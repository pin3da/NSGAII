rm(list=ls())

# Parámetros
MaxMin<-c(1,1)
genera<-1500 # numbers of generations
tm<-1 # mutation rate
tc<-0.8 # crosover rate
sizepob<-10 # poblation size

source("FunNSGA.R")
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
pob<-matrix(nrow=sizepob*2,ncol=numrow) # poblation two times more bigger

# Lo que se va a realizar es duplicar la población inicial.
pcity<-1:numrow
funObje<-matrix(nrow = numrow,ncol = 2)
for (i in 1:(sizepob*2)){
   pcity<-sample(pcity)
   pob[i,]<-pcity
   funObje[i,1]<-disre(pcity,dist)
   funObje[i,2]<-disre(pcity,poluci)
  }
funObje<-cbind(funObje,1:nrow(funObje))
funObje<-funObje[order(funObje[,1],funObje[,2]),]
frentes<-ParetoFront(funObje,MaxMin)
rmna<-which(is.na(frentes[,1]))
frentes<-frentes[-rmna,]
orfre<-apply(frentes,1,dife<-function(a){a[!is.na(a)]})

gra<-funObje[funObje[,3] %in% orfre[[1]],]
plot(gra[,1],gra[,2],type="b",col="blue",xlim = c(min(funObje[,1]),max(funObje[,1])),ylim =c(min(funObje[,2]),max(funObje[,2])))
for( i in 2:length(orfre)){
  par(new=TRUE)
  gra<-funObje[funObje[,3] %in% orfre[[i]],]
  if (is.matrix(gra)==TRUE){
    plot(gra[,1],gra[,2],type="b",col="blue",xlim = c(min(funObje[,1]),max(funObje[,1])),ylim =c(min(funObje[,2]),max(funObje[,2])))
   }else{
   plot(gra[1],gra[2],type="b",col="blue",xlim = c(min(funObje[,1]),max(funObje[,1])),ylim =c(min(funObje[,2]),max(funObje[,2])))
  }
 }      

