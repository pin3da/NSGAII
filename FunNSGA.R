disre<-function(pcity,dis){
   n<-length(pcity)
   disre<-0
    for (i in 1:(n-1)){
      disre<-disre+dis[pcity[i],pcity[i+1]]
    }
   disre<-disre+dis[pcity[n],pcity[1]]
   return(disre)
}

ParetoFront<-function(funObje,MaxMin){
  numrow<-nrow(funObje)
  x1<-funObje
  repet<-vector(length = numrow)
  cont<-0
  for(i in 1:(numrow-1)){
    if(all(x1[i,1:2]==x1[(i+1),1:2])==TRUE){
      cont<-cont+1
      repet[cont]<-i
    }
  }
  # se elieminan las funciones objetivos iguales. 
  if(any(repet!=0)==TRUE){
    repet<-repet[repet > 0]
    x1<-x1[-repet,]
  }
  
  front<-matrix(nrow = nrow(x1),ncol=nrow(x1))
  contcol<-0
  contfil<-0
  breakd<-0
  maxfil<-nrow(x1)
  while(breakd < (maxfil-1)){
    contfil<-contfil+1
    contcol<-0
    for (i in 1:nrow(x1)){
      unic<-0
      for (j in 1:nrow(x1)){
        if (all(MaxMin==c(0,1))==TRUE && i!=j){
          if(x1[j,1] <= x1[i,1] && x1[j,2] >= x1[i,2]){
            unic<-1
            break
          }    
        }
      }
      if(unic==0){
        contcol<-contcol+1
        front[contfil,contcol]<-x1[i,3]
      }
    }
    breakd<-sum(!is.na(front))
    
    a<-front[!is.na(front)]
    p<-which(x1[,3] %in% a)
    x1<-x1[-p,]
  }
  if(breakd != maxfil){
    front[(contfil+1),1]<-x1[3]
  }
  return(front)
  }









