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
  repet<-vector(length = numrow)
  cont<-0
  for(i in 1:(numrow-1)){
    if(all(funObje[i,1:2]==funObje[(i+1),1:2])==TRUE){
      cont<-cont+1
      repet[cont]<-i
    }
  }
  # se elieminan las funciones objetivos iguales. 
  if(any(repet!=0)==TRUE){
    repet<-repet[repet > 0]
    funObje<-funObje[-repet,]
  }
  if(all(MaxMin==c(1,1)) || all(MaxMin==c(1,0))){
    funObje<-funObje[order(funObje[,1],funObje[,2],decreasing=TRUE),]
  }
  front<-matrix(nrow = nrow(funObje),ncol=nrow(funObje))
  contcol<-0
  contfil<-0
  breakd<-0
  maxfil<-nrow(funObje)
  while(breakd < (maxfil-1)){
    contfil<-contfil+1
    contcol<-0
    for (i in 1:nrow(funObje)){
      unic<-0
      for (j in 1:nrow(funObje)){
        if (all(MaxMin==c(0,1))==TRUE && i!=j){
          if(funObje[j,1] <= funObje[i,1] && funObje[j,2] >= funObje[i,2]){
            unic<-1
            break
          }    
        }else if(all(MaxMin==c(1,0))==TRUE && i!=j){
          if(funObje[j,1] >= funObje[i,1] && funObje[j,2] <= funObje[i,2]){
            unic<-1
            break
          }
        }else if(all(MaxMin==c(1,1))==TRUE && i!=j){
          if(funObje[j,1] >= funObje[i,1] && funObje[j,2] >= funObje[i,2]){
            unic<-1
            break
          } 
        }else{
          if(funObje[j,1] <= funObje[i,1] && funObje[j,2] <= funObje[i,2] && i!=j){
            unic<-1
          break
          }
        }
      }
      if(unic==0){
        contcol<-contcol+1
        front[contfil,contcol]<-funObje[i,3]
      }
    }
    breakd<-sum(!is.na(front))
    
    a<-front[!is.na(front)]
    p<-which(funObje[,3] %in% a)
    funObje<-funObje[-p,]
  }
  if(breakd != maxfil){
    front[(contfil+1),1]<-funObje[3]
  }
  return(front)
  }









