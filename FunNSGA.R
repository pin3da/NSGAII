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

apilamiento<-function(mat){
  numfil<-nrow(mat)
  apilamiento<-vector(length = (numfil-2))
  f1max<-max(mat[,1])
  f1min<-min(mat[,1])
  f2max<-max(mat[,2])
  f2min<-min(mat[,2])
  for (i in 1:(numfil-2)){
    f1<-abs({mat[i,1]-mat[(i+2),1]}/{f1max-f1min})
    f2<-abs({mat[i,2]-mat[(i+2),2]}/{f2max-f2min})
    apilamiento[i]<-f1+f2
  }
  return(apilamiento)
}

nwpob<-function(orfre,funObje,pob,sizepob){
  summ<-0
  ind<-0
  while(summ < sizepob){
    ind<-ind+1
    summ<-summ+length(orfre[[ind]])
  }
  if(summ==sizepob){
    a<-NULL
    for(i in 1:ind){
      a<-c(a,orfre[[i]])
    }
    nwpob<-funObje[funObje[,3] %in% a,]
  }else{ # aqui vamos
    
  }
  
}

