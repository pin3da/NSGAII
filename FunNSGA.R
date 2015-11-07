# this file has all functions to run the NGSA algorithm. 

# we need compute the total distance traveled, the phat is given by "pcity" 
# vector that contain the order followed. 

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
  x1<-cbind(funObje,1:numrow)
  x1<-x1[order(x1[,1],x1[,2]),]
  n<-0
  front<-matrix(nrow = numrow,ncol=numrow)
  breakd<-0
  cont<-0
  cont1<-2
  vec<-vector(length = numrow)
    while(breakd < numrow){
        for (i in 1:numrow){
          for (j in (i+1):numrow){
              if (all(MaxMin==c(0,1))==TRUE){
                if(x1[i,1] == x1[j,1] && x1[i,2] <= x1[j,2]){
                  break
                }else if (x1[i,2] < x1[j,2]){
                  if(cont==0){
                     vec[1]<-i
                     vec[2]<-j
                     cont<-cont+1
                  }else{
                     cont1<-cont1+1
                     vec[cont1]<-j
                    }
                } 
              }   
          }
          
        }
    }
}