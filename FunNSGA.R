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
  
}