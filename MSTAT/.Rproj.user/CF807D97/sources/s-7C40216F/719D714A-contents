#' @title MM1Queue
#' @author Nithin Nayak
#' @param N initial size of the queue
#' @param lambda arrival rate
#' @param mu Departure rate
#' @param Time Time for which you want to see arrivals and departures in the queue
#' @param n number of transitions; either n or Time should be mentioned
#' @examples
#' MM1Queue(lamda=3,mu=4,Time=8)
#' MM1Queue(lamda=4,mu=4,n=100)
#' @export




MM1Queue=function(N=0,lambda,mu,Time=0,n=0){
  m=0;result=data.frame(N=0,Total_Time=0);t=0
  repeat{m=m+1
  r=runif(1)
  if(N==0){X=-log(r)/lambda;t=t+X;N=N+1}else{X=-log(r)/lambda;Y=-log(r)/mu
  Z=min(X,Y)
  if(Z==Y){N=N-1}else{N=N+1}
  t=t+Z
  }
  result[m,]=c(N,t)
  time=ceiling(t)
  if(time==Time|m==n){break}}
  return(result)}


