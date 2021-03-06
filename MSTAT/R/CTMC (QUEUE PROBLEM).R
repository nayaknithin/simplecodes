#' @title MM1 Queu Problem
#' @author Nithin
#' @param lambda Arrival rate
#' @param mu Departure rate
#' @param  time time for which you want the process to run
#' @export





MM1=function(lambda,mu,time){
  n=0;t=0
  Q=0;Queue=data.frame(transition=n,Time=t,Queue_Length=Q)
  r=runif(1)

  if(Q==0){
    X=-(log(r))/lambda
    t=X+t
    n=n+1
    Q=Q+1
    Queue[n,]=c(n,X,Q)
  }
  if(!Q==0){r=runif(1);X=-(log(r))/lambda
  Y=-(log(r))/mu
  repeat{
    n=n+1
    Z=min(X,Y)
    t=t+Z
    if(Z==X){Q=Q+1;r=runif(1);X=-(log(r))/lambda
    Y=Y-Z
    }else{
      Q=Q-1;r=runif(1)
      Y=-(log(r))/mu
      X=X-Z
    }
    Queue[n,]=c(n,Z,Q)

    if(Q==0){r=runif(1)
    X=-(log(r))/lambda
    t=X+t
    n=n+1
    Q=Q+1
    Queue[n,]=c(n,X,Q)
    }
    if(t>time)break}
  }

  a=plot(cumsum(Queue$Time),Queue$Queue_Length,type="s",xlab="Time",ylab="Queue length",main="M/M/1 Simulation")

  return(list(Queue,a))
}

