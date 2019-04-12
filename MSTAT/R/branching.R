#' @title branching process
#' @author Nithin Nayak
#' @param X_o the initial population size
#' @param p probabilty parameter
#' @param n number of generations (Dont Specify if you want the number of generation before extintion  )
branching=function(X_o,p,n=NULL){
  result=data.frame(gen=0,X=X_o)
  X=X_o;  m=0
  repeat{m=m+1
  z=array()
  for(i in 1:X){
    r=runif(1);a=floor(log(r)/log(1-p))
    z[i]=a}
  X=sum(z)
  result[m+1,]<-c(m,X)
  if(is.null(n)){if(X==0){break}}else{if(m==n)break}
  }
  if(is.null(n)){return(nrow(result)-1)}else{return(result)}
  }

set.seed(123)
branching(X_o=2000,p=.55)

branching(1,.3,10)
