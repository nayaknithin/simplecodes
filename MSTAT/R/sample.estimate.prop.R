#' @title Sample size for estimation of proportion
#' @author Nithin Nayak
#' @param p anticipated proportion
#' @param d margin or error
#' @param e relative precision
#' @param sig.level Desierd significance level default is 0.05
#' @description This command gives minimum sample size for estimation of mean and proportion
#' @return gives output n: minimum sample size
#' @export



sample.estimate.prop=function(p,d=NA,e=NA,sig.level=0.05){Z.alpha=qnorm(1-(sig.level/2))
if(is.na(p)){return("Error: p cannot be missing please enter a value")}
if(!(is.na(d)|is.na(e))){return("Error not both relative precision and margin of error sould be given")}
if(p>1|p<0){return("Error: p should be a value between 0 and 1")}
if(!is.na(d)){if(d>1|d<0){return("Error: d should be a value between 0 and 1")}
  n<-(Z.alpha*sqrt(p*(1-p))/d)**2}else{
  if(e>1|e<0){return("Error: 'relative precision' should be a value between 0 and 1")}
  n<-(Z.alpha*sqrt(p*(1-p))/(e*p))**2
  }
return(n)  }
