#' @title Sample size for comparison of proportion
#' @author Nithin Nayak
#' @param p1&p2 estimate of proportion of first and second group respectively
#' @param d The expected difference in proportion. If not entered d will be calculated as (p2-p1)
#' @param oneside Enter T if the test is onesided.
#' @param sig.level&power Significance level and power of the test respectively.
#' @return Returns the minimum sample size requierd per group
#' @export

sample.prop.test=function(p1,p2,d=NULL,sig.level=0.05,power=0.8,oneside=F){
  if(is.null(d)){d<-(p2-p1)}
Z.alpha=qnorm(if(oneside==F){1-(sig.level/2)}else{1-sig.level});Z.beta=qnorm(power)
n=((Z.alpha*sqrt(2*((p1+p2)/2)*((2-p1-p2)/2))+Z.beta*sqrt((p1*(1-p1)/2)+(p2*(1-p2)/2)))/d)**2;b="per group"
return(cat(n,b))}


