#' @title Sample size for estimation of mean
#' @author Nithin Nayak
#' @param d margin of error
#' @param sd Standard deviation
#' @param w effect size
#' @param sig.level Desierd significance level default is 0.05
#' @description This command gives minimum sample size for estimation of mean and proportion
#' @return gives output n: minimum sample size
#' @export


sample.estimate.mean=function(d=NA,sd=NA,w=NA,sig.level=0.05){
  if(!is.na(w)&!is.na(d)){return(print("Error: Either w or (d and sd) should be input. Not both"))}
  Z.alpha=qnorm(1-(sig.level/2))
  if(is.na(w)){ifelse(is.na(d)|is.na(sd),return(print("Error:d and sd or effect size(w) must be specified")),n<-(Z.alpha*sd/d)**2)}
  if(!is.na(w)){n<-(Z.alpha/w)**2};return(n)
}
