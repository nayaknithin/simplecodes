#' @title Sample size for estimation
#' @author Nithin Nayak
#' @param estimate The quantity you want to estimate either "mu" or "p"
#' @param sig.level Desierd significance level default is 0.05
#' @description This command gives minimum sample size for estimation of mean and proportion
#' @return gives output n: minimum sample size
#' @export

sample.estimate=function(estimate,sig.level=0.05){
Z.alpha=qnorm(1-(sig.level/2))
if(estimate=="mu"){
  mu=as.numeric(readline("Enter mu: "))
  w=as.numeric(readline("Enter effectsize (d/sigma) if d and sd are availabel press ENTER: "))
  if(is.na(w)){sd=as.numeric(readline("Enter sd: "))
               d=as.numeric(readline("Enter d: "));n=(Z.alpha*sd/d)**2}
  if(!is.na(w)){n=(Z.alpha/w)**2};return(cat("Sample size for estimation of proportion is:",n))
}
if(!estimate=="mu"){
  p=as.numeric(readline("Enter p: "))
  if(is.na(p)){return("Error: p cannot be missing please enter a value")}
  if(p>1|p<0){return("Error: p should be a value between 0 and 1")}
  d=as.numeric(readline("Enter d: "))
  n=(Z.alpha*sqrt(p*(1-p))/d)**2;return(cat("Sample size for estimation of proportion is:",n))
}}


