#' @title Sample size for t test
#' @author Nithin Nayak
#' @param d The clinically significant difference
#' @param sd The estimate of standard deviation
#' @param w The effect size
#' @param paired Should set paired=T if sample size is for paired t test
#' @param sig.level The significance level defaults to 0.05
#' @param power Power of the test defaults to 0.8(80\%)
#' @param nonpar Should be set as nonpar=T if nonparametric inflation is needed.
#' @param oneside Should set oneside=T if the test is onesided
#' @param inflate a vector or single value specifying the \% of inflation needed for nonparametric inflation default output is for 15\% and 20\%
#' @description Gives the required minimum sample size for t test
#' @export

sample.t.test=function(d=NA,sd=NA,w=NA,sig.level=0.05,power=0.8,oneside=F,nonpar=F,inflate=c(0.20,0.15),paired=F){
  if(!is.na(w)&!is.na(d)){return(print("Error: Either w or (d and sd) should be input. Not both"))}
  Z.alph=qnorm(if(oneside==T){1-sig.level}else{1-(sig.level/2)})
  Z.beta=qnorm(power)
  if(is.na(w)){ifelse(is.na(d)|is.na(sd),return(print("Error:d and sd or effect size(w) must be specified")),n<-((Z.alph+Z.beta)*sd/d)**2)}
  if(!is.na(w)){n=((Z.alph+Z.beta)/w)**2}
  if(paired==F){n<-n*2};sample.size=array();for(i in 1:length(inflate)){sample.size[i]=n/(1-inflate[i])}
  ifelse(nonpar==T,return(data.frame(inflation=inflate,sample.size)),return(n))}
