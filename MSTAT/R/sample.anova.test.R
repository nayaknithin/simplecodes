#' @title Sample size for One-way ANOVA
#' @author Nithin Nayak
#' @param k No. of groups (No.of multiple comparisons will be calculated automatically)
#' @param d The clinically significant difference
#' @param sd The estimate of standard deviation
#' @param w The effect size
#' @param sig.level The significance level defaults to 0.05
#' @param power Power of the test defaults to 0.8(80\%)
#' @param nonpar Should be set as nonpar=T if nonparametric inflation is needed.
#' @param inflate a vector or single value specifying the \% of inflation needed for nonparametric inflation default output is for 15\% and 20\%
#' @description Gives the required minimum sample size for t test
#' @export


sample.anova.test=function(k,d=NA,sd=NA,w=NA,sig.level=0.05,power=0.8,nonpar=F,inflate=c(0.15,0.20)){
  g=dim(combn(k,2))[2];if(is.na(g)){g<-1}
  if(!is.na(w)&!is.na(d)){return(print("Error: Either w or (d and sd) should be input. Not both"))}
  Z.alph=qnorm(1-((sig.level/2)/g));Z.beta=qnorm(power)
  if(is.na(w)){ifelse(is.na(d)|is.na(sd),return(print("Error:d and sd or effect size(w) must be specified")),n<-2*((Z.alph+Z.beta)*sd/d)**2)}
  if(!is.na(w)){n=2*((Z.alph+Z.beta)/w)**2}
  sample.size=array();for(i in 1:length(inflate)){sample.size[i]=n/(1-inflate[i])}
  ifelse(nonpar==T,return(data.frame(inflation=inflate,sample.size)),return(n))}
