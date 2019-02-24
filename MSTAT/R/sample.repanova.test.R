#' @title Sample Size for repeated measures ANOVA
#' @author Nithin Nayak
#' @param d Clinically significant difference between groups
#' @param sd pooled standard deviation
#' @param w Effect size if estimates for sd or dis unknown
#' @param m Number of time points
#' @param rho Estimated correlation between outcomes measured at different time points
#' @param sig.level Significance level
#' @param power Power of the test
#' @param nonpar Set T if nonparametric inflation needed
#' @param inflate vector containing percentage of inflation needed default returns inflation for 15 and 20 \% inflation
#' @export


sample.repanova.test=function(d=NULL,sd=NULL,w=NULL,m,rho,sig.level=0.05,power=0.8,nonpar=F,inflate=c(0.15,0.2)){
  Z.alpha=qnorm(1-sig.level/2);Z.beta=qnorm(power)
  if(!is.null(w)&!is.null(d)){return(print("Error: Either w or (d and sd) should be input. Not both"))}
  if(is.null(w)){ifelse(is.null(d)|is.null(sd),return(print("Error:d and sd or effect size(w) must be specified")),n<-(2*((Z.alph+Z.beta)^2)*(1+((m-1)*rho))*(sd**2)/(d**2)))}
  if(!is.null(w)){n<-(2*((Z.alpha+Z.beta)**2)*(1+((m-1)*rho))/(w**2))}
  sample.size=array();for(i in 1:length(inflate)){sample.size[i]=n/(1-inflate[i])}
  ifelse(nonpar==T,return(data.frame(inflation=inflate,sample.size)),return(n))}

