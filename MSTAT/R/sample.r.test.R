#' @title sample.r.test
#' @author Nithin Nayak
#' @param r Anticipated correlation coefficient. Defaults to 0.3
#' @param sig.level Significance level
#' @param power Requierd power of the test
#' @param nonpar Should be given as T if non parametric inflation is requierd
#' @param inflate Vector or single value indicting the \% of inflation
#' @export



sample.r.test=function(r=0.3,sig.level=0.05,power=0.8,nonpar=F,inflate=c(0.15,0.2)){
  Z.alpha=qnorm(1-(sig.level/2));Z.beta=qnorm(power)
  n=(((Z.alpha+Z.beta)/(0.5*log((1+r)/(1-r))))**2)+3
  sample.size=array();for(i in 1:length(inflate)){sample.size[i]=n/(1-inflate[i])}
  ifelse(nonpar==T,return(data.frame(inflation=inflate,sample.size)),return(n))}


