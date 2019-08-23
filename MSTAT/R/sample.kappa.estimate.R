#'@title Sample size for Kappa
#'@author Nithin Nayak
#'@param k Anticipated kappa value
#'@param d margin of error
#'@param pi proportion of disagreement
#'@return Returns the minimum sample size requierd for estimation for kappa coeffitient
#'@export

sample.kappa=function(k,d,pi,sig.level=0.05){
  Z.alpha=qnorm(1-sig.level/2)
  wk=2*d
  a=4*(1-k)/(wk**2)
  a=a*(Z.alpha**2)
  b=((1-k)*(1-(2*k)))+(k*(2-k)/(2*pi*(1-pi)))
  return(a*b)
}
