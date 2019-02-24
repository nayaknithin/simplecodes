#' @title Sample size for Sensitivity Specificity
#' @author Nithin Nayak
#' @param sen The anticipated sensitivity
#' @param spec The anticipated specificity
#' @param d margin of error
#' @param p Prevelance
#' @param sig.level Level of Significance
#' @export

sample.sen.spec=function(sen=NULL,spec=NULL,d,p,sig.level=0.05){
  Z.alpha=qnorm(1-sig.level/2)
  if(!is.null(spec)){if(spec>=1|spec<=0){return(print("Error: Specificity should be between 0 and 1"))}
    n=(Z.alpha**2)*spec*(1-spec)/((d**2)*p)
                     a=n}
  if(!is.null(sen)){if(sen>=1|sen<=0){return(print("Error: Sensitivity should be between 0 and 1"))}
    n=(Z.alpha**2)*sen*(1-sen)/((d**2)*p)}
  if((is.null(sen)|is.null(spec))){return(n)}else{return(cat("Sensitivity :" ,n , "\n",
                                                    "Specificity :",a , "\n"))}
  }
