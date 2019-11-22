#' @title Password generator
#' @author Nithin Nayak
#' @param char number of character yo want in your password defaults to 8
#' @param spl.char set it to TRUE if u need special character in your password. Special characters are 
#' @param key can be given any number which works as set.seed() if u need to create same password next time you run the same code
#' @param ppl number of password you want to create in one go (Works only if key is specified)


Gen.password=function(char=8,spl.char=F,key=NULL,ppl=NULL){
  if(is.null(key)){
    if(spl.char==T){universe=c(letters,LETTERS,0:9,0:9,c("!","@","#","$","%","^","&","*","(",")","_","-","+","="))}else{universe=c(letters,LETTERS,0:9,0:9)}
    a=sample(universe,size=char)
    return(paste(a,collapse=""))
  }else{
    result=data.frame(passwords=0)
    if(spl.char==T){universe=c(letters,LETTERS,0:9,0:9,c("!","@","#","$","%","^","&","*","(",")","_","-","+","="))}else{universe=c(letters,LETTERS,0:9,0:9)}
    set.seed(key)
    t=sample(1:9999999,size=ppl)
    for (i in 1:length(t)) {
      set.seed(t[i])
      a=sample(universe,size=char)
      result[i,]=paste(a,collapse="")
    }
  return(result)}

}
