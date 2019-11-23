#' @title The Cow-Bull game
#' @author Nithin Nayak
#' @param num your number. If not specified computer will generate random number. If u r specifying your own number it is advisable to save it in a variable the press "cntrl+l" to clear console then call function and input that variable in the function. i.e if u save number x=1234 then press "cntrl+l" then give CowBull.game(x) and let your friend guess your number.
#' @export


CowBull.game=function(num=NULL){
  if(is.null(num)){num=as.character(sample(0:9,4,replace = F))}else{num=as.character(num)
  num=strsplit(num,"")[[1]]}
 n=0
 bull=0
 result=data.frame(Guess=0,...=0,Cow.Bull=0)
 repeat{n=n+1
 guess=readline("Enter your number or Q to quit: ")
 if(guess=="Q"|guess=="q"){break}else{guess=strsplit(guess,"")[[1]]
 if(length(guess)>=5|length(guess)<=3){print("The Number should be 4 digits" )
  ;n=n-1; next}else{charcheck=suppressWarnings(as.numeric(guess))
  if(!sum(is.na(charcheck))==0){print("Wrong character inserted Please insert only numbers");n=n-1; next}
  }
 }
 check=matrix(guess, nrow = length(guess), ncol = length(guess),byrow = T)==num
 cowcheck=check
 diag(cowcheck)<-rep(F,4)
 cow=sum(colSums(cowcheck))
 bull=sum(diag(check))
 m=c(bull,"B",cow,"C")
 result[n,]=c(paste(guess,collapse=""),"  ",paste(m,collapse = " "))
 print(result)
 cat("\n")
 if(bull==4)break}
 if(bull==4){return(cat("You Win!! \n
                        No. of Trials = ",n,"\n"))}else{cat("You Didn't Complete the game \n",
                                                                 "You Quitted after trying",n-1,"times\n")}
 }
