#' @title BINGO game
#' @author Nithin Nayak
#' @param mat The matrix u want to choose
#' @export


BINGO=function(mat=NULL,random=T){
  if(is.null(mat)){mat=matrix(sample(1:25,replace = F,size = 25),5,5)}
  number=1:ncol(mat)**2;column=array()
  row=array()
  repeat{n=as.numeric(readline(prompt="New number: "))
    if(any(number==n)){number=number[-which(number==n)]}
  mat[mat==n]=NA
         for(i in 1:ncol(mat)){
           column[i]=all(is.na(mat[,i]))
         }
         for(i in 1:nrow(mat)){
           row[i]=all(is.na(mat[i,]))
         }
         a=sum(row)+sum(column)
         print(list(YOUR_MATRIX=mat,AVAILABLE_NUMBERS=number))
         if(a==5){break}
  }
  return(cat("################## \n \n BINGO!!! Congrats \n \n################## \n"))
  }
