#' @title foo
#' @export
dist.subjects <-
function(data, weights, alwaysGower=FALSE){
#function(data, type=list()){
# !! to be done: allow also asymmetric binary variables
  
  # variable classes (binary can be any of numeric, factor, ordered, logic)
  #dc <- sapply(data, function(x) ifelse(length(na.omit(unique(x))) == 2, "binary", data.class)

  # if all variables are numeric, use Euclidean distance
  dc <- sapply(data, data.class)
  if(all(dc == "numeric") & !alwaysGower)
    D <- dist(data)
  
  # if not, use Gower's distance with Podani's extension
  else{
    # !! depending on type, define asymmetric binary variables for parameter asym.bin  
  
    # binary variables have to be numeric
    K <- sapply(data[,dc == "factor", drop=FALSE], function(x) length(levels(x)))
    bin <- names(K)[K == 2]
    data[,bin] <- sapply(data[,bin], function(x) as.numeric(x) - 1)
    
    # in case there are logical variables
    if(any(dc == "logical"))
      data[,dc == "logical"] <- sapply(data[,dc == "logical"], as.numeric)

    # in case there are character variables
    if(any(dc == "character")){
      K <- sapply(data[,dc == "character", drop=FALSE], function(x) length(unique(x)))
      bin <- names(K)[K == 2]
      data[,bin] <- sapply(data[,bin], function(x) as.numeric(factor(x)) - 1)
    }
    
    D <- FD::gowdis(x=data, w=weights, ord="metric") # asym.bin=!!
    #D <- sqrt(D)  # gowdis calculates D = 1-S, but we want D = sqrt(1-S) (?)
  }
  return(D)
}
