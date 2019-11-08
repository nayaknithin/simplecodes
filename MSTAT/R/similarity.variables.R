#' @title foo
#' @export
similarity.variables <-
function(data, method=c("associationMeasures", "distcor"), associationFun=association, check.psd=TRUE, make.psd=TRUE){
# data: data.frame of original data 
# method: method to calculate distances: combination of association measures ("associationMeasures") or distance correlation ("distcor")
# associationFun: function that calculates association measure for each pair of variables in case of method="association"
# check.psd: check if resulting similarity matrix S is positive semi-definite?
# make.psd: if S is not p.s.d., shall it be transformed to be p.s.d.? (only done if also check.psd=TRUE)
  
  method <- match.arg(method)
  
  #n <- nrow(data)
  p <- ncol(data)
  
  if(method == "associationMeasures"){
  S <- matrix(0, nrow=p, ncol=p)
  for(i in 1:p){
    for(j in 1:p){
      if(i > j){
        # distance = sqrt(1 - association)
        S[i,j] <- associationFun(data[,i], data[,j])
      }
    }
  }
  dimnames(S) <- list(names(data), names(data))
  
  # make it symmetric (since only lower triangle was calculated) 
  S <- S + t(S) 
  diag(S) <- 1
  
  # check if S is p.s.d.
  if(check.psd){
    psd <- all(eigen(S, only.values=TRUE)$values >= 0)
  
    # if S is not p.s.d., get "nearest p.s.d. matrix"
    if(!psd){
      if(!make.psd)
        warning("similarity matrix is not positive semidefinite")
      else{
        S <- Matrix::nearPD(S, keepDiag=TRUE, conv.norm.type="F")$mat
        #warning("similarity matrix was adjusted to be positive semidefinite")
      }
    }
  }
  }
  
  else if(method == "distcor"){
    dcm <- dcormat_bc(data)  
    S <- sign(dcm) * sqrt(abs(dcm))
  }

  return(as.matrix(S))
}
