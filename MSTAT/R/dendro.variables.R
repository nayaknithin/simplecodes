#' @title foo
#' @export
dendro.variables <- function(data, method=c("associationMeasures", "distcor", "ClustOfVar"), linkage="ward.D2", associationFun=association, check.psd=TRUE){
  
  method <- match.arg(method)
  if(method == "associationMeasures"){
    S <- similarity.variables(data, associationFun=associationFun, check.psd=check.psd)
    D.variables <- as.dist(sqrt(1 - S))
    dend <- as.dendrogram(hclust(D.variables, method=linkage))
  }

  else if(method == "distcor"){
    D.variables <- dcor_dist_bc(data)
    dend <- as.dendrogram(hclust(D.variables, method=linkage))
  }

  else if(method == "ClustOfVar"){
     dc <- sapply(data, data.class)
    if(any(dc == "numeric"))
      X.quanti <- data[,dc == "numeric"]
    else
      X.quanti <- NULL
    if(all(dc == "numeric"))
      X.quali <- NULL
    else
      X.quali <- data[,dc != "numeric"]
    dend <- as.dendrogram(ClustOfVar::hclustvar(X.quanti, X.quali))
  }
  
  return(dend)
}
