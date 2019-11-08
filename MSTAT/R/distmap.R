#' @title foo
#' @export
distmap <-
function(data, what=c("subjects", "variables"), variables.method=c("associationMeasures", "distcor"), varweights, linkage="ward.D2", reorderdend, col, ...){
#function(data, what=c("subjects", "variables"), type=list(), col, ...){
  # !! to be done: allow also asymmetric binary variables

  variables.method <- match.arg(variables.method)
  
  # if data is original data.frame, calculate similarity matrix if missing
  if(data.class(data) == "data.frame"){
    what <- match.arg(what)
    if(what == "subjects"){
      D <- dist.subjects(data, weights=varweights)
      S <- 1 - as.matrix(D)
    }
    
    else if(what == "variables"){
      S <- similarity.variables(data, method=variables.method)
      
      if(variables.method == "associationMeasures")
        D <- as.dist(sqrt(1 - S))
      else if(variables.method == "distcor")
        D <- as.dist(1 - S)
    }
  }
  
  # else if data is similarity matrix
  else if(data.class(data) == "matrix" & isSymmetric(data)){
    S <- data
    D <- as.dist(1 - S)
  }
  
  else
    stop("'data' has to be a data.frame with the original data or a similarity matrix")
  
  # corresponding dendrogram
  dendro <- as.dendrogram(hclust(D, method=linkage))
    
  # reorder dendrogram if wanted
  if(!missing(reorderdend))
    dendro <- stats::reorder(dendro, reorderdend)
  
  # graphical parameters
  if(missing(col))
    col <- marray::maPalette(low="#F7FBFF", mid="#6BAED6", high="#08306B", 30)

  # distogram
  gplots::heatmap.2(S, Rowv=dendro, Colv=dendro, trace="none", density.info="none", col=col, ...)
}
