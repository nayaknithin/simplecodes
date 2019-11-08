#' @title foo
#' @export
dendro.subjects <-
function(data, weights, linkage="ward.D2"){  
#function(data, type=list()){  
# !! to be done: allow also asymmetric binary variables
  
  D.subjects <- dist.subjects(data, weights=weights)
  #D.subjects <- dist.subjects(data, weights=weights, type=type)
  as.dendrogram(hclust(D.subjects, method=linkage))
}
