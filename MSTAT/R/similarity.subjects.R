#' @title foo
#' @export
similarity.subjects <- function(data, weights){
  D <- dist.subjects(data, weights=weights)
  S <- 1 - D
  return(as.matrix(S))
}