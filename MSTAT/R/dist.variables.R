#' @title foo
#' @export
dist.variables <-
function(data, method=c("associationMeasures", "distcor"), associationFun=association, check.psd=TRUE){
# data: data.frame of original data 
# method: method to calculate distances: combination of association measures ("associationMeasures") or distance correlation ("distcor")
# associationFun: function that calculates association measure for each pair of variables in case of method="association"
# check.psd: check if resulting similarity matrix S is positive semi-definite?

  method <- match.arg(method)
  
  if(method == "associationMeasures"){
    S <- similarity.variables(data, associationFun=associationFun, check.psd=check.psd)
    D <- as.dist(sqrt(1 - S))
  }
  
  else if(method == "distcor")
    D <- dcor_dist_bc(data)
  
  return(D)
}
