#' @title foo
#' @export
# variable distance matrix based on distance correlation - by Dominic Edelmann

dcormat_bc <- function(X)
{
  p_sample <- ncol(X)
  n <- nrow(X)
  
  ## Distanzmatrizen werden erstellt.
  distmats <- lapply(1:p_sample, function(i) as.matrix(dist.subjects(as.data.frame(X[,i]))))
  
  # Means werden berechnet
  means <- lapply(1:p_sample, function(i) mean(distmats[[i]], na.rm=TRUE))
  
  # rowmeans werden berechnet
  rowmats <- lapply(1:p_sample, function(i)
    matrix(rep(rowMeans(distmats[[i]], na.rm=TRUE),n),ncol=n)
  )
  
  ## Zentralierte Distanzmatrizen werden erstellt 
  centmats <- lapply(1:p_sample, function(i)
    return(distmats[[i]] - rowmats[[i]] - t(rowmats[[i]]) + means[[i]])
  )
  
  centmatsnew <- lapply(1:p_sample, function(i)
  {
    matr <- n/(n-1)*(centmats[[i]]-(1/n)*distmats[[i]])
    diag(matr) <- n/(n-1)*(rowmats[[i]][,1]-means[[i]])
    return(matr)
  })
  
  ## Distanzkorrelationen werden berechnet
  NAs <- apply(X, 2, function(x) any(is.na(x)))
  centmatsnew_comp <- centmatsnew
  sim <- outer( 
    1:p_sample, 
    1:p_sample,
    Vectorize(function(i,j){
      if(NAs[i] | NAs[j]){
        if(NAs[i])
          nai <- apply(centmatsnew[[i]], 1, function(x) sum(is.na(x)) == n-1)
        else
          nai <- rep(FALSE, n)
        if(NAs[j])
          naj <- apply(centmatsnew[[j]], 1, function(x) sum(is.na(x)) == n-1)
        else
          naj <- rep(FALSE, n) 
        na  <- nai | naj 
        centmatsnew_comp[[i]] <- centmatsnew[[i]][!na, !na]
        centmatsnew_comp[[j]] <- centmatsnew[[j]][!na, !na]
      }
      mean(centmatsnew_comp[[i]] * centmatsnew_comp[[j]]) / sqrt((mean(centmatsnew_comp[[i]]^2) * mean(centmatsnew_comp[[j]]^2)))
    }))
  
  ## add variable names
  dimnames(sim) <- list(colnames(X), colnames(X))
  
  sim
}  



dcor_dist_bc <- function(X)
{
  dcm <- dcormat_bc(X)
  as.dist((1-sign(dcm)*sqrt(abs(dcm))))
}

