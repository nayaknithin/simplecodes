#' @title foo
#' @export
confounderPlot <-
function(data, S, x, y, labels, method=c("associationMeasures", "distcor"), returnS=FALSE, plotLegend=TRUE, col, pch, font, cex.text, xlim, ylim, ...){
# data: data frame with variables of interest
# S: similarity matrix; if missing it will be calculated from data
# x: predictor of main interest, for which confounders / collinearities shall be detected
# y: outcome variable
# labels: variable names used for plotting - have to be in corresponding order with columns of data!
# method: method to calculate distances: combination of association measures ("associationMeasures") or distance correlation ("distcor")
# returnS: shall similarity matrix be returned
  
  method <- match.arg(method)
  
  # similarity matrix
  if(missing(S))
    S <- similarity.variables(data, method=method)
  
  if(!identical(names(data), colnames(S)))
    stop("names of 'data' and 'S' must coincide")
  
  if(missing(labels))
    labels <- names(data)
  names(labels) <- names(data)
  
  # color: categorical/continuous variables
  if(missing(col)){
    dc <- sapply(data, data.class)
    col <- ifelse(dc == "numeric", "black", "purple")
  }
  
  # highlight x and y (bold)
  if(missing(font))
    font <- ifelse(names(data) %in% c(x,y), 2, 1)
  
  # graphical parameters
  if(missing(pch)) pch <- 16
  if(missing(cex.text)) cex.text <- 1
  if(missing(xlim)) xlim <- c(0,1.05)
  if(missing(ylim)) ylim <- c(0,1.05)
  
  
  # plot
  par(mar=c(5,4,4,7))
  #plot(0:1, 0:1, type="n", xlab=paste("similarity to", labels[x]), ylab=paste("similarity to", labels[y]), ...)
  #text(x=S[x,], y=S[y,], labels=labels, col=col, font=font, xpd=T)
  plot(S[x,], S[y,], ylim=ylim, xlim=xlim, frame=FALSE, pch=pch, col=col, xlab=paste("similarity to", labels[x]), ylab=paste("similarity to", labels[y]), ...)
  text(x=S[x,], y=S[y,] + 0.03, labels=labels, col=col, font=font, xpd=T, cex=cex.text)
  rect(0, 0, 1, 1, lty=3)
  if(plotLegend)
    legend(1.05, 0.9, c("continuous", "categorical"), col=c("black", "purple"), text.col=c("black", "purple"), xpd=T)
  
  if(returnS)
    return(S)
}
