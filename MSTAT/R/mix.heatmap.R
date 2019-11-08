#' @title mix heatmap
#' @export


mix.heatmap <-
#function(data, D.subjects, D.variables, dend.subjects, dend.variables, type=list(), 
# !! to be done: allow also asymmetric binary variables
function(data, D.subjects, D.variables, dend.subjects, dend.variables,  
                        varweights,
                        dist.variables.method=c("associationMeasures","distcor", "ClustOfVar"), linkage="ward.D2", associationFun=association,
                        rowlab, rowmar=3, lab.cex=1.5, ColSideColors, RowSideColors,
                        col.cont=marray::maPalette(low="lightblue", high="darkblue", k=50),
                        cont.fixed.range=FALSE, cont.range,
                        col.ord=list(low="lightgreen", high="darkgreen"),
                        col.cat=c("indianred1","darkred","orangered","orange","palevioletred1","violetred4","red3","indianred4"),
                        #col.cat=c("darkorange","darkred","thistle","cornflowerblue","olivedrab","darkgrey","purple4","indianred","yellow2","darkseagreen4"),
                        legend.colbar, legend.rowbar, legend.mat=FALSE, legend.cex=1, legend.srt=0){
# data: data frame where columns are variables (of different data types) and rows are observations (subjects, samples)
# D.subjects, D.variables: the already calculated distance matrices (class 'dissimilarity') for subjects and variables can be given; 
  # if missing, they will be calculated; if set to NULL, no clustering is done and original order in 'data' will be preserved
# dend.subjects, dend.variables: dendrograms for subects and variables can be given;
  # then no distances will be calculated and also D.subjects/D.variables will be ignored
# varweights: optional vector of variable weights, used for calculating Gower distances btw. subjects
# dist.variables.method: distance can be based on 1-sqrt(associationmeasures) or distance correlation or ClustOfVar approach
# linkage: agglomeration method used for hierarchical clustering
# associationFun: function calculating association coefficients between variables (only used when dist.variables.method="associationMeasures")
# rowlab: variable labels; if missing, colnames of data are used
# rowmar: margin for variable labels  
# lab.cex: size of row labels
# ColSideColors, RowSideColors: color bars can be added on top / to the left (just one bar each)
# col.cont: color palette for continuous variables
# cont.fixed.range: if FALSE, color range of each continuous variable is defined by respective individual variable's range; 
  #  if TRUE, all continuous variables are assumed to have similar range; extreme colors correspond to max/min values over all continuous variables and are applied to all of them equally
# cont.range: if cont.fixed.range=TRUE, extreme value limits for coloring continuous variables can be specified;
  #  if missing, extreme values are taken from the data; ignored if cont.fixed.range=FALSE
# col.ord: list with colors for lowest and highest category of ordinal variables -> color palette will be created based on the number of categories
# col.cat: vector of colors for categorical variables
# legend.colbar / legend.rowbar: class labels for subject/variable groups defined by ColSideColors/RowSideColors
# legend.mat: shall legend matrix for heatmap be shown
# legend.cex: size of legend text
# legend.srt: legend matrix label string rotation in degrees; i.e. legend.srt=90 produces vertical labels
  
  if(ncol(data) > 200)
    stop("the heatmap is currently only available for a maximum of 200 variables")
  
  # number of subjects and variables
  p <- ncol(data)
  n <- nrow(data)

  ## subjects
  # if dendrogram is given, order subjects by its labels
  if(!missing(dend.subjects)){
    if(!all(labels(dend.subjects) %in% rownames(data)))
      stop("labels of dend.subjects have to correspond to rownames of data")
    o.subjects <- labels(dend.subjects)
    plotdend.sub <- TRUE
  }

  # if neither dendrogram nor dist matrix is given, calculate distance matrix for subjects 
  else if(missing(D.subjects)){
    D.subjects <- dist.subjects(data, weights=varweights)
    #D.subjects <- dist.subjects(data, type=type)
    dend.subjects <- as.dendrogram(hclust(D.subjects, method=linkage))
    o.subjects <- labels(dend.subjects)
    plotdend.sub <- TRUE
  }
    
  # if D.subjects is set to NULL, keep order of data
  else if(is.null(D.subjects)){
    o.subjects <- 1:n
    plotdend.sub <- FALSE
  }
  
  # if D.subjects is specified
  else{
    dend.subjects <- as.dendrogram(hclust(D.subjects, method=linkage))
    o.subjects <- labels(dend.subjects)
    plotdend.sub <- TRUE
  }
  
  ## variables
  # if dendrogram is given, order variables by its labels
  if(!missing(dend.variables)){
    if(!all(labels(dend.variables) %in% names(data)))
      stop("labels of dend.variables have to correspond to colnames of data")
    o.variables <- labels(dend.variables)
    plotdend.var <- TRUE
  }

  # if neither dendrogram nor dist matrix is given, calculate distance matrix for variables 
  else if(missing(D.variables)){
    dist.variables.method <- match.arg(dist.variables.method)
    if(dist.variables.method == "associationMeasures" | dist.variables.method == "distcor"){
      D.variables <- dist.variables(data, method=dist.variables.method, associationFun=associationFun)
      dend.variables <- as.dendrogram(hclust(D.variables, method=linkage))
    }
    
    else if(dist.variables.method == "ClustOfVar"){
      dc <- sapply(data, data.class)
      if(any(dc == "numeric"))
        X.quanti <- data[,dc == "numeric"]
      else
        X.quanti <- NULL
      if(all(dc == "numeric"))
        X.quali <- NULL
      else
        X.quali <- data[,dc != "numeric"]
      dend.variables <- as.dendrogram(ClustOfVar::hclustvar(X.quanti, X.quali))
    }
    
    o.variables <- labels(dend.variables)
    plotdend.var <- TRUE
  }
  
  # if D.variables is set to NULL, keep order of data
  else if(is.null(D.variables)){
    o.variables <- 1:p
    plotdend.var <- FALSE
  }
  
  # if D.variables is specified
  else{
    dend.variables <- as.dendrogram(hclust(D.variables, method=linkage))
    o.variables <- labels(dend.variables)
    plotdend.var <- TRUE
  }
  
  # order rows and columns for plotting
  data.plot <- data.frame(data[o.subjects, rev(o.variables)])
  if(!missing(ColSideColors)){
    names(ColSideColors) <- rownames(data)
    ColSideColors2 <- ColSideColors[o.subjects]
  }
  if(!missing(RowSideColors)){
    names(RowSideColors) <- names(data)
    RowSideColors2 <- RowSideColors[rev(o.variables)]
  }
  
  # plot layout matrix: 1: subjects dendrogram, 2: column color bar, 3: variables dendrogram, 4: row color bar, 
  #                     5-(p+4): heatmap rows, p+5: leave some empty space 
  #                     p+6: legend for column/row color bar (if legend.colbar and/or legend.rowbar is specified)
  #                     p+7: legend matrix for heatmap (if legend.mat=TRUE)
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  mardend1 <- ifelse(!plotdend.sub, 0.3, 1)  # space for column dendrogram or nothing
  mardend2 <- ifelse(missing(ColSideColors), .1, 0.6)  # space for column color bar or nothing
  mardend3 <- ifelse(!plotdend.var, 0.3, 1)  # space for row dendrogram or nothing
  mardend4 <- ifelse(missing(RowSideColors), .05, 0.3)  # space for row color bar or nothing
  
  if(missing(legend.colbar) & missing(legend.rowbar)){
    layoutmat <- matrix(c(0, 0, rep(3, p), rep(0,3), rep(4,p), 0:2, 5:(p+5)), ncol=3)   ## maximal 200 rows allowed in layout!
    if(legend.mat){
      layoutmat <- rbind(layoutmat, max(layoutmat) + 1)
      layout(layoutmat, widths=c(mardend3, mardend4, 7), heights=c(mardend1, mardend2, rep(.25, p+1), 1))  
    }
    layout(layoutmat, widths=c(mardend3, mardend4, 7), heights=c(mardend1, mardend2, rep(.25, p+1)))  
  }
  else{
    layoutmat <- matrix(c(0, 0, rep(3, p), rep(0,3), rep(4,p), 0:2, 5:(p+5), rep(p+6, p+3)), ncol=4)  
    if(legend.mat){
      layoutmat <- rbind(layoutmat, max(layoutmat) + 1)
      layout(layoutmat, widths=c(mardend3, mardend4, 7, 2), heights=c(mardend1, mardend2, rep(.25, p+1), 1))  
    }
    layout(layoutmat, widths=c(mardend3, mardend4, 7, 2), heights=c(mardend1, mardend2, rep(.25, p+1)))      
  }
  
  # subjects dendrogram / nothing
  if(!plotdend.sub){
      par(mar=c(0, 1, 0, rowmar))
      plot(rep(1,n), type="n", axes=F, xlab="", ylab="")
  }
  else{
    par(mar=c(0, 1, 1, rowmar))
    plot(dend.subjects, axes=FALSE, leaflab="none", xaxs="i")
  }
  
  # column color bar / nothing
  if(missing(ColSideColors)){
    par(mar=c(0, 1, 0, rowmar))
    plot(rep(1,n), type="n", axes=FALSE, xlab="", ylab="")
  }
  else{
      par(mar=c(1, 1, 1, rowmar))
      addfac(as.matrix(ColSideColors2))
  }
    
  # variables dendrogram / nothing
  if(!plotdend.var){
    par(mar=c(0, 0, 0, 0))
    plot(rep(1,p), type="n", axes=F, xlab="", ylab="")
  }
  else{
    par(mar=c(0, 1, 0, 0))
    plot(dend.variables, axes=FALSE, leaflab="none", yaxs="i", horiz=TRUE)
  }
  
  # row color bar / nothing
  if(missing(RowSideColors)){
    par(mar=c(0, 0, 0, 0))
    plot(rep(1,p), type="n", axes=F, xlab="", ylab="")
  }
  else{
    par(mar=c(0, 1, 0, 0))
    addfac(t(as.matrix(RowSideColors2)))
  }  
  
  # row (=variable) labels
  if(missing(rowlab))
    rowlab <- names(data.plot)
  else{
    names(rowlab) <- names(data)
    rowlab <- rowlab[rev(o.variables)]
  }

  # make sure all binary variables are factors
  dc <- sapply(data.plot, function(x) 
    ifelse(length(na.omit(unique(x))) == 2, "binary", data.class(x)))
  data.plot[,dc == "binary"] <- lapply(data.plot[,dc == "binary", drop=FALSE], factor)
  
  # if continuous variables shall have same color range 
  n.col <- length(col.cont)
  if(cont.fixed.range){
    if(missing(cont.range))
      cont.range <- quantile(data.plot[, dc == "numeric", drop=FALSE], c(0.025, 0.975), na.rm=TRUE)
    colbins <- seq(cont.range[1], cont.range[2], length.out=n.col)
  }

  # heatmap
  for(i in 1:p){
    v.i <- data.plot[,i]
    #dc <- ifelse(length(na.omit(unique(v.i))) == 2, "binary", data.class(v.i))
    #dc <- data.class(v.i)
    N <- rowlab[i]
    par(mar=c(0, 1, 0, rowmar))
    
    # continuous -> heatmap colors
    if(dc[i] == "numeric"){
      mincol <- 1
      maxcol <- n.col

      # if color range shall be same for all cont. variables, limit col.cont w.r.t. overall extreme values
      if(cont.fixed.range){
        qu <- quantile(v.i, c(0.025, 0.975), na.rm=TRUE)
        mincol <- max(which(colbins <= qu[1]), 1)
        maxcol <- min(which(colbins > qu[2]), n.col)
      }
      heat(t(as.matrix(v.i)), ylab=N, cols=col.cont[mincol:maxcol], cex=lab.cex)
    }
    
    # ordinal -> green scale colors
    else if(dc[i] == "ordered"){
      col.o <- fac2col(v.i, cols=maPalette(low=col.ord$low, high=col.ord$high, k=length(levels(v.i))))
      addfac(as.matrix(col.o), ylab=N, cex=lab.cex)
    }
    
    # categorical -> category colors
    else {
      col.c <- fac2col(v.i, cols=col.cat)
      addfac(as.matrix(col.c), ylab=N, cex=lab.cex)
    }
  }
  
  # leave some empty space
  plot(1, type="n", frame=F, axes=F, xlab="", ylab="")
  
  # legends for column/row color bars
  if(!missing(legend.colbar) & !missing(legend.rowbar)){
    par(mar=c(0,0,0,0))
    nc <- length(unique(ColSideColors))
    nr <- length(unique(RowSideColors))
    plot(c(0,0), c(0, nc+nr), type="n", axes=FALSE, xlab="", ylab="")
    legend(x=-.9, y=nc+nr, legend=legend.colbar, fill=sort(unique(ColSideColors)), border="black", cex=1.5, bty="n")
    legend(x=-.9, y=nr, legend=legend.rowbar, fill=sort(unique(RowSideColors)), border="black", cex=1.5, bty="n")    
  }
  
  else if(!missing(legend.colbar)){
    par(mar=c(0,0,0,0))
    plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
    legend(x=-.9, y=.9, legend=legend.colbar, fill=sort(unique(ColSideColors)), border="black", cex=1.5, bty="n")
  }

  else if(!missing(legend.rowbar)){
    par(mar=c(0,0,0,0))
    plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
    legend(x=-.9, y=.9, legend=legend.rowbar, fill=sort(unique(RowSideColors)), border="black", cex=1.5, bty="n")
  }
  
  # legend matrix for heatmap
  if(legend.mat){
    if(cont.fixed.range) 
      cont.range <- signif(cont.range, 2)
    else 
      cont.range <- c("min","max")
    legendmat(data.plot, Names=rowlab, col.cont, cont.range, col.ord, col.cat, lab.cex=legend.cex, lab.srt=legend.srt)
  }
  
  # reset plotting parameters
  par(def.par)
}
