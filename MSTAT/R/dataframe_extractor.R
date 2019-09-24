#'@title Extracting data frame
#'@author Nithin Nayak
#'@param data the data frame which u want to extract the specific rows
#'@param sampleid The vector indicationg which values you need to extract of a particular column of the data
#'@param id The column number of the variable in the data frame which you want to compare with sample id to get extracted if the values are equal to the values in the sample id
#'
#'@export




extract.data.frame=function(data,sampleid,id){
  extracted.data=data
  for (i in 2:nrow(extracted.data)) {
    extracted.data[i,]=rep(NA,ncol(extracted.data))
  }
  extracted.data=na.omit(extracted.data)
  for(i in 1:length(sampleid)){
    a=data[which(data[,id]==sampleid[i]),]
    extracted.data=rbind(extracted.data,a)
  }
  b=extracted.data[-1,]
  return(b)
}
