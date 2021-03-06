#' @title Cronbach's alpha
#' @author Nithin Nayak
#' @param data The Data frame
#' @param f The colum number of the first item
#' @param n The number of items
#' @export


cronbach.alpha=function(data,f,n){result=list()
#Alpha
x=as.data.frame(data[,f:(f+n-1)]);m=array();a1=array();a2=array()
for(i in 1:ncol(x)){m[i]=var(x[,i])
a1[i]=mean(x[,i]);a2[i]=round(sd(x[,i]),3)};t=array()
for(i in 1:nrow(x)){t[i]=sum(x[i,])};result[[2]]=data.frame(item=1:ncol(x),Mean=a1,SD=a2)#Stats
s=sum(m)/var(t);s=1-s
k=ncol(x)/(ncol(x)-1);alpha=s*k
result[[1]]=matrix(c(alpha,nrow(x)),1,2,dimnames = list(NULL,c("Cronbach Alpha","items")))
deleted=data.frame(deleted_item=0,Mean=0,Variance=0,Correlation=0,Cronbach.alpha=0)
for(j in 1:ncol(x)){
  d=x[,-j];m=array();c.sum=array()
  for(i in 1:ncol(d)){
    m[i]=var(d[,i]);c.sum[i]=sum(d[,i])};t=array()
    for(i in 1:nrow(d)){
      t[i]=sum(d[i,])
    };mean.del=sum(c.sum)/nrow(d);v=round(var(t),3);co=cor(x[,j],t)
    s=sum(m)/var(t);s=1-s
    k=ncol(d)/(ncol(d)-1)
    deleted[j,]=c(j,mean.del,v,co,s*k)
}
result[[3]]=deleted;names(result)=c("Reliability Statistics","Item Statistics","After deleting ith item")
return(result)
}
