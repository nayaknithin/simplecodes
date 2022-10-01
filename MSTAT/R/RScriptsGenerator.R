#' @title R Script Files Generator Function
#' @author Nithin Nayak
#' @param path The directory path where you want to create the R script files. defaults to your current working directory.
#' @param ScriptNames The character vector indicating the names of the R sripts to be generate.
#' @description This function generates R script files in a given path.
#' @export

RscriptGenerator<-function(path=getwd(),ScriptNames=NULL){
  if(is.null(ScriptNames)){stop("ScriptNames cannot be empty.\n Please provide atleast one script name inside 'ScriptNames' argument")}
  ScriptNames<- gsub(" ","",ScriptNames)
  tempgen=function(filename){
    tempile=file(paste0(path,"/",filename,".R",sep=""))
    writeLines("",tempile)
    close(tempile)
  }
  for(i in 1:length(ScriptNames)){
    tempgen(ScriptNames[i])
    cat(paste0("#====================#\n",ScriptNames[i],".R has been generated.\n",sep=""))
    if(i==length(ScriptNames)){cat(" Process Completed......\n Please Check Directory:",path,"to find the generated files.\n")}
  }
}

