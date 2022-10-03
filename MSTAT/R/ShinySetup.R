#' @title File Structure generator function for Shiny App development
#' @author Nithin Nayak
#' @param path The directory path where you want to setup the Shiny app folder; defaults to your current working directory.
#' @description This function help setup directories and files for new Shiny development.
#' @export

ShinySetup=function(path=getwd()){
  folders=c("R","www")
for (i in 1:length(folders)){
  if(dir.exists(paste0(path,"/",folders[i],sep=""))){cat("\n xxxxxxxxxxxxxxx\n Folder ",folders[i],"already exits...\n Skipping this folder...\n \n")
}else{
    dir.create(paste0(path,"/",folders[i],sep=""))
    cat("\n===============\n Folder",folders[i]," is sucessfully created.")
    }
}

cat("\n+++++++++++++++\n Folder Generation Complete.\n \n Initiating Scripts Generation.")

Scripts=c("global","ui","server","app")
  app="\n shinyApp(ui=ui,server=server)\n"
  ui="\n ui <- fluidpage(\n \n }\n"
  server="server <- funcion(input,output,session){\n \n }\n"
  global="# Packages Required -------------------------------------------------------
\n library(shiny) \n #add additional packages required here...."

  for (i in 1:length(Scripts)) {
    filename=paste0(path,"/",Scripts[i],".R",sep="")
    if(file.exists(filename)){cat("\n xxxxxxxxxxxxxxx\n The File ",paste0(Scripts[i],".R",sep=""),"already exits...\n Skipping this file...\n \n")
      }else{
    sink(file=filename,append=T)
    cat(eval(parse(text=Scripts[i])))
    sink()
    cat("\n===============\n",paste0(Scripts[i],".R",sep="") ,"is sucessfully generated.")
      }
  }

cat("\n=====Process Completed=====\n")
}



