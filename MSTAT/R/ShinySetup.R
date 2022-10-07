#' @title File Structure generator function for Shiny App development
#' @author Nithin Nayak
#' @param path The directory path where you want to setup the Shiny app folder; defaults to your current working directory.
#' @description This function help setup directories and files for new Shiny development.
#' @export
ShinySetup=function(path=getwd()){
UseMethod("ShinySetup")
}



setMethod("ShinySetup","ANY",
          function(path=getwd()){

            cat(cli::rule(
              left = crayon::bold("Creating Sub-Directories"),
              right = Sys.time()
            ),"\n")
            folders=c("R","www","Modules")
            for (i in 1:length(folders)){
              if(dir.exists(paste0(path,"/",folders[i],sep=""))){cat(crayon::red(cli::symbol$cross)," Folder",paste0("~/",folders[i],sep="")," already exist...\n Skipping this folder...\n")
              }else{
                dir.create(paste0(path,"/",folders[i],sep=""))
                cat( crayon::green(cli::symbol$tick)," Folder ",paste0("~/",folders[i],sep="")," is sucessfully created.\n")
              }
            }
            cat(cli::bg_br_green("Folder Generation Complete"),"\n",crayon::bold(paste0("The Created Folders are in:",crayon::cyan(path),sep="")),"\n \n \n")
            cat( cli::rule(
              left = "Creating Scripts",
              right = Sys.time()
            ),"\n")

            Scripts=c("global","ui","server","app")
            app="shinyApp(ui=ui,server=server)\n"
            ui="ui <- fluidPage(\n \n )\n"
            server="server <- funcion(input,output,session){\n \n }\n"
            global="# Packages Required -------------------------------------------------------
\nlibrary(shiny)\n#add additional packages required here....

dir.names=c('R','Modules') \n
for(i in 1:length(dir.names)){
  sapply(
    list.files(paste0('./',dir.names[i],'/',sep=''),
    pattern='*.R$',
    recursive = T),
    source)
}\nrm(list=c('dir.names','i'))\n"

            for (i in 1:length(Scripts)) {
              filename=paste0(path,"/",Scripts[i],".R",sep="")
              if(file.exists(filename)){cat(crayon::red(cli::symbol$cross)," ",paste0(Scripts[i],".R",sep=""),"already exist...\n Skipping this file...\n")
              }else{
                sink(file=filename,append=T)
                cat(eval(parse(text=Scripts[i])))
                sink()
                cat(crayon::green(cli::symbol$tick)," ",paste0(Scripts[i],".R",sep="") ,"is sucessfully generated.\n")
              }
            }

            cat(cli::bg_br_green("Script Generation Complete"),"\n", crayon::bold(paste0("The Created Scripts are in:",crayon::cyan(path),sep="")),"\n \n \n")

          }
          )
