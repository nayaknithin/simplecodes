#' @title package installation
#' @author Nithin Nayak
#' @param .  Nothing should be inputted
#' @export


install.all.packages=function(){
x=c("lme4","foreign","coin","npsm","GLSME","gee","geepack","MuMIn","survival","survminer","cutpointr","caTools","ROSE","readxl",
    "FSA","nlme","markovchain","rgl","userfriendlyscience","car","ggplot2",
    "PMCMR","DescTools","haven","vcd","HH","lawstat","Rcmdr","Bolstad","RcmdrMisc","multcomp","RcmdrPlugin.EZR","ResourceSelection")
install.packages(x)
}



