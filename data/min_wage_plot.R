
rm(list=ls()) 

library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
minwg <- read.csv(paste(getwd(),"minwage_clean_ub.csv",sep = "/"),header=TRUE,sep=",")