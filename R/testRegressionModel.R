rm(list=ls())
source("R/setup.R")
# list content of models/regression/*.Rds folder
DIR <- list.files("models/regression",pattern="*.Rds")
# Remove extension .Rds
DIR <- gsub("\\.Rds", "", DIR)
# Split by "_". First element is SET, second is ML, third is YoID. Build a data.table
DT <- tstrsplit(DIR,"_",keep=1:3) |> as.data.table()
setnames(DT,old=c("V1","V2","V3"),new=c("SET","ML","YoID"))
SET <- "An"
YoID <- "Ag"
ML <- "glmnet"
# Load model
model <- readRDS(file = paste0("models/regression/",SET,"_",ML,"_",YoID,".Rds"))
