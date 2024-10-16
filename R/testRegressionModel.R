rm(list=ls())
source("R/setup.R")
# list content of models/regression/*.Rds folder
PATH <- "train/regression"
DIR <- list.files(file.path(PATH),pattern="*.Rds")
# Remove extension .Rds
DIR <- gsub("\\.Rds", "", DIR)
# Split by "_". First element is SET, second is ML, third is YoID. Build a data.table
if(length(DIR)>1){
  ModelIndex <- tstrsplit(DIR,"_",keep=1:3) |> as.data.table()
  setnames(ModelIndex,old=c("V1","V2","V3"),new=c("SET","ML","YoID"))
}

SET <- "Rn"
YoID <- "Au"
ML <- "ranger"
# Load model
FILE <- file.path(PATH,paste0(SET,"_",ML,"_",YoID,".Rds"))
model <- readRDS(file = FILE)

I <- as.numeric(row.names(model$bestTune))
model$results$RMSE[I]
model$results$Rsquared[I]
model$results$MAE[I]
