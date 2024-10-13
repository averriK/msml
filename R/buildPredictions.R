rm(list=ls())
source("R/setup.R")
# Sb,Au,Sn,Ag,Pb,Ag,W,Hg
YoID_list <- c("Ag","Pb","W", "Hg","Sb","Au","Sn")
ML_list <- c("svmRadialSigma","ranger","avNNet","glmnet")

Xi <- fread("data/Xi.csv")
DT.test <- Xi

DATA <- data.table()
for(YoID in YoID_list){
  for(ML in ML_list){
    model <- readRDS(file = paste0("models/model_",ML,"_",YoID,".Rds"))
    AUX <-  DT.test[,.(SampleID,ElementID=YoID,ModelID=ML,predict(model,newdata=.SD, type = "prob"))]
    # Tag Lat/Lon data
    DATA <- rbindlist(list(AUX,DATA),use.names = TRUE) |> unique()
  }
}
YL <- DATA[,-c("N")] |> unique() |> na.omit()
fwrite(YL,"pred/YL.csv")
# Tag Lat/Lon data
IDX <- fread("data/Index.csv") |> unique()
IDX <- IDX[,.(SampleID,UTM.EAST,UTM.NORTH)]
DATA <- dcast(YL, SampleID + ModelID ~ ElementID, value.var = "Y", fun.aggregate = mean)
YW <- IDX[DATA,on=.(SampleID)]
#
YmL <- YL[,.(Y=mean(Y),ModelID="average"),by=.(SampleID,ElementID)]
fwrite(YmL,"pred/YmL.csv")

# Tag Lat/Lon data
IDX <- fread("data/Index.csv") |> unique()
IDX <- IDX[,.(SampleID,UTM.EAST,UTM.NORTH)]
DATA <- dcast(YmL, SampleID + ModelID ~ ElementID, value.var = "Y")
YmW <- IDX[DATA,on=.(SampleID)]
fwrite(YmW,"pred/YmW.csv")


