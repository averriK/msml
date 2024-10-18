rm(list=ls())
source("R/setup.R")
# list content of models/regression/*.Rds folder
PATH <- "model/nb"
DIR <- list.files(file.path(PATH),pattern="*.Rds")
# Remove extension .Rds
DIR <- gsub("\\.Rds", "", DIR)
# Split by "_". First element is SET, second is ML, third is YoID. Build a data.table
if(length(DIR)>1){
  ModelIndex <- tstrsplit(DIR,"_",keep=1:3) |> as.data.table()
  setnames(ModelIndex,old=c("V1","V2","V3"),new=c("S","M","ID"))
  ModelIndex[,file:=paste0(S,"_",M,"_",ID,".Rds")]
} else stop()

# SET <- "Rn"
DATA <- data.table()
YFILE <- file.path(PATH,"Yp.csv")
if(file.exists(YFILE)) file.remove(YFILE,showWarnings=FALSE)
# SET <- ModelIndex$S[1]
for(SET in unique(ModelIndex$S)){
  
  Xi <- fread(paste0("data/Xi.",SET,".csv"))
  # YoID <- "Ag"
  for(YoID in unique(ModelIndex$ID)){
    # Build a list of files with ModelIndex[oID == YoID_target),file]
    INDEX <-  ModelIndex[ID == YoID & S == SET ]
    FILE_LIST <-INDEX$file
    # Build model list for stacking
    
    # MODEL_LIST <- list()
    FILE <- FILE_LIST[1]
    for(FILE in FILE_LIST){
      O <- readRDS(file.path(PATH,FILE))
      METHOD <- O$model$method
      sprintf("File: %s  Model:%s Element:%s\n",FILE,METHOD,YoID) %>% cat()
      
      P <- predict(O$model,newdata=Xi,type="prob")
      DATA <-Xi[,.(SampleID,ElementID=YoID,DataID=SET,ModelID=METHOD,FileID=FILE,"P[C>10*DL]"= P$Y,ROC=O$ROC)]
      # MODEL_LIST[[METHOD]] <- O$model
      fwrite(DATA,YFILE,append = TRUE)
    }
  }
  
}





# 
# 
# for(SET in unique(ModelIndex$SET)){
#   Xi <- fread(paste0("data/Xi.",SET,".csv"))
#   for(YoID in unique(ModelIndex$YoID)){
#     model_list <- list()
#     
#     YoID_target <-YoID
#     SET_target <- SET
#     ML_target <- ModelIndex[YoID == YoID_target & SET== SET_target]$ML |> unique()
#     
#     
#     for(ML in ML_target){
#       MID <- paste0(SET,"_",ML,"_",YoID,".Rds")
#       FILE <- file.path(PATH,MID)
#      
#       
#       if(!file.exists(FILE)) next
#       MDL <- readRDS(file = FILE)
#       model <- MDL$model
#       browser()
#       P <- predict(model,newdata=Xi,type="prob") #|> as.data.table()
#       P[]
#       AUX <- Xi[,.(SampleID,ElementID=YoID,ModelID=ML,DataID=SET,"P[C>10*DL]"= P$Y)]
#       
#       I <- as.numeric(row.names(model$bestTune))
#       ROC <- model$results$ROC[I]
#     }
#     
#     # stackModel <- caretStack(
#     #   all.models=modelList,
#     #   method="glm",
#     #   trControl = trainControl(
#     #     summaryFunction = twoClassSummary,
#     #     classProbs = TRUE, # IMPORTANT!
#     #     verboseIter = TRUE,
#     #     allowParallel = TRUE),
#     #   metric="ROC"
#     # )
#     
#   }
# }

# 
# 
# SET <- "Rn"
# YoID <- "Au"
# ML <- "ranger"
# # Load model
# FILE <- file.path(PATH,paste0(SET,"_",ML,"_",YoID,".Rds"))
# MDL <- readRDS(file = FILE)
# 
# predict()
