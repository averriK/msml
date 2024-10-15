rm(list=ls())
source("R/setup.R")
SET_list <- c("Rn","An")
# source("R/buildDataset")
# Get element Target
Y.stack <- list()
LGL <- fread("data/LGL.csv")
YoID_list <- LGL[!(ElementID%in%c("Sum","MnO","MgO","P2O5","Cr2O3","CaO","Al2O3","Fe2O3","SiO2","K2O","TiO2", "LOI"   ,"Na2O","Total_C", "Total_S"   ))]$ElementID |> unique()
# Models
ML_list <-"glmnet"# c("svmRadialSigma","ranger","avNNet","glmnet")

# Start clusters
if (!exists("cl") || is.null(cl)) {
  CORES <- detectCores(logical = TRUE)  # Detect the number of logical cores
  cl <- makePSOCKcluster(CORES)         # Create a parallel cluster
  registerDoParallel(cl)                # Register the parallel backend
}

tuneLength <- 19 
trControl <-  trainControl(
  method = "cv",
  number = 10,
  # summaryFunction = twoClassSummary, # ONLY for CLASSIFICATION
  summaryFunction = defaultSummary,
  # classProbs = TRUE, # IMPORTANT! # ONLY for CLASSIFICATION 
  verboseIter = TRUE,
  allowParallel = TRUE)
SET <- SET_list[1]
for(SET in SET_list){
  Xo <- fread(paste0("data/Xo.",SET,".csv"))
  Yo <- fread(paste0("data/Yo.",SET,".csv"))
  YoID <- YoID_list[2]
  for(YoID in YoID_list){
    Yo_AUX <- Yo[ElementID==YoID]
    DT.train <- Xo[Yo_AUX,on=.(SampleID)][,-c("SampleID","ElementID","SourceID")]
    # DT.train[,Y:=factor(ifelse(Y>0,"Y","N"))] # ONLY for CLASSIFICATION
    
    for(ML in ML_list){
      
      # *********************************************************************************
      
      model <- train(
        Y~.,
        data=DT.train,
        method=ML,
        trControl = trControl,
        tuneLength = tuneLength,
        preProcess=c("nzv"),
        metric="MAE" 
        # metric="ROC" # ONLY for CLASSIFICATION
      )
      
      # *********************************************************************************
      # Stop and release the cluster after use
      stopCluster(cl)
      registerDoSEQ()  # Ensure that parallel processing is turned off
      rm(cl)           # Remove the cluster object from the environment
      # *********************************************************************************
      saveRDS(model, file = paste0("models/",SET,"_",ML,"_",YoID,".Rds"))
    }
  }
}




# 










