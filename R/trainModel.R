rm(list=ls())
source("R/setup.R")
TYPE <- "Rn"
# source("R/buildDataset")
# Get element Target
Y.stack <- list()
YoID_list <- c("Ag","Pb","W", "Hg","Sb","Au","Sn")
# Models
ML_list <- "ranger"# c("svmRadialSigma","ranger","avNNet","glmnet")
Xo <- fread("data/Xo.csv")
Yo <- fread("data/Yo.csv")

tuneLength <- 10
for(YoID in YoID_list){
  for(ML in ML_list){
    Yo_AUX <- Yo[ElementID==YoID]
    DT.train <- Xo[Yo_AUX,on=.(SampleID)]
    DT.train[,Y:=factor(ifelse(Y>0,"Y","N"))]
    # *********************************************************************************
    # Start clusters
    if (!exists("cl") || is.null(cl)) {
      CORES <- detectCores(logical = TRUE)  # Detect the number of logical cores
      cl <- makePSOCKcluster(CORES)         # Create a parallel cluster
      registerDoParallel(cl)                # Register the parallel backend
    }
    model <- train(
      Y~.,
      data=DT.train[,-c("SampleID","ElementID")],
      method=ML,
      trControl = trainControl(
        method = "cv",
        number = 10,
        summaryFunction = twoClassSummary,
        classProbs = TRUE, # IMPORTANT!
        verboseIter = TRUE,
        allowParallel = TRUE),
      tuneLength = tuneLength,
      preProcess=c("nzv"),
      metric="ROC"
    )
    
    # *********************************************************************************
    # Stop and release the cluster after use
    stopCluster(cl)
    registerDoSEQ()  # Ensure that parallel processing is turned off
    rm(cl)           # Remove the cluster object from the environment
    # *********************************************************************************
    saveRDS(model, file = paste0("models/",TYPE,"_model_",ML,"_",YoID,".Rds"))
  }
}



# 










