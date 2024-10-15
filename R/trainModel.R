rm(list=ls())
source("R/setup.R")
SET_list <- c("Rn","An")
# source("R/buildDataset")
# Get element Target
Y.stack <- list()
YoID_list <- Yo[!(ElementID%in%c("MnO","MgO","P2O5","Cr2O3","CaO","Al2O3","Fe2O3","SiO2","K2O","TiO2", "LOI"   ,"Na2O","Total_C", "Total_S"   ))]$ElementID |> unique()
# Models
ML_list <- c("ranger","glmnet")# c("svmRadialSigma","ranger","avNNet","glmnet")


tuneLength <- 20
trControl <-  trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  allowParallel = TRUE)
for(SET in SET_list){
  Xo <- fread(paste0("data/Xo.",SET,".csv"))
  Yo <- fread(paste0("data/Yo.",SET,".csv"))
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
        trControl = trControl,
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
      saveRDS(model, file = paste0("models/",SET,"_",ML,"_",YoID,".Rds"))
    }
  }
}




# 










