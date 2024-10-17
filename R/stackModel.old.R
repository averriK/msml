rm(list=ls())
source("R/setup.R")
# Detection Limits
kB <- 0 # 0% of below-detection limit 
kA <- 1 # 100% below-detection limit data
# build dataset with the detection limits fixed 
# (toDo:  replace script by a function)
source("R/buildDataset.R")

# Get element Target
Y.stack <- list()
YoID_list <- c("Au","Sb","Sn","Ag","Pb","W", "Hg")#
# Models
ML <- c("svmRadialSigma","glmnet","ranger","avNNet")


for(YoID in YoID_list){
  Xo <- fread("data/Xo.csv")
  Yo <- fread("data/Yo.csv")
  Yo <- Yo[ElementID==YoID]
  DT.train <- Xo[Yo,on=.(SampleID)]
  DT.train[,Y:=factor(ifelse(Y>0,"Y","N"))]
  # *********************************************************************************
  # Start clusters
  if (!exists("cl") || is.null(cl)) {
    CORES <- detectCores(logical = TRUE)  # Detect the number of logical cores
    cl <- makePSOCKcluster(CORES)         # Create a parallel cluster
    registerDoParallel(cl)                # Register the parallel backend
  }
  modelList <- caretList(
    Y~.,
    data=DT.train[,-c("SampleID","ElementID")],
    methodList=ML,
    trControl = trainControl(
      method = "cv",
      number = 10,
      summaryFunction = twoClassSummary,
      classProbs = TRUE, # IMPORTANT!
      verboseIter = TRUE,
      allowParallel = TRUE),
    tuneLength = 20,
    preProcess=c("nzv"),
    metric="ROC"
  )
  
  
  stackModel <- caretStack(
    all.models=modelList,
    method="glm",
    trControl = trainControl(
      summaryFunction = twoClassSummary,
      classProbs = TRUE, # IMPORTANT!
      verboseIter = TRUE,
      allowParallel = TRUE),
    metric="ROC"
  )
  # *********************************************************************************
  # Stop and release the cluster after use
  stopCluster(cl)
  registerDoSEQ()  # Ensure that parallel processing is turned off
  rm(cl)           # Remove the cluster object from the environment
  # *********************************************************************************
  saveRDS(modelList, file = paste0("models/modelList_",YoID,".Rds"))
  saveRDS(stackModel, file = paste0("models/stackModel_",YoID,".Rds"))
}


# 










