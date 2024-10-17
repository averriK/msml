rm(list=ls())
source("R/setup.R")
LGL <- fread("data/LGL.csv")
YoID_target <- LGL[!(ElementID %in% c("Cu","Be"))]$ElementID |> unique()
YoID_target <- sample(YoID_target,size=length(YoID_target))
PATH <- "model/C"
nADLmin <- 10 # Numero de veces por encima del limite de deteccion
.tuneLength <- 10 
.trControl <-  trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  allowParallel = TRUE)
.preProcess <-c("scale","center")

SET <- "Rn"# An,Rn
.method <- "ranger" #c("svmRadialSigma","ranger","avNNet") #"glmnet"

Xo <- fread(paste0("data/Xo.",SET,".csv"))
Yo <- fread(paste0("data/Yo.",SET,".csv"))
YoID <- YoID_target[1]
for(YoID in YoID_target){
  FILE <- file.path(PATH,paste0(SET,"_",.method,"_",YoID,".Rds"))
  if(file.exists(FILE)) next
  DT.Y <- Yo[ElementID==YoID ,.(SampleID,Y=factor(ifelse(nADL>=nADLmin,"Y","N")))]
  DT.train <- Xo[DT.Y,on=.(SampleID)][,-c("SampleID","SourceID")]
  
  # *********************************************************************************
  # Start clusters
  if (!exists("cl") || is.null(cl)) {
    CORES <- detectCores(logical = TRUE)  # Detect the number of logical cores
    cl <- makePSOCKcluster(CORES)         # Create a parallel cluster
    registerDoParallel(cl)                # Register the parallel backend
  }
  model <- train(
    Y~.,
    data=DT.train,
    method=.method,
    trControl = .trControl,
    tuneLength = .tuneLength,
    preProcess = .preProcess,
    metric="ROC"
  )
  
  # *********************************************************************************
  # Stop and release the cluster after use
  stopCluster(cl)
  registerDoSEQ()  # Ensure that parallel processing is turned off
  rm(cl)           # Remove the cluster object from the environment
  # *********************************************************************************
  Y <- DT.Y$Y
  Yp <- predict(model,newdata=DT.train) 
  I <- as.numeric(row.names(model$bestTune))
  ROC <- model$results$ROC[I]
  
  MODEL <- list(model=model,Y=Y,Yp=Yp,ROC=ROC)
  saveRDS(MODEL, file=FILE)
}










