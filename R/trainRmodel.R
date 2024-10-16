rm(list=ls())
source("R/setup.R")
LGL <- fread("data/LGL.csv")
YoID_target <- "Au" #LGL$ElementID |> unique()
PATH <- "train/regression"
nADLmin <- 10 # Numero de veces por encima del limite de deteccion
.tuneLength <- 10 
.trControl <-  trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary,
  verboseIter = TRUE,
  allowParallel = TRUE)
.preProcess <-c("scale","center")

SET <- "Rn"# An,Rn
.method <- "glmnet" # c("svmRadialSigma","ranger","avNNet","glmnet")

Xo <- fread(paste0("data/Xo.",SET,".csv"))
Yo <- fread(paste0("data/Yo.",SET,".csv"))
YoID <- YoID_target[1]
for(YoID in YoID_target){
  DT.Y <- Yo[ElementID==YoID ,.(SampleID,Y=nADL)]
  Y <- DT.Y$Y
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
    metric="MAE" 
  )
  
  # *********************************************************************************
  # Stop and release the cluster after use
  stopCluster(cl)
  registerDoSEQ()  # Ensure that parallel processing is turned off
  rm(cl)           # Remove the cluster object from the environment
  # *********************************************************************************
  Yp <- predict(model,newdata=DT.train) |> floor() # We are predicting nADL
  RSS  <- (Y-Yp)%*%(Y-Yp) |> as.double()
  I <- as.numeric(row.names(model$bestTune))
  RMSE <- model$results$RMSE[I]
  R2 <- model$results$Rsquared[I]
  MAE <- model$results$MAE[I]
  FILE <- file.path(PATH,paste0(SET,"_",.method,"_",YoID,".Rds"))
  MODEL <- list(model=model,Y=Y,Yp=Yp,RMSE=RMSE,MAE=MAE,R2=R2,RSS=RSS)
  saveRDS(MODEL, file=FILE)
}










