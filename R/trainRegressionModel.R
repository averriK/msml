rm(list=ls())
source("R/setup.R")
LGL <- fread("data/LGL.csv")
YoID_target <- "Au" #LGL[!(ElementID%in%c("Sum","MnO","MgO","P2O5","Cr2O3","CaO","Al2O3","Fe2O3","SiO2","K2O","TiO2", "LOI"   ,"Na2O","Total_C", "Total_S"   ))]$ElementID |> unique()
PATH <- "train/regression"
.tuneLength <- 10 
.trControl <-  trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary,
  verboseIter = TRUE,
  allowParallel = TRUE)
SET <- "Rn"# An,Rn
ML <- "ranger" # c("svmRadialSigma","ranger","avNNet","glmnet")

Xo <- fread(paste0("data/Xo.",SET,".csv"))
Yo <- fread(paste0("data/Yo.",SET,".csv"))
YoID <- YoID_target[1]
for(YoID in YoID_target){
  Y <- Yo[ElementID==YoID]
  DT.train <- Xo[Y,on=.(SampleID)][,-c("SampleID","ElementID","SourceID")]
  .preProcess <-c("scale","zv","YeoJohnson")
  
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
    method=ML,
    trControl = .trControl,
    tuneLength = .tuneLength,
    preProcess=.preProcess,
    metric="MAE" 
  )
  
  # *********************************************************************************
  # Stop and release the cluster after use
  stopCluster(cl)
  registerDoSEQ()  # Ensure that parallel processing is turned off
  rm(cl)           # Remove the cluster object from the environment
  # *********************************************************************************
  Yp <- predict(model,newdata=DT.train)
  RSS  <- (Y-Yp)%*%(Y-Yp) |> as.double()
  I <- as.numeric(row.names(model$bestTune))
  RMSE <- model$results$RMSE[I]
  R2 <- model$results$Rsquared[I]
  MAE <- model$results$MAE[I]
  FILE <- file.path(PATH,paste0(SET,"_",ML,"_",YoID,".Rds"))
  MODEL <- list(model=model,Y=Y,Yp=Yp,RMSE=RMSE,MAE=MAE,R2=R2,RSS=RSS)
  saveRDS(MODEL, file=FILE)
}










