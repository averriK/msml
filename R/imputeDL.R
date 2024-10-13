rm(list=ls())
source("R/setup.R")

# Get lithogeology data
LGL <- fread("data/LGL.csv")
SML <- fread("data/SML.csv")

# Aggregate data. Remove isDrillhole by averaging parameters 
DATA <- LGL[,.(ElementValue=mean(ElementValue)),by=.(SampleID,ElementID,BDL,ADL)] |> unique(by=c("SampleID","ElementID","BDL","ADL"))

# Select element
YoID <- "Sb" 
# *********************************************************************************

# Training data: ----
# Samples with all all elements within detection limits
SampleID_target <- DATA[ElementID==YoID][BDL==FALSE]$SampleID |> unique()
AUX <- DATA[SampleID %in% SampleID_target]  |> na.omit() |> unique()


# Training data. First approach. impute BDL values for all elements (not YoID)
AUX[BDL==TRUE,ElementValue:=0]
# Aggregate data. Remove BDL ADL by averaging parameters
AUX[,.(min(ElementValue),max(ElementValue),n=.N),by=.(SampleID,ElementID)][n>1]
AUX <- AUX[,.(ElementValue=mean(ElementValue)),by=.(SampleID,ElementID)] |> unique(by=c("SampleID","ElementID"))

# AUX <- AUX[,-c("ADL","BDL")] |> unique(by=c("SampleID","ElementID"))


# Reshape data to wide format, with ElementID as columns and ElementValue as values
DT <- data.table::dcast(AUX, SampleID ~ ElementID, value.var = "ElementValue")
DT <- DT[,-"SampleID"]
# Rename response variable
XCOLS <- colnames(DT)[!(colnames(DT) %in% c(YoID))]
XID <- paste0("X",1:length(XCOLS))
setnames(DT,old=YoID,new="Y")
# Rename features to X1, X2, X3, ...
setnames(DT,old=XCOLS,new=XID)
VARNAMES <- data.table(ID=XID,Name=XCOLS)
DT.train <- DT |> unique()
rm(AUX,DT)
# *********************************************************************************
# Test Data. Remove response variable. Remove rows related with target data YoID  ----
AUX <- DATA[ElementID!=YoID][!(SampleID %in% SampleID_target)]  |> na.omit() |> unique()

# impute BDL values for all elements (including YoID)
AUX[BDL==TRUE,ElementValue:=0]
# Aggregate data. Remove BDL ADL by averaging parameters
AUX[,.(min(ElementValue),max(ElementValue),n=.N),by=.(SampleID,ElementID)][n>1]
AUX <- AUX[,.(ElementValue=mean(ElementValue)),by=.(SampleID,ElementID)] |> unique(by=c("SampleID","ElementID"))
# AUX <- AUX[,-c("ADL","BDL")] |> unique(by=c("SampleID","ElementID"))



# Reshape data to wide format, with ElementID as columns and ElementValue as values
DT <- data.table::dcast(AUX, SampleID ~ ElementID, value.var = "ElementValue")
DT <- DT[,-"SampleID"]
# Rename response variable
# Rename features to X1, X2, X3, ...
setnames(DT,old=XCOLS,new=paste0("X",1:length(XCOLS)))
DT.test <- DT |> unique()
rm(AUX,DT)
# Sumary:
# Number of observations in training dataset
nrow(DT.train) |> sprintf(fmt="There are %d observations in the training dataset")
# Number of observations in test dataset
nrow(DT.test) |> sprintf(fmt="There are %d observations in the test dataset")
# Number of features available for training 
ncol(DT.test) |> sprintf(fmt="There are %d features available for training")

# Anna: optimize feature selection to impute BDL values

# *********************************************************************************
# random forest model ----

# RF arroja mejores resultados con 10-folds (datasets mas chicos) que con 3-folds (dataset mas grandes)

# CV.MDL <- caret::train(Y ~ .,
#                 data = DT.train,
#                 method = "rf",
#                 preprocess=preProcess(DT.train,"BoxCox"),
#                 metric = "RMSE",# metric ="MAE" # metric ="Rsquared"
#                 trControl = trainControl(method="cv",allowParallel = TRUE))
# stopImplicitCluster()
# CV.MDL <- caret::train(Y ~ ., 
#                        data = DT.train,  
#                        method = "lm",
#                        metric = "RMSE")
# stopImplicitCluster()
CV.MDL <- caret::train(Y ~ .,
                data = DT.train,
                method = "glmnet",
                preprocess=preProcess(DT.train,"BoxCox"),
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared"
                trControl = trainControl(method="cv",allowParallel = TRUE))
# Variable Importance
AUX <- varImp(CV.MDL,scale=TRUE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX))
VARIMP <- VARIMP[VARNAMES,on="ID"][Overall>0][order(Overall,decreasing = TRUE)]
print(VARIMP)

# Impute parameters
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
Y <- DT.train$Y #True Values
# Check detection limits
DL <- LGL[ElementID==YoID & BDL==TRUE]$ElementValue |> unique() |> mean()
# Check if the predictions are BELOW detection limits
Yp[Yp<=DL] |> length() |> sprintf(fmt="There are %d predictions below the detection limits")

# Model performance
I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]
RMSE <- CV.MDL$results$RMSE[I]

