source("R/setup.R")
# 750 (visible) a 2400 >2400 ruido)
YoID <- "Au"
stackModel <- readRDS(paste0("models/stackModel_",YoID,".Rds"))
modelList <- readRDS(paste0("models/modelList_",YoID,".Rds"))
PL <- 0.65 # Threeshold
Xo <- fread("data/Xo.csv")
Yo <- fread("data/Yo.csv")
DT.train <- Xo[Yo[ElementID==YoID],on=.(SampleID)]

DT.train[,Y:=factor(ifelse(Y>0,"Y","N"))]
Y <- DT.train$Y
# Predict probabilities from stacked model
Yp <- predict(stackModel,newdata=DT.train)
Yp <- ifelse(Yp[, "Y"] > PL, "Y", "N") |> factor()
confusionMatrix(Yp, Y) #confusion matrix


# Predict probabilities from individual models: svmRadialSigma
# High number of false negatives
Yp <- predict(modelList$svmRadialSigma,newdata=DT.train, type = "prob")
Yp <- ifelse(Yp[, "Y"] > PL, "Y", "N") |> factor()
confusionMatrix(Yp, Y) #confusion matrix

# Predict probabilities from individual models: avNNet
Yp <- predict(modelList$avNNet,newdata=DT.train, type = "prob")
Yp <- ifelse(Yp[, "Y"] > PL, "Y", "N") |> factor()
confusionMatrix(Yp, Y) #confusion matrix

# Predict probabilities from individual models: glmnet
Yp <- predict(modelList$glmnet,newdata=DT.train, type = "prob")
Yp <- ifelse(Yp[, "Y"] > PL, "Y", "N") |> factor()
confusionMatrix(Yp, Y) #confusion matrix
# Predict probabilities from individual models: ranger
# 0 False negatives and 0 false positives. But we need to see how well it generalizes with test data
Yp <- predict(modelList$ranger,newdata=DT.train, type = "prob")
Yp <- ifelse(Yp[, "Y"] > PL, "Y", "N") |> factor()
confusionMatrix(Yp, Y) #confusion matrix

# colAUC(Yp, DT.train$Y, plotROC = TRUE)
