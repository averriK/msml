# nolint start

LGL <- fread("data/LGL.csv")
YoID_target <- LGL$ElementID |> unique()

.tuneLength <- 10 
.preProcess <- NULL

SET <- "Rn" # Options: An, Rn
.methods <- c("ranger","gaussprRadial","svmRadialSigma","C5.0", "glmnet", "gbm")

Xo <- fread(paste0("data/Xo.", SET, ".csv"))
Yo <- fread(paste0("data/Yo.", SET, ".csv"))

for (.method in .methods) {
  PATH <- file.path("model", .method)
  if (!dir.exists(PATH)) dir.create(PATH)
  
  for (YoID in YoID_target) {
    FILE <- file.path(PATH, paste0(SET, "_", .method, "_", YoID, ".Rds"))
    if (file.exists(FILE)) next
    DT.Y <- Yo[ElementID == YoID, .(SampleID, Y = Class)]
    DT.train <- Xo[DT.Y, on = .(SampleID)][, -c("SampleID", "SourceID")]
    
    # Ensure 'Y' is a factor with levels L and H
    DT.train$Y <- factor(DT.train$Y, levels = c("L", "H"))
    
    # Create element-specific trainControl
    .trControl <- trainControl(
      method = "cv",
      number = 10,
      summaryFunction = twoClassSummary,
      classProbs = TRUE,
      verboseIter = TRUE,
      allowParallel = TRUE,
      sampling = "up",
      index = createFolds(DT.train$Y, k = 10, list = TRUE, returnTrain = TRUE)
    )
    
    # Set method-specific parameters
    methodParams <- switch(.method,
      "svmRadialSigma" = list(),
      "ranger" = list(importance = 'impurity'),
      "gbm" = list(verbose = FALSE),
      "gaussprRadial" = list(),
      "C5.0" = list(winnow = TRUE),
      "glmnet" = list(family = "binomial")
    )
    
    model <- do.call(train, c(list(
      Y ~ .,
      data = DT.train,
      method = .method,
      trControl = .trControl,
      tuneLength = .tuneLength,
      preProcess = .preProcess,
      metric = "ROC"
    ), methodParams))
    
    Y <- DT.Y$Y
    Yp <- predict(model, newdata = DT.train)
    I <- as.numeric(row.names(model$bestTune))
    ROC <- model$results$ROC[I]
    
    MODEL <- list(model = model, Y = Y, Yp = Yp, ROC = ROC)
    saveRDS(MODEL, file = FILE)
    
    cat("Completed:", .method, "for", YoID, "\n")
  }
}
# nolint end
