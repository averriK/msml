# nolint start

rm(list=ls()) # nolint
source("R/setup.R")
LGL <- fread("data/LGL.csv")
YoID_target <- LGL[!(ElementID %in% c("Be"))]$ElementID |> unique()

nADLmin <- 10 # Numero de veces por encima del limite de deteccion
.tuneLength <- 10 
.trControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = multiClassSummary,
  verboseIter = TRUE,
  allowParallel = TRUE
)
.preProcess <- c("scale", "center")

SET <- "Rn" # Options: An, Rn
.method <- "glmnet" # svmRadialSigma, ranger, gbm, gaussprRadial, C5.0
PATH <- file.path("model", .method)
if (!dir.exists(PATH)) dir.create(PATH)
Xo <- fread(paste0("data/Xo.", SET, ".csv"))
Yo <- fread(paste0("data/Yo.", SET, ".csv"))

# *********************************************************************************
# Start clusters outside the loop
CORES <- detectCores(logical = TRUE)  # Detect the number of logical cores
cl <- makePSOCKcluster(CORES)         # Create a parallel cluster
registerDoParallel(cl)                # Register the parallel backend
# *********************************************************************************

for (YoID in YoID_target) {
  FILE <- file.path(PATH, paste0(SET, "_", .method, "_", YoID, ".Rds"))
  if (file.exists(FILE)) next
  DT.Y <- Yo[ElementID == YoID, .(SampleID, Y = Class)]
  DT.train <- Xo[DT.Y, on = .(SampleID)][, -c("SampleID", "SourceID")]
  
  # Ensure 'Y' is a factor with levels 0 to 5
  DT.train$Y <- factor(DT.train$Y)
  
  model <- train(
    Y ~ .,
    data = DT.train,
    method = .method,
    trControl = .trControl,
    tuneLength = .tuneLength,
    preProcess = .preProcess,
    metric = "Accuracy",
    family = "multinomial"
  )
  
  Y <- DT.Y$Y
  Yp <- predict(model, newdata = DT.train)
  I <- as.numeric(row.names(model$bestTune))
  Accuracy <- model$results$Accuracy[I]
  
  MODEL <- list(model = model, Y = Y, Yp = Yp, Accuracy = Accuracy)
  saveRDS(MODEL, file = FILE)
}

# *********************************************************************************
# Stop and release the cluster after use
stopCluster(cl)
registerDoSEQ()  # Ensure that parallel processing is turned off
rm(cl)           # Remove the cluster object from the environment
# *********************************************************************************

# nolint end
