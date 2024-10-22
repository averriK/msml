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

# List of methods to iterate over
.methods <- c("glmnet", "svmRadialSigma", "ranger", "gbm", "gaussprRadial", "C5.0")

Xo <- fread(paste0("data/Xo.", SET, ".csv"))
Yo <- fread(paste0("data/Yo.", SET, ".csv"))

# *********************************************************************************
# Start clusters outside the loop
CORES <- detectCores(logical = TRUE)  # Detect the number of logical cores
cl <- makePSOCKcluster(CORES)         # Create a parallel cluster
registerDoParallel(cl)                # Register the parallel backend
# *********************************************************************************

for (.method in .methods) {
  PATH <- file.path("model", .method)
  if (!dir.exists(PATH)) dir.create(PATH, recursive = TRUE)
  
  for (YoID in YoID_target) {
    FILE <- file.path(PATH, paste0(SET, "_", .method, "_", YoID, ".Rds"))
    if (file.exists(FILE)) next
    DT.Y <- Yo[ElementID == YoID, .(SampleID, Y = Class)]
    DT.train <- Xo[DT.Y, on = .(SampleID)][, -c("SampleID", "SourceID")]
    
    # Ensure 'Y' is a factor
    DT.train$Y <- factor(DT.train$Y)
    
    # Set up the basic parameters for train
    train_params <- list(
      formula = Y ~ .,
      data = DT.train,
      method = .method,
      trControl = .trControl,
      tuneLength = .tuneLength,
      preProcess = .preProcess,
      metric = "Accuracy"
    )
    
    # Additional parameters for specific methods
    if (.method == "glmnet") {
      train_params$family <- "multinomial"
    }
    if (.method == "gbm") {
      train_params$distribution <- "multinomial"
    }
    
    # Try to train the model
    tryCatch({
      model <- do.call(train, train_params)
      
      Y <- DT.Y$Y
      Yp <- predict(model, newdata = DT.train)
      I <- as.numeric(row.names(model$bestTune))
      Accuracy <- model$results$Accuracy[I]
      
      MODEL <- list(model = model, Y = Y, Yp = Yp, Accuracy = Accuracy)
      saveRDS(MODEL, file = FILE)
    }, error = function(e) {
      message(sprintf("Error training method '%s' for YoID '%s': %s", .method, YoID, e$message))
    })
  }
}

# *********************************************************************************
# Stop and release the cluster after use
stopCluster(cl)
registerDoSEQ()  # Ensure that parallel processing is turned off
rm(cl)           # Remove the cluster object from the environment
# *********************************************************************************

# nolint end
