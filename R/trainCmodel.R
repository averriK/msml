# nolint start

rm(list=ls()) # nolint
source("R/setup.R")

# Load data
LGL <- fread("data/LGL.csv")
YoID_target <- LGL[!(ElementID %in% c("Be"))]$ElementID |> unique()

nADLmin <- 10 # Number of times above detection limit
.tuneLength <- 10
.trControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = multiClassSummary,
  verboseIter = TRUE,
  allowParallel = TRUE,
  classProbs = TRUE,
  sampling = "smote"  # Handle class imbalance
)
.preProcess <- c("scale", "center")

SET <- "Rn" # Options: An, Rn

# List of methods to iterate over
.methods <- c("glmnet", "svmRadialSigma", "ranger", "gbm", "gaussprRadial", "C5.0")

# Load datasets
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
    DT.Y$Y <- droplevels(factor(DT.Y$Y))  # Ensure 'Y' is a factor
    
    DT.train <- Xo[DT.Y, on = .(SampleID)][, -c("SampleID", "SourceID")]
    DT.train$Y <- DT.Y$Y  # Assign 'Y' to training data
    
    # Check for classes with fewer than 8 observations
    class_counts <- table(DT.train$Y)
    if (any(class_counts < 8)) {
      message(sprintf("Skipping YoID '%s' for method '%s' due to insufficient data in some classes.", YoID, .method))
      next
    }
    
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
      # Specify multinomial family for multiclass classification
      train_params$family <- "multinomial"
      train_params$maxit <- 1e6  # Increase iterations
    }
    if (.method == "gbm") {
      # Set distribution to multinomial for multiclass targets
      train_params$distribution <- "multinomial"
    }
    if (.method == "gaussprRadial") {
      # Ensure the method performs classification
      train_params$type <- "Classification"
    }
    
    # Try to train the model
    tryCatch({
      model <- do.call(train, train_params)
      
      # Predict on the training data
      Yp <- predict(model, newdata = DT.train)
      Yp <- factor(Yp, levels = levels(DT.Y$Y))  # Ensure levels match
      Yp <- droplevels(Yp)
      
      Y <- DT.Y$Y  # 'Y' is already a factor
      
      # Compute accuracy
      Accuracy <- sum(Yp == Y) / length(Y)
      
      # Create MODEL list
      MODEL <- list(
        model = model,
        Y = Y,
        Yp = Yp,
        Accuracy = Accuracy
      )
      
      # Save the MODEL
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
