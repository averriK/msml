
# *********************************************************************************
# Start clusters outside the loop
CORES <- detectCores(logical = TRUE)  # Detect the number of logical cores
cl <- makePSOCKcluster(CORES)         # Create a parallel cluster
registerDoParallel(cl)                # Register the parallel backend
# *********************************************************************************
source("R/trainCmodel.R")

# *********************************************************************************
# Stop and release the cluster after use
stopCluster(cl)
registerDoSEQ()  # Ensure that parallel processing is turned off
rm(cl)           # Remove the cluster object from the environment
# *********************************************************************************





