rm(list=ls())
source("R/setup.R")
# list content of models/regression/*.Rds folder
PATH <- "model"
DIRS <- list.dirs(path=file.path(PATH))
DIRS <- DIRS[DIRS!=PATH]
