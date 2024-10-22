rm(list=ls())
source("R/setup.R")
# list content of models/regression/*.Rds folder
PATH <- "model"
DIRS <- list.dirs(path=file.path(PATH))
DIRS <- DIRS[DIRS!=PATH ]
Yp <- data.table()
YFILE <- file.path("data","Yp.csv")
if(file.exists(YFILE)) file.remove(YFILE,showWarnings=FALSE)

for(DIR in DIRS){
  FILE <- file.path(DIR,"Yp.csv")
  if(!file.exists(FILE)) next
  DATA <- fread(FILE)
  fwrite(DATA,YFILE,append = TRUE)
}

