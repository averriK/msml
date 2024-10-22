rm(list=ls())
source("R/setup.R")
LGL <- fread("data/LGL.csv")

# Exclude unwanted elements
YoID_target <- LGL[!(ElementID %in% c("Be"))]$ElementID |> unique()

nADLmin <- 10 # Minimum number above the detection limit
.tuneLength <- 10 
.trControl <-  trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  allowParallel = TRUE
)
.preProcess <- c("scale", "center")

SET <- "Rn" # Set of data
.method <- "gaussprRadial"#"gaussprRadial" #C5.0 # Selected model
PATH <- file.path("model", .method)
if (!dir.exists(PATH)) dir.create(PATH)

Xo <- fread(paste0("data/Xo.", SET, ".csv"))
Yo <- fread(paste0("data/Yo.", SET, ".csv"))

# Start clusters only once, outside the loop
CORES <- detectCores(logical = TRUE)
cl <- makePSOCKcluster(CORES)
registerDoParallel(cl)

# Ensure cluster is stopped when the script finishes
on.exit({
  stopCluster(cl)
  registerDoSEQ()
})

for (YoID in YoID_target) {
  FILE <- file.path(PATH, paste0(SET, "_", .method, "_", YoID, ".Rds"))
  if (file.exists(FILE)) next
  
  DT.Y <- Yo[ElementID == YoID, .(SampleID, Y = factor(ifelse(nADL >= nADLmin, "Y", "N")))]
  DT.train <- Xo[DT.Y, on = .(SampleID)][, -c("SampleID", "SourceID")]
  
  model <- train(
    Y ~ .,
    data = DT.train,
    method = .method,
    trControl = .trControl,
    tuneLength = .tuneLength,
    preProcess = .preProcess,
    metric = "ROC"
  )
  
  Y <- DT.Y$Y
  Yp <- predict(model, newdata = DT.train)
  I <- as.numeric(row.names(model$bestTune))
  ROC <- model$results$ROC[I]
  
  MODEL <- list(model = model, Y = Y, Yp = Yp, ROC = ROC)
  saveRDS(MODEL, file = FILE)
}


PATH <-file.path("model",.method)
DIR <- list.files(file.path(PATH),pattern="*.Rds")
# Remove extension .Rds
DIR <- gsub("\\.Rds", "", DIR)
# Split by "_". First element is SET, second is ML, third is YoID. Build a data.table
if(length(DIR)>1){
  ModelIndex <- tstrsplit(DIR,"_",keep=1:3) |> as.data.table()
  setnames(ModelIndex,old=c("V1","V2","V3"),new=c("S","M","ID"))
  ModelIndex[,file:=paste0(S,"_",M,"_",ID,".Rds")]
} else stop()

# SET <- "Rn"
DATA <- data.table()
YFILE <- file.path(PATH,"Yp.csv")
if(file.exists(YFILE)) file.remove(YFILE,showWarnings=FALSE)
# SET <- ModelIndex$S[1]
for(SET in unique(ModelIndex$S)){
  
  Xi <- fread(paste0("data/Xi.",SET,".csv"))
  # YoID <- "Ag"
  for(YoID in unique(ModelIndex$ID)){
    # Build a list of files with ModelIndex[oID == YoID_target),file]
    INDEX <-  ModelIndex[ID == YoID & S == SET ]
    FILE_LIST <-INDEX$file
    # Build model list for stacking
    
    # MODEL_LIST <- list()
    FILE <- FILE_LIST[1]
    for(FILE in FILE_LIST){
      O <- readRDS(file.path(PATH,FILE))
      METHOD <- O$model$method
      sprintf("File: %s  Model:%s Element:%s\n",FILE,METHOD,YoID) %>% cat()
      
      P <- predict(O$model,newdata=Xi,type="prob")
      DATA <-Xi[,.(SampleID,ElementID=YoID,DataID=SET,ModelID=METHOD,FileID=FILE,"P[C>10*DL]"= P$Y,ROC=O$ROC)]
      # MODEL_LIST[[METHOD]] <- O$model
      fwrite(DATA,YFILE,append = TRUE)
    }
  }
  
}






# Release cluster (this is also handled by on.exit)
stopCluster(cl)
registerDoSEQ()
rm(cl)
