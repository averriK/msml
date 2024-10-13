# parse MasterIndex  -----------------------------------------

source("R/setup.R")
AUX <- fread("data-raw/MRD274/metadata.csv",check.names = TRUE,blank.lines.skip = TRUE)
# Tag rejeted records 
AUX[,Rejected:=(grepl(SampleID.1,pattern="R$") | grepl(SampleID.1,pattern="R$"))]
# Remove Rejected Records
AUX <- AUX[Rejected==FALSE]
# Load Spectral data LONG
SML <- fread("data/SML.csv") 
# Find exact matches
SampleID <- SML[ProjectID=="mrd"]$SampleID |> unique() |> sort()
DT1 <- AUX[SampleID.1 %in% SampleID & !(SampleID.2 %in% SampleID)][,SampleID:=SampleID.1]
DT2 <- AUX[SampleID.2 %in% SampleID & !(SampleID.1 %in% SampleID)][,SampleID:=SampleID.2]
DT3 <- AUX[SampleID.2 %in% SampleID & SampleID.1 %in% SampleID][,SampleID:=SampleID.1]
DATA <- rbindlist(list(DT1,DT2,DT3))[,-c("SampleID.1","SampleID.2")] |> unique()

# 930 samples with no exact matches. 
AUX <-  AUX[!(SampleID.2 %in% SampleID) & !(SampleID.1 %in% SampleID)]
# Normalize patterns
ID0 <- gsub(pattern = "[-_.]", replacement = "", SampleID)
ID1 <- gsub(pattern = "[-_.]", replacement = "", AUX$SampleID.1)
ID2 <- gsub(pattern = "[-_.]", replacement = "", AUX$SampleID.2)

DT1 <- AUX[ID1 %in% ID0 & !(ID2 %in% ID0)][,ID0:=ID1]
DT2 <- AUX[ID2 %in% ID0 & !(ID1 %in% ID0)][,ID0:=ID2]
DT3 <- AUX[ID1 %in% ID0 & ID2 %in% ID0][,ID0:=ID2]
DT <- rbindlist(list(DT1,DT2,DT3))[,-c("SampleID.1","SampleID.2")] |> unique()

# Find normalized matches
(ID1 %in% ID0) |> sum()
(ID2 %in% ID0) |> sum()
# BOTH CONDITIONS ARE ZERO. NO MATCHES
rm(AUX,DT1,DT2,DT3,DT,ID0,ID1,ID2)

# Replace "X"/"" by TRUE/FALSE
DATA[,PIMA:=ifelse(PIMA=="X",TRUE,FALSE)]
DATA[,TerraSpec:=ifelse(TerraSpec=="X",TRUE,FALSE)]
DATA[,Litho:=ifelse(Litho=="X",TRUE,FALSE)]
DATA[,XRD:=ifelse(XRD=="X",TRUE,FALSE)]
DATA[,Petro:=ifelse(Petro=="X",TRUE,FALSE)]
DATA[,Niton:=ifelse(Niton=="X",TRUE,FALSE)]

# Fix Depth

DATA[,Depth_m:=as.numeric(Depth_m)]
# Tag Drillholes

DATA[,isDrillhole:=FALSE]
DATA[length(HoleID)>0 | Depth_m>0,isDrillhole:=TRUE]

SampleIDX <- SML[,.(minWL=min(WL),maxWL=max(WL)),by=.(SampleID,SourceID,ProjectID)][order(SampleID)]

DATA <- SampleIDX[DATA,on=.(SampleID)]

fwrite(DATA, "data/Index.csv")
rm(DATA)

