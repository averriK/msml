# *********************************************************************************
source("R/setup.R")
SourceID_Target <- c("dsp","fos") # We cannot MIX asd/sco together with dsp/fos.
ProjectID_Target <- "mrd"
YoID <- "Sb"

# Load Master Index ?
MasterIDX <- fread("data/Index.csv",check.names=TRUE)
# Load Lithology LONG 
LGL <- fread("data/LGL.csv") 
# Load Spectral data LONG
SML <- fread("data/SML.csv") 
# Select data
DATA <- SML[SourceID %in% SourceID_Target & ProjectID ==ProjectID_Target]
# Restrict DATA to SampleID with lithogeochemistry available
SampleID_Target <- intersect(unique(LGL$SampleID),unique(DATA$SampleID))

# Get WL ranges (1300-2500)
WL_ranges <- DATA[SampleID %in% SampleID_Target,.(minWL=min(WL),maxWL=max(WL)),by=.(SampleID)][order(SampleID)]
minWL <-  WL_ranges$minWL |> max()
maxWL <-  WL_ranges$maxWL |> min()

# Reshape DATA to wide format
dcast(DATA,SampleID+SourceID+ProjectID~WL,value.var="R") -> DATA
# If dcast() fails, means that not all spectra has spectral ordinates in all WLo. In that case, resample DATA to WLo ranges
# Get WL range for all spectra. 604 spectral ordinates
WLo <- DATA$WL |> unique()
# Build Xo (Features Matrix) Remove SourceID and ProjectID
Xo <- DATA[SampleID %in% SampleID_Target,-c("SourceID","ProjectID")]
# Build Xi with samples with no litogeochem available
Xi <- DATA[!(SampleID %in% SampleID_Target),-c("SourceID","ProjectID")]
COLS <- setdiff(names(Xo), "SampleID")
setnames(Xo,COLS,paste0("X",COLS))
setnames(Xi,COLS,paste0("X",COLS))
fwrite(Xo,"data/Xo.csv")
fwrite(Xi,"data/Xi.csv")
# Fix BDL/ADL values
LGL[BDL==TRUE,ElementValue:=kB*ElementValue,by=.(ElementID,ElementValue)]
LGL[ADL==TRUE,ElementValue:=kA*ElementValue,by=.(ElementID,ElementValue)]
LGL[,BDL:=NULL]
LGL[,ADL:=NULL]
LGL <- unique(LGL)

# Build Yo
names(Yo)
# Remove (aggregate) isDrillhole
Yo <- LGL[SampleID %in% SampleID_Target,.(Y=mean(ElementValue)),by=.(ElementID,SampleID)]



fwrite(Yo,"data/Yo.csv")
# 
# a=fread("pred/YmW.csv")
# nrow(a) #4195
