source("R/setup.R")
source("R/utils.R")
# Load Master Index ?
MasterIDX <- fread("data/Index.csv",check.names=TRUE)
# Load Lithology LONG 
LGL <- fread("data/LGL.csv") 
# Load Spectral data LONG
SML <- fread("data/SML.csv") 
# Load Spectral data LONG
SXL <- fread("data/SXL.csv") 

# Option A. (R) Full spectra
# TYPE <- "R" 
# Option B. Train model with Rn, without envelope
# TYPE <- "Rn"


# Detection Limits
kB <- 1 # 0% of below-detection limit 
# Fix BDL/ADL values
LGL[BDL==TRUE,ElementValue:=kB*ElementValue,by=.(ElementID,ElementValue)]
LGL[,BDL:=NULL]
LGL <- unique(LGL)
# *********************************************************************************
# Build Standard Dataset. Continuous Wavelength range (CWLR)
# Reshape DATA to wide format
# If dcast() fails, means that not all spectra has spectral ordinates in all WLo. In that case, resample DATA to WLo ranges
# This approach requires the full range of wavelengths with values. Therefore infrared spectra with values below 1400 cannot be mixed with PIMA data with values only for WLlarger than 1400


# *********************************************************************************
# Build Standard Dataset. Continuous Wavelength range (CWLR)
# ASD spectra has WL 300,301,...1000,1001,1002,1003...
# DSP spectra has WL 1300,1302,1304,....
# FOS spectra has WL 1300,1302,1304,....
# The following code resamples all SourceID groups to the same WL range

# Get WL range for all spectra. 604 spectral ordinates
WLo <- SML$WL |> unique()
# build a function is.odd(x) that reports TRUE if x is odd and FALSE otherwise
is.odd <- function(x) x %% 2 != 0
# constant column check
removeZV <- function(.x) {
  XCOL <- .x[, sapply(.SD, function(y) length(unique(y)) == 1)]
  .x[, !XCOL, with = FALSE]
}
# Remove odd values from WLo using is.odd()
WLo <- WLo[!is.odd(WLo)]
# Remove NA values if any, otherwise approx() fails
SML <- na.omit(SML)
# Resample all SourceID groups to the same WL range
AUX <- SML[,.(WL=WLo,Rn=approx(x=.SD$WL,y=.SD$Rn,xout=WLo,yleft=0)$y),by=.(SampleID,SourceID)]
# Reshape DATA to wide format
DATA <- dcast(AUX,SampleID+SourceID~WL,value.var="Rn") 

# rename cols
COLS <- setdiff(names(DATA), c("SampleID","SourceID")) |> trimws()
setnames(DATA,old=COLS,new=paste0("X",COLS))


IDX <- intersect(unique(LGL$SampleID),unique(DATA$SampleID)) 



# Restrict DATA to SampleID with lithogeochemistry available
Xo <- DATA[SampleID %in% IDX] 
# Remove Zero-Variance columns 
Xo <- removeZV(Xo)  
# Build unsupervised Dataset
COLS <- names(Xo)
Xi <- DATA[!(SampleID %in% IDX),..COLS]
Yo <- LGL[SampleID %in% IDX,.(Y=mean(ElementValue)),by=.(ElementID,SampleID)]
# Check column integrity
names(Xo) %in% names(Xi) |> all()
fwrite(Xo,"data/Xo.Rn.csv")
fwrite(Xi,"data/Xi.Rn.csv")
fwrite(Yo,"data/Yo.Rn.csv")



# *********************************************************************************
# Build Reduced Dataset. Discrete Wavelength range (DWLR)
# This approach allows to mix different formats (fos, dsp asd) and wavelength ranges
# ****************************************************
# Extract Features WL
DATA <- na.omit(SML)
SXL <- DATA[,get_peaks(.SD,x=.SD$Rm),by=.(SourceID,SampleID)]
fwrite(SXL,"data/SXL.csv")
# Get supervised samples
IDX <- intersect(unique(LGL$SampleID),unique(SXL$SampleID)) |> unique()

DATA <- dcast(SXL[,.(SourceID,SampleID,WL,A=Rn)],SampleID+SourceID~WL,value.var="A",fill =0) 
# rename cols
COLS <- setdiff(names(DATA), c("SampleID","SourceID")) |> trimws()
setnames(DATA,old=COLS,new=paste0("X",COLS))
# Restrict DATA to SampleID with lithogeochemistry available
Xo <- DATA[SampleID %in% IDX] 
# Remove Zero-Variance columns 
Xo <- removeZV(Xo)  
# Build unsupervised Dataset
COLS <- names(Xo)
Xi <- DATA[!(SampleID %in% IDX),..COLS]
Yo <- LGL[SampleID %in% IDX,.(Y=mean(ElementValue)),by=.(ElementID,SampleID)]
# Check column integrity
names(Xo) %in% names(Xi) |> all()

fwrite(Xo,"data/Xo.An.csv")
fwrite(Xi,"data/Xi.An.csv")
fwrite(Yo,"data/Yo.An.csv")
