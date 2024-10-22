rm(list=ls()) # nolint # nolint
source("R/setup.R")
source("R/utils.R")

# Load Lithology LONG  # nolint
LGL <- fread("data/LGL.csv")  # nolint
# Load Spectral data LONG
SML <- fread("data/SML.csv")  # nolint
SXL <- SML[complete.cases(),get_peaks(.SD,x=.SD$Rm),by=.(SourceID,SampleID)]
fwrite(SXL,"data/SXL.csv")

# Option A. (R) Full spectra
# TYPE <- "R" 
# Option B. Train model with Rn, without envelope
# TYPE <- "Rn"

LGL <- unique(LGL)
# *********************************************************************************
# Build Standard Dataset. Continuous Wavelength range (CWLR)
# Reshape DATA to wide format
# If dcast() fails, means that not all spectra has spectral ordinates in all WLo. In that case, resample DATA to WLo ranges
# This approach requires the full range of wavelengths with values. Therefore infrared spectra with values below 1400 cannot be mixed with PIMA data with values only for WLlarger than 1400

# Reshape DATA to wide format
DATA <- dcast(SML,SampleID+SourceID~WL,value.var="Rn") 

# rename cols
COLS <- setdiff(names(DATA), c("SampleID","SourceID")) |> trimws()
setnames(DATA,old=COLS,new=paste0("X",COLS))


IDX <- intersect(unique(LGL$SampleID),unique(DATA$SampleID)) 



# Restrict DATA to SampleID with lithogeochemistry available
Xo <- DATA[SampleID %in% IDX] 
# Remove Zero-Variance columns 
Xo <- removeNZV(Xo)  

# Build unsupervised Dataset
COLS <- names(Xo)
Xi <- DATA[!(SampleID %in% IDX),..COLS]
Yo <- LGL[SampleID %in% IDX,.(ElementID,SampleID,ElementValue,nADL)]
# Check column integrity
names(Xo) %in% names(Xi) |> all()
fwrite(Xo,"data/Xo.Rn.csv")
fwrite(Xi,"data/Xi.Rn.csv")
fwrite(Yo,"data/Yo.Rn.csv")



# *********************************************************************************
# Build Reduced Dataset. Discrete Wavelength range (DWLR)
# This approach allows to mix different formats (fos, dsp asd) and wavelength ranges
# ****************************************************
# Load Spectral data LONG
SXL <- fread("data/SXL.csv") 
# Get supervised samples
IDX <- intersect(unique(LGL$SampleID),unique(SXL$SampleID)) |> unique()

DATA <- dcast(SXL[,.(SourceID,SampleID,WL,A=Rn)],SampleID+SourceID~WL,value.var="A",fill =0) 
# rename cols
COLS <- setdiff(names(DATA), c("SampleID","SourceID")) |> trimws()
setnames(DATA,old=COLS,new=paste0("X",COLS))
# Restrict DATA to SampleID with lithogeochemistry available
Xo <- DATA[SampleID %in% IDX] 
# Remove Zero-Variance columns 
Xo <- removeNZV(Xo)  

# Build unsupervised Dataset
COLS <- names(Xo)
Xi <- DATA[!(SampleID %in% IDX),..COLS]
Yo <- LGL[SampleID %in% IDX,.(ElementID,SampleID,ElementValue,nADL)]
# Check column integrity
names(Xo) %in% names(Xi) |> all()
# data.train <- Xo[Yo[ElementID==YoID],on=.(SampleID)][,-c("SampleID","ElementID","SourceID")]

fwrite(Xo,"data/Xo.An.csv")
fwrite(Xi,"data/Xi.An.csv")
fwrite(Yo,"data/Yo.An.csv")
