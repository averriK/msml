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
kB <- 0 # 0% of below-detection limit 
kA <- 1 # 100% below-detection limit data
# Fix BDL/ADL values
LGL[BDL==TRUE,ElementValue:=kB*ElementValue,by=.(ElementID,ElementValue)]
LGL[ADL==TRUE,ElementValue:=kA*ElementValue,by=.(ElementID,ElementValue)]
LGL[,BDL:=NULL]
LGL[,ADL:=NULL]
LGL <- unique(LGL)
# *********************************************************************************
# Build Standard Dataset. Continuous Wavelength range (CWLR)
# This approach requires the full range of wavelengths with values. Therefore infrared spectra with values below 1400 cannot be mixed with PIMA data with values only for WLlarger than 1400



# Get WL ranges (1300-2500)

# Get WL range for all spectra. 604 spectral ordinates
# WLo <- SML$WL |> unique()
# WL_ranges <- SML[SampleID %in% SampleID_Target,.(minWL=min(WL),maxWL=max(WL)),by=.(SampleID)][order(SampleID)]
# minWL <-  WL_ranges$minWL |> max()
# maxWL <-  WL_ranges$maxWL |> min()

# Reshape DATA to wide format
# If dcast() fails, means that not all spectra has spectral ordinates in all WLo. In that case, resample DATA to WLo ranges
DATA.asd <- dcast(SML[SourceID=="asd",-c("SourceID")],SampleID~WL,value.var="Rn") 
# Restrict DATA to SampleID with lithogeochemistry available
SampleID_Target <- intersect(unique(LGL$SampleID),unique(SML[SourceID=="asd"]$SampleID)) |> trimws()



COLS <- setdiff(names(DATA.asd), c("SampleID")) |> trimws()
Xo.asd <- DATA.asd[SampleID %in% SampleID_Target] |> setnames(old=COLS,new=paste0("X",COLS))
Xi.asd <- DATA.asd[!(SampleID %in% SampleID_Target)] |> setnames(old=COLS,new=paste0("X",COLS))
# Build Yo
# Remove (aggregate) isDrillhole
Yo.asd <- LGL[SampleID %in% SampleID_Target,.(Y=mean(ElementValue)),by=.(ElementID,SampleID)]
fwrite(Yo.asd,"data/Yo.asd.csv")
fwrite(Xo.asd,"data/Xo.asd.csv")
fwrite(Xi.asd,"data/Xi.asd.csv")


DATA.dsp <- dcast(SML[SourceID=="dsp",-c("SourceID")],SampleID~WL,value.var="Rn") 
SampleID_Target <- intersect(unique(LGL$SampleID),unique(SML[SourceID=="dsp"]$SampleID)) |> trimws()
COLS <- setdiff(names(DATA.dsp), c("SampleID")) |> trimws()
Xo.dsp <- DATA.dsp[SampleID %in% SampleID_Target] |> setnames(old=COLS,new=paste0("X",COLS))
Xi.dsp <- DATA.dsp[!(SampleID %in% SampleID_Target)] |> setnames(old=COLS,new=paste0("X",COLS))
# Build Yo
# Remove (aggregate) isDrillhole
Yo.dsp <- LGL[SampleID %in% SampleID_Target,.(Y=mean(ElementValue)),by=.(ElementID,SampleID)]
fwrite(Yo.dsp,"data/Yo.dsp.csv")
fwrite(Xo.dsp,"data/Xo.dsp.csv")
fwrite(Xi.dsp,"data/Xi.dsp.csv")







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
COLS <- setdiff(names(DATA), c("SampleID","SourceID")) |> trimws()
setnames(DATA,old=COLS,new=paste0("X",COLS))

# Remove Zero columns
COLS <- setdiff(names(DATA), c("SampleID","SourceID"))
NZCOLS <- COLS[colSums(DATA[,..COLS]!=0)>0]
DATA <- DATA[,..NZCOLS]
Xo <- DATA[SampleID %in% IDX] 
Xi <- DATA[!(SampleID %in% IDX)]
Yo <- LGL[SampleID %in% IDX,.(Y=mean(ElementValue)),by=.(ElementID,SampleID)]


fwrite(Xo,"data/Xo.An.csv")
fwrite(Xi,"data/Xi.An.csv")
fwrite(Yo,"data/Yo.An.csv")
