# nolint start
rm(list=ls())
# import spectral data -----------------------------------------
source("R/setup.R")
source("R/utils.R")
# MRD274 data
ASD1<- fread("spectra/MRD274/ASD.CSV")
DSP <- fread("spectra/MRD274/DSP.CSV")
FOS <- fread("spectra/MRD274/FOS.CSV")
# BalyaNorth data
ASD2<- fread("spectra/BalyaNorth/ASD.CSV")


# Get data from spectrometers
ivar <- "Wavelength_(nm)"
mvars <- colnames(ASD1)[!colnames(ASD1) %in% ivar]
melt(ASD1, id.vars = ivar, measure.vars = mvars, variable.name = "Sample", value.name = "Reflectance") -> ASD1

ivar <- "Wavelength_(nm)"
mvars <- colnames(ASD2)[!colnames(ASD2) %in% ivar]
melt(ASD2, id.vars = ivar, measure.vars = mvars, variable.name = "Sample", value.name = "Reflectance") -> ASD2

mvars <- colnames(DSP)[!colnames(DSP) %in% ivar]
melt(DSP, id.vars = ivar, measure.vars = mvars, variable.name = "Sample", value.name = "Reflectance") -> DSP

mvars <- colnames(FOS)[!colnames(FOS) %in% ivar]
melt(FOS, id.vars = ivar, measure.vars = mvars, variable.name = "Sample", value.name = "Reflectance") -> FOS
# Tag and Bind tables
DATA <- rbindlist(list(
  data.table(SourceID="asd", ProjectID="mrd",ASD1),
  data.table(SourceID="asd", ProjectID="bn",ASD2),
  data.table(SourceID="dsp", ProjectID="mrd",DSP),
  data.table(SourceID="fos", ProjectID="mrd",FOS)
  
))
# Rename Wavelegth column to WL
setnames(DATA, "Wavelength_(nm)", "WL")
# Rename Reflectance to R
setnames(DATA, "Reflectance", "R")
# Remove shit tagged by TSG8
DATA[,SampleID:=sub(".*:(\\w+).*", "\\1", Sample)]
DATA[,Sample:=NULL]
DATA[,SampleID:=gsub(" \\d+\\.\\d+M", "", SampleID)]
DATA[,SampleID:=gsub(" \\d+\\.\\d+FT", "", SampleID)]
# DATA[,SampleID:=gsub(" *[[:space:]]+.*", "", SampleID)]
DATA[,SampleID:=gsub("[-.]", "", SampleID)]
DATA[,SampleID:=trimws(SampleID)]
DATA[,SampleID:=toupper(SampleID) |> trimws()]


DATA$SampleID |> unique() |> length() |> sprintf(fmt="There are %d unique SampleIDs available")


# *********************************************************************************
# Build Standard Dataset. Continuous Wavelength range (CWLR)
# ASD spectra has WL 300,301,...1000,1001,1002,1003...
# DSP spectra has WL 1300,1302,1304,....
# FOS spectra has WL 1300,1302,1304,....
# The following code resamples all SourceID groups to the same WL range

WLmin <- 1000
WLmax <- 2450
# Remove (Aggregate) possible duplicates from different ProjectIDs
DATA <- DATA[WL>=WLmin & WL<=WLmax,.(R=mean(R)),by=.(SampleID,SourceID,WL)]

# Get WL range for all spectra. 604 spectral ordinates
WLo <- DATA$WL |> unique()

# Remove odd values from WLo using is.odd()
WLo <- WLo[!is.odd(WLo)]


# Resample all SourceID groups to the same WL range
DATA <- DATA[,.(WL=WLo,R=approx(x=.SD$WL,y=.SD$R,xout=WLo,yleft=0)$y),by=.(SampleID,SourceID)]



SML <- DATA[,.(
  WL,R,
  # Rm=get_emd_envelope(x=.SD$WL,y=.SD$R),
  Rn=get_convex_envelope(x=.SD$WL,y=.SD$R,type="upper")
),by=.(SampleID,SourceID)]

SML[,Ue:=R+Rn,by=.(SampleID,SourceID)]
fwrite(SML, "data/SML.csv")
rm(DATA)

# Replace NA values in Rm
setorder(SML, SampleID, SourceID,WL)

# SML[, Rm := nafill(Rm, type = "nocb"), by = .(SampleID,SourceID)]

# SML[is.na(Rm),Rm:=0,by=.(SampleID,SourceID)]

SXL <- SML[,get_peaks(.SD,x=.SD$Rn),by=.(SourceID,SampleID)]
fwrite(SXL,"data/SXL.csv")

# nolint end