rm(list=ls()) # nolint # nolint
source("R/setup.R")
source("R/utils.R")

# Load Lithology LONG  # nolint
LGL <- fread("data/LGL.csv")  # nolint
# Load Spectral data LONG
SML <- fread("data/SML.csv")  # nolint

LGL <- unique(LGL)
# *********************************************************************************
# Build Standard Dataset. Continuous Wavelength range (CWLR)
# Reshape DATA to wide format
# If dcast() fails, means that not all spectra has spectral ordinates in all WLo. In that case, resample DATA to WLo ranges # nolint
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
Yo <- LGL[SampleID %in% IDX,.(ElementID,SampleID,ElementValue)]

# Define Y based on ElementValue
Yo[, Class := cut(ElementValue,
              breaks = c(-Inf, 1, 10, 100, 1000, 10000, Inf),
              labels = 0:5,
              right = TRUE)]

# Convert Y to a factor with levels 0 to 5
Yo[, Y := factor(Y, levels = 0:5)]



# Check column integrity
names(Xo) %in% names(Xi) |> all()
fwrite(Xo,"data/Xo.Rn.csv")
fwrite(Xi,"data/Xi.Rn.csv")
fwrite(Yo,"data/Yo.Rn.csv")


