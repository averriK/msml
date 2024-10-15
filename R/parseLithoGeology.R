source("R/setup.R")
# Dataset A
# Removed from Source duplicated Ni. We kept Ni values which most values above the DL 
# Removed from Source LithoID

LITH_A <- fread("data-raw/litho_A_v2.csv",check.names=TRUE) #1115 samples
LITH_A[,SetID:="A"] # Tag provider
# Remove spaces and dots from column names, LITH_A |> colnames() |> grep(pattern="[ .]",value=TRUE)

OLD <- colnames(LITH_A)
NEW <- OLD |> gsub(pattern ="[ .]", replacement="_")
setnames(LITH_A,OLD,NEW)

# Dataset B
# Removed from Source LithoID
# Removed from source Ni_4AES_pp with most values below detection limits. Use Ni_1DXMS_p instead. Anna double check
LITH_B <- fread("data-raw/litho_B_v2.csv",check.names=TRUE) #951 samples 
LITH_B[,SetID:="B"] # Tag provider
# remove Wgt
# Remove suffixes from features names
OLD <- colnames(LITH_B)
NEW <- OLD |> sub(pattern="(_4AES.*|_1DXMS.*)$", replacement="")
setnames(LITH_B,OLD,NEW)

# Remove spaces
OLD <- colnames(LITH_B)
NEW <- OLD |> gsub(pattern ="[ .]", replacement="_")
setnames(LITH_B,OLD,NEW)




# Add isDrillhole flag to LITH_A and fix isDrillhole in LITH_B
# Double check Anna if must be FALSE?
LITH_A[,isDrillhole:=FALSE]
LITH_B[isDrillhole=="N",isDrillhole:=FALSE]
LITH_B[isDrillhole!="N",isDrillhole:=TRUE]
# Remove Wgt
LITH_A[,Wgt:=NULL]
LITH_B[,Wgt:=NULL]

# Common Features
union(colnames(LITH_A),colnames(LITH_B)) |> unique() |> length() |> sprintf(fmt="There are %d features in A and B")

# How many samples in A are also in B? 776.
intersect(LITH_A$SampleID,LITH_B$SampleID) |> length() |> sprintf(fmt="There are %d samples in common between A and B")

# How many samples has been analyzed by both providers? 1288
union(LITH_B$SampleID,LITH_A$SampleID) |> unique() |> length() |> sprintf(fmt="There are %d samples in common between A and B")

# How many features are in common between A and B? 62
intersect(colnames(LITH_A),colnames(LITH_B)) |> length() |> sprintf(fmt="There are %d features in common between A and B")

# Which features are in A and not in B?
setdiff(colnames(LITH_A),colnames(LITH_B)) |> length() |> sprintf(fmt="There are %d features in A and not in B")

# Which features are in B and not in A?
setdiff(colnames(LITH_B),colnames(LITH_A)) |> length() |> sprintf(fmt="There are %d features in B and not in A")



# Exclude features that are not in common between A and B
LITH_A <- LITH_A[,intersect(colnames(LITH_A),colnames(LITH_B)),with=FALSE]
LITH_B <- LITH_B[,intersect(colnames(LITH_A),colnames(LITH_B)),with=FALSE]
DATA <- rbindlist(list(LITH_A,LITH_B)) |> unique()
rm(LITH_A,LITH_B)
# Reshape the Data
# Coerce to character (melt requirements)
XCOLS <- c("SampleID","SetID","isDrillhole")
COLS <- setdiff(colnames(DATA),XCOLS)
DATA[, (COLS) := lapply(.SD, as.character),.SDcols = COLS]

ivars <- c("SampleID","SetID","isDrillhole")
mvars <- colnames(DATA[, - c("SampleID","SetID","isDrillhole")])
AUX <- data.table::melt(DATA, id.vars = ivars, measure.vars = mvars,variable.name = "ElementID",value.name="ElementValue") |> na.omit()

# Identify concentrations below the instrument detection limits (IDL)
AUX[,BDL:=grepl(ElementValue,pattern="^<")]
AUX[BDL == TRUE, ElementValue := sub("^<", "", ElementValue)]


# Identify concentrations above the instrument detection limits (ADL)
AUX[grepl(ElementValue,pattern="^>"), ElementValue := sub("^>", "", ElementValue)]

# Record Detection Limits
AUX[BDL == TRUE, DL := ElementValue,by=.(SampleID,ElementID,SetID)]

# Identify ElementValue<DL and BDL==FALSE and set BDL:=TRUE
AUX[ElementValue<=DL & BDL==FALSE, BDL:=TRUE,by=.(SampleID,ElementID,SetID)]

# Replace non-numeric values by 0
AUX[grepl("^[^0-9.-]*$", ElementValue),ElementValue:=NA]
# Convert to numeric
# AUX[is.na(as.numeric(ElementValue))] ?
AUX[,ElementValue:=as.double(ElementValue)] 
AUX[,DL:=as.double(DL)] 




# ******************************************************************************
# Aggregate data. Remove SetID. Average parameters 
AUX <- AUX[,.(ElementValue=mean(ElementValue)),by=.(SampleID,ElementID,isDrillhole,BDL)] 

# Check that there are not elements with the same SampleID
AUX[,.(n=.N),by=.(SampleID,ElementID,isDrillhole,BDL)][n>1]


LGL <- AUX |> unique()



# Fix SampleID codes. 
# Remove SAS prefix (there are no samples with SAS-)
# Remove Depth suffix
# remove trailing spaces

# LGL[,SampleID:=gsub("\\bSAS[ -]", "", SampleID)]
LGL[,SampleID:=gsub(" \\d+\\.\\d+M", "", SampleID)]
LGL[,SampleID:=gsub(" \\d+\\.\\d+FT", "", SampleID)]
LGL[,SampleID:=gsub(" *[[:space:]]+.*", "", SampleID)]
LGL[,SampleID:=gsub("[-.]", "", SampleID)]

LGL[,SampleID:=toupper(SampleID)]



# SAS[- ]?: This regular expression matches “SAS” followed by either a dash (-) or a space (indicated by the [- ]?).
# \\d+\\.\\d+M: This pattern matches a numeric suffix (one or more digits \\d+, a decimal \\., more digits \\d+, followed by “M”).
# gsub(): Replaces the matched patterns with an empty string.
# rimws(): Removes any extra whitespace that might be left after the replacement.



# Save long data
fwrite(LGL, "data/LGL.csv")



