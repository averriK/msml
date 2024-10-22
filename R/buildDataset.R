# nolint start
rm(list=ls()) 
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


# Function to determine the break point for 2/3 L and 1/3 H split
get_break <- function(values) {
  sorted_values <- sort(values)
  n <- length(sorted_values)
  break_index <- ceiling(2 * n / 3)  # Index for 2/3 of the data
  sorted_values[break_index]
}

# Create reference table with break point, considering only ElementID
YoCategory <- Yo[, .(Break = round(get_break(ElementValue), 3)), by = ElementID]

# Function to classify based on break point
classify_element <- function(value, Break) {
  ifelse(value <= Break, "L", "H")
}

# Apply classification
Yo[YoCategory, on = "ElementID", 
   Class := sapply(.SD, function(x) classify_element(x, i.Break)), 
   .SDcols = "ElementValue"]

# Convert Class to factor with levels L and H
Yo[, Class := factor(Class, levels = c("L", "H"))]

# Calculate class counts
class_counts <- Yo[, .N, by = .(ElementID, Class)]

# Update YoCategory with counts
YoCategory[, c("nL", "nH") := 0]
YoCategory[class_counts[Class == "L"], on = "ElementID", nL := N]
YoCategory[class_counts[Class == "H"], on = "ElementID", nH := N]

# Print class information for each ElementID
Yo[, {
  class_counts <- table(Class)
  total_samples <- sum(class_counts)
  cat("ElementID:", unique(ElementID), "\n")
  cat("Total samples:", total_samples, "\n")
  cat("Class counts:\n")
  print(class_counts)
  element_category <- YoCategory[ElementID == .BY$ElementID]
  cat("Class boundary:\n")
  cat(sprintf("L: x <= %.3f (n = %d, %.2f%%)\n", 
              element_category$Break, element_category$nL, 
              100 * element_category$nL / total_samples))
  cat(sprintf("H: x > %.3f (n = %d, %.2f%%)\n", 
              element_category$Break, element_category$nH,
              100 * element_category$nH / total_samples))
  cat("\n")
}, by = ElementID]

# Print summary of YoCategory
print(summary(YoCategory))
print(nrow(YoCategory))

# Verify the unique classes for a specific element (e.g., Lu)
print(Yo[ElementID=="Lu"]$Class %>% unique())

# Check column integrity
names(Xo) %in% names(Xi) |> all()
fwrite(Xo,"data/Xo.Rn.csv")
fwrite(Xi,"data/Xi.Rn.csv")
fwrite(Yo,"data/Yo.Rn.csv")


# nolint end




