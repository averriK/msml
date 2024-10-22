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


# Function to remove outliers using IQR method
remove_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  x[x >= lower_bound & x <= upper_bound]
}

# Function to determine the break point for a ~50/50 split
get_break <- function(values) {
  # Remove zeros
  values <- values[values > 0]
  
  # Sort values
  sorted_values <- sort(values)
  
  # Find the index that splits the data into two equal parts
  split_index <- ceiling(length(sorted_values) / 2)
  
  # Use the value at this index as the break point
  break_value <- sorted_values[split_index]
  
  return(break_value)
}

# Merge Yo with LGL to get the corresponding DL for each sample
Yo <- merge(Yo, LGL[, .(ElementID, SampleID, DL)], by = c("ElementID", "SampleID"))

# Create reference table with break point, considering ElementID
YoCategory <- Yo[, {
  cat("Processing ElementID:", unique(ElementID), "\n")
  break_value <- get_break(ElementValue)
  cat("Break value:", break_value, "\n")
  .(Break = round(break_value, 3))
}, by = ElementID]

# Function to classify based on break point
classify_element <- function(value, Break) {
  ifelse(value < Break, "L", "H")  # Changed <= to < to handle ties at the break point
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
  cat(sprintf("L: x < %.3f (n = %d, %.2f%%)\n", 
              element_category$Break, element_category$nL, 
              100 * element_category$nL / total_samples))
  cat(sprintf("H: x >= %.3f (n = %d, %.2f%%)\n", 
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




