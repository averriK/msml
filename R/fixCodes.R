is_substring <- function(x, y) {
  grepl(x, y) || grepl(y, x)
}

# Step 2: Identify substring matches and fix SampleID in IDX
possible_matches <- data.frame(SampleID_IDX = character(), SampleID_DATA = character(), stringsAsFactors = FALSE)

for (x in unmatched_in_IDX) {
  for (y in unmatched_in_DATA) {
    if (is_substring(x, y)) {
      possible_matches <- rbind(possible_matches, data.frame(SampleID_IDX = x, SampleID_DATA = y))
      # Fix SampleID in IDX by replacing the entry with the match from DATA
      IDX$SampleID[IDX$SampleID == x] <- y
    }
  }
}

# Step 3: Update the vectors SampleID_IDX and SampleID_DATA after fixing SampleID in IDX
SampleID_IDX <- IDX$SampleID |> unique() |> sort()
SampleID_DATA <- DATA$SampleID |> unique() |> sort()

# Step 4: Update unmatched_in_IDX and unmatched_in_DATA
unmatched_in_IDX <- SampleID_IDX[!(SampleID_IDX %in% SampleID_DATA)]
unmatched_in_DATA <- SampleID_DATA[!(SampleID_DATA %in% SampleID_IDX)]

# Step 6: Print the results
cat("Matches based on substrings:\n")
print(possible_matches)
