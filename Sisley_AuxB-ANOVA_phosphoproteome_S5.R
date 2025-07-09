# Clear the workspace
rm(list = ls())

# Set a random seed for reproducibility
set.seed(12345)

# Load required libraries
library(readxl)
library(openxlsx)
library(stats)

# Define file paths
phosphosite_file <- "/Users/tylersisley/Documents/ANOVA R scripts/Supplementary Table S5.xlsx"

# Check if file exists
if (!file.exists(phosphosite_file)) {
  stop("Input file does not exist.")
}


# Read the different parts of the Excel sheet
data_part1 <- read_excel(phosphosite_file, sheet = "Phosphosite", range = "A2:B58")
data_part2 <- read_excel(phosphosite_file, sheet = "Phosphosite", range = "K2:Z58")
output_file <- "/Users/tylersisley/Documents/ANOVA R scripts/Table_S5_phosphosite_TukeyHSD.xlsx"


# Check the dimensions of the data frames
print(paste("Dimensions of data_part1:", paste(dim(data_part1), collapse = " x ")))
print(paste("Dimensions of data_part2:", paste(dim(data_part2), collapse = " x ")))

# Define a function to perform one-way ANOVA for each row
row_anova <- function(row) {
  row <- as.numeric(row)  # Ensure row is numeric
  num_samples <- length(row)  # Get number of samples
  
  if (num_samples %% 4 != 0) {
    stop("Error: The number of samples is not evenly divisible by 4 groups.")
  }
  
  groups <- factor(rep(1:4, each = num_samples / 4))  # Define groups
  res <- aov(row ~ groups)
  return(res)
}

# Apply ANOVA to each row
anova_results <- apply(data_part2, 1, row_anova)

# Extract p-values from ANOVA results
anova_p_values <- sapply(anova_results, function(x) summary(x)[[1]][["Pr(>F)"]][1])

# Perform Tukey’s HSD for **all proteins**, even if ANOVA is not significant
tukey_results <- lapply(anova_results, function(x) {
  tryCatch(TukeyHSD(x)[[1]][, "p adj"], error = function(e) rep(NA, 6))
})

# Convert list of p-values into a matrix
tukey_p_values_matrix <- do.call(rbind, tukey_results)

# Ensure row count matches input data
if (nrow(tukey_p_values_matrix) != nrow(data_part2)) {
  stop("Error: Output matrix does not match the original data size.")
}

# Write Tukey's HSD results to an Excel file
write.xlsx(as.data.frame(tukey_p_values_matrix), file = output_file)

print("Analysis complete! Tukey's HSD results saved.")