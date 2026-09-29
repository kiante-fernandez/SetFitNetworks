# thomas2021_item_mapping.R - Map stimulus numbers to food names
#
# This script creates a mapping from Thomas2021 stimulus files (nr*.png)
# to food item names using rating similarity matching.
#
# The approach:
# 1. Load Thomas2021 liking ratings (49 subjects × 80 stimuli)
# 2. Load thomolt data from liking-rating-database (49 subjects × 78 items)
# 3. Match stimuli to food names by correlating rating patterns across subjects
#
# Copyright (C) 2025 Kianté Fernandez
# License: GPL-3

# Libraries ----------------------------------------------------------------
library(tidyverse)
library(here)

# Load Thomas2021 liking ratings -------------------------------------------
thomas_dir <- here::here("data", "thomas2021", "subject_files")
rating_files <- list.files(thomas_dir, pattern = "liking_ratings.csv", full.names = TRUE)

# Read all subject ratings
thomas_ratings <- map_df(rating_files, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    filter(grepl("^nr", stimulus)) %>%  # Only keep stimulus rows (filter out any parsing issues)
    select(subject, stimulus, rating)
})

# Create subject × stimulus matrix for Thomas2021
thomas_matrix <- thomas_ratings %>%
  pivot_wider(
    id_cols = subject,
    names_from = stimulus,
    values_from = rating,
    values_fn = mean  # In case of duplicates
  ) %>%
  column_to_rownames("subject")

cat("Thomas2021 ratings:", nrow(thomas_matrix), "subjects ×", ncol(thomas_matrix), "stimuli\n")

# Load thomolt data from database ------------------------------------------
database <- read_csv(here::here("data", "liking_initiative", "final_database.csv"), show_col_types = FALSE)

# Filter to thomolt study only
thomolt <- database %>%
  filter(grepl("^thomolt_", dataset_subjectid)) %>%
  mutate(
    subject = str_extract(dataset_subjectid, "(?<=thomolt_)\\d+")
  ) %>%
  select(subject, item_name, rating)

# Create subject × item matrix for thomolt
thomolt_matrix <- thomolt %>%
  pivot_wider(
    id_cols = subject,
    names_from = item_name,
    values_from = rating,
    values_fn = mean
  ) %>%
  column_to_rownames("subject")

cat("Thomolt ratings:", nrow(thomolt_matrix), "subjects ×", ncol(thomolt_matrix), "items\n")

# Match subjects between datasets ------------------------------------------
# The subjects should be the same 49 people in both datasets
# Match by subject number

thomas_subjects <- as.numeric(rownames(thomas_matrix))
thomolt_subjects <- as.numeric(rownames(thomolt_matrix))

common_subjects <- intersect(thomas_subjects, thomolt_subjects)
cat("Common subjects:", length(common_subjects), "\n")

# Align matrices to common subjects
thomas_aligned <- thomas_matrix[as.character(sort(common_subjects)), ]
thomolt_aligned <- thomolt_matrix[as.character(sort(common_subjects)), ]

# Compute correlation matrix between stimuli and food items ----------------
# For each stimulus, find the food item with highest correlation across subjects

n_stim <- ncol(thomas_aligned)
n_items <- ncol(thomolt_aligned)

cor_matrix <- matrix(NA, nrow = n_stim, ncol = n_items)
rownames(cor_matrix) <- colnames(thomas_aligned)
colnames(cor_matrix) <- colnames(thomolt_aligned)

for (i in 1:n_stim) {
  for (j in 1:n_items) {
    stim_ratings <- thomas_aligned[, i]
    item_ratings <- thomolt_aligned[, j]

    # Compute correlation (use complete cases)
    valid <- complete.cases(stim_ratings, item_ratings)
    if (sum(valid) > 10) {  # Require at least 10 valid pairs
      cor_matrix[i, j] <- cor(stim_ratings[valid], item_ratings[valid])
    }
  }
}

# Use Hungarian algorithm for optimal assignment ---------------------------
# Install if needed: install.packages("clue")
library(clue)

# Convert correlations to cost (for minimization)
# Handle NAs by setting to low correlation
cor_matrix_filled <- cor_matrix
cor_matrix_filled[is.na(cor_matrix_filled)] <- -1

# Convert to non-negative cost matrix: cost = 1 - correlation
# This way, higher correlations have lower costs
# Range: [-1, 1] -> [0, 2]
cost_matrix <- 1 - cor_matrix_filled

# Solve assignment problem
# Note: Hungarian algorithm requires square matrix, so we'll handle the size difference
if (n_stim >= n_items) {
  # More stimuli than items - pad with dummy items (high cost = bad match)
  padding <- matrix(3, nrow = n_stim, ncol = n_stim - n_items)
  cost_padded <- cbind(cost_matrix, padding)
  assignment <- solve_LSAP(cost_padded)
  assignment <- as.integer(assignment)

  # Create mapping (some stimuli will map to dummy items)
  mapping <- tibble(
    stimulus = colnames(thomas_aligned),
    item_idx = assignment,
    item_name = ifelse(assignment <= n_items, colnames(thomolt_aligned)[assignment], NA_character_),
    correlation = sapply(1:n_stim, function(i) {
      if (assignment[i] <= n_items) cor_matrix[i, assignment[i]] else NA
    })
  )
} else {
  # More items than stimuli - pad with dummy stimuli (high cost = bad match)
  padding <- matrix(3, nrow = n_items - n_stim, ncol = n_items)
  cost_padded <- rbind(cost_matrix, padding)
  assignment <- solve_LSAP(cost_padded)
  assignment <- as.integer(assignment)

  # Create mapping (only for real stimuli)
  mapping <- tibble(
    stimulus = colnames(thomas_aligned),
    item_idx = assignment[1:n_stim],
    item_name = colnames(thomolt_aligned)[assignment[1:n_stim]],
    correlation = sapply(1:n_stim, function(i) cor_matrix[i, assignment[i]])
  )
}

# Summary statistics -------------------------------------------------------
cat("\n=== Mapping Results ===\n")
cat("Mapped stimuli:", sum(!is.na(mapping$item_name)), "/", n_stim, "\n")
cat("Mean correlation:", round(mean(mapping$correlation, na.rm = TRUE), 3), "\n")
cat("Min correlation:", round(min(mapping$correlation, na.rm = TRUE), 3), "\n")
cat("Max correlation:", round(max(mapping$correlation, na.rm = TRUE), 3), "\n")

# Check for low-confidence mappings
low_conf <- mapping %>% filter(correlation < 0.5)
if (nrow(low_conf) > 0) {
  cat("\nLow confidence mappings (r < 0.5):\n")
  print(low_conf)
}

# Validation: check mean ratings match ------------------------------------
# Compute mean rating per stimulus and per item, compare mapped pairs
validation <- mapping %>%
  filter(!is.na(item_name)) %>%
  mutate(
    thomas_mean = sapply(stimulus, function(s) mean(thomas_aligned[, s], na.rm = TRUE)),
    thomolt_mean = sapply(item_name, function(i) mean(thomolt_aligned[, i], na.rm = TRUE)),
    mean_diff = abs(thomas_mean - thomolt_mean)
  )

cat("\nValidation - Mean rating differences:\n")
cat("Mean absolute difference:", round(mean(validation$mean_diff), 3), "\n")
cat("Max absolute difference:", round(max(validation$mean_diff), 3), "\n")

# Save mapping -------------------------------------------------------------
output_mapping <- mapping %>%
  select(stimulus, item_name, correlation) %>%
  arrange(stimulus)

write_csv(output_mapping, here("data", "thomas2021_stimulus_mapping.csv"))
cat("\nMapping saved to: data/thomas2021_stimulus_mapping.csv\n")

# Also save the full mapping with validation info
write_csv(validation, here("data", "thomas2021_stimulus_mapping_validated.csv"))
cat("Validated mapping saved to: data/thomas2021_stimulus_mapping_validated.csv\n")

# Print sample of the mapping
cat("\n=== Sample Mapping (first 20) ===\n")
print(head(output_mapping, 20))
