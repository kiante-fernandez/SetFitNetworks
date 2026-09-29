# thomas2021_network.R - Build preference network for Thomas2021 items
#
# This script builds a preference network from independent samples in the
# liking-rating-database for items that appear in Thomas2021/thomolt.
#
# Studies used: bakbot, smikrab, gwikrab, gwileb, smikrab2018
# These are independent from the 49 Thomas2021 choice study participants.
#
# Copyright (C) 2025 Kianté Fernandez
# License: GPL-3

# Libraries ----------------------------------------------------------------
library(tidyverse)
library(here)
suppressMessages(library(EGAnet))
library(igraph)

# Configuration ------------------------------------------------------------
set.seed(2025)
ITER_BOOTEGA <- 10000
NCORES <- 10

# Load thomolt items (the 78 items used in Thomas2021) ---------------------
thomolt_items <- read_csv(here::here("data", "liking_initiative", "final_database.csv"),
                          show_col_types = FALSE) %>%
  filter(grepl("^thomolt_", dataset_subjectid)) %>%
  pull(item_name) %>%
  unique()

cat("Thomas2021/thomolt items:", length(thomolt_items), "\n")

# Load database and filter to network-building studies ---------------------
database <- read_csv(here::here("data", "liking_initiative", "final_database.csv"),
                     show_col_types = FALSE)

# Studies to use for network building (independent from choice study)
network_studies <- c("bakbot", "smikrab", "gwikrab", "gwileb", "smikrab2018")

# Filter to network-building studies and thomolt items
network_data <- database %>%
  mutate(study = str_extract(dataset_subjectid, "^[^_]+")) %>%
  filter(study %in% network_studies) %>%
  filter(item_name %in% thomolt_items)

cat("Network data:", nrow(network_data), "ratings\n")
cat("Studies:", paste(unique(network_data$study), collapse = ", "), "\n")

# Check item coverage
items_in_network <- unique(network_data$item_name)
cat("Items with ratings:", length(items_in_network), "/", length(thomolt_items), "\n")

# Create subject × item rating matrix --------------------------------------
# Need to handle different rating scales across studies
# Convert all to z-scores within study before combining

network_data_z <- network_data %>%
  group_by(study) %>%
  mutate(rating_z = scale(rating)[,1]) %>%
  ungroup()

# Create matrix (use mean if multiple ratings per subject-item)
rating_matrix <- network_data_z %>%
  select(dataset_subjectid, item_name, rating_z) %>%
  group_by(dataset_subjectid, item_name) %>%
  summarise(rating = mean(rating_z, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = dataset_subjectid,
    names_from = item_name,
    values_from = rating
  ) %>%
  column_to_rownames("dataset_subjectid")

cat("Rating matrix:", nrow(rating_matrix), "subjects ×", ncol(rating_matrix), "items\n")

# Check for items with too few ratings
item_counts <- colSums(!is.na(rating_matrix))
cat("Min ratings per item:", min(item_counts), "\n")
cat("Max ratings per item:", max(item_counts), "\n")

# Remove items with very few ratings (< 10)
valid_items <- names(item_counts)[item_counts >= 10]
rating_matrix <- rating_matrix[, valid_items]
cat("Items after filtering:", ncol(rating_matrix), "\n")

# Handle missing values by imputing with column means
# This is necessary because bootEGA needs complete data
missing_pct <- mean(is.na(rating_matrix)) * 100
cat("Missing data:", round(missing_pct, 1), "%\n")

# Impute missing values with column means
rating_matrix_complete <- rating_matrix
for (j in 1:ncol(rating_matrix_complete)) {
  col_mean <- mean(rating_matrix_complete[, j], na.rm = TRUE)
  rating_matrix_complete[is.na(rating_matrix_complete[, j]), j] <- col_mean
}

# Verify no missing values remain
cat("Missing after imputation:", sum(is.na(rating_matrix_complete)), "\n")

# Run bootEGA --------------------------------------------------------------
cat("\nRunning bootEGA (this may take a while)...\n")

if (!file.exists(here("data", "thomas2021_network.RData"))) {

  ega_res <- EGAnet::bootEGA(
    rating_matrix_complete,
    iter = ITER_BOOTEGA,
    n = nrow(rating_matrix_complete),
    model = "glasso",
    algorithm = "walktrap",
    ncores = NCORES,
    typicalStructure = TRUE
  )

  # Get adjacency matrix from typical structure
  A <- ega_res[["typicalGraph"]][["graph"]]

  # Get cluster memberships
  dimattributes <- ega_res[["typicalGraph"]][["wc"]]

  # Create igraph object
  g <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
  V(g)$snack_type <- dimattributes

  # Save intermediate results
  save(ega_res, g, file = here("data", "thomas2021_network.RData"))

} else {
  cat("Loading existing network from data/thomas2021_network.RData\n")
  load(here("data", "thomas2021_network.RData"))
}

# Calculate centrality metrics ---------------------------------------------
cat("\nCalculating centrality metrics...\n")

# Transform edge weights for centrality calculations
G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))

# Calculate centrality metrics
net_stats <- data.frame(
  Name = V(g)$name,
  degree = degree(g, normalized = TRUE),
  strength = strength(g),
  eigen = igraph::eigen_centrality(G)$vector,
  weighted_transitivity = transitivity(g, type = "weighted"),
  closeness = igraph::closeness(G, normalized = TRUE, cutoff = -1),
  betweenness = betweenness(G, normalized = TRUE),
  snack_type = V(g)$snack_type
)

# Check for NA/Inf values in centrality metrics
cat("Checking centrality metrics for NA/Inf values...\n")
for (col in c("degree", "strength", "eigen", "weighted_transitivity", "closeness", "betweenness")) {
  n_na <- sum(is.na(net_stats[[col]]))
  n_inf <- sum(is.infinite(net_stats[[col]]))
  if (n_na > 0 || n_inf > 0) {
    cat("  ", col, ": NA=", n_na, ", Inf=", n_inf, "\n")
  }
}

# Replace NA/Inf with 0 for centrality metrics
for (col in c("degree", "strength", "eigen", "weighted_transitivity", "closeness", "betweenness")) {
  net_stats[[col]][is.na(net_stats[[col]])] <- 0
  net_stats[[col]][is.infinite(net_stats[[col]])] <- 0
}

# Run PCA on centrality metrics --------------------------------------------
cat("Running PCA on centrality metrics...\n")

pca_data <- net_stats[, c("degree", "strength", "eigen",
                          "weighted_transitivity", "closeness", "betweenness")]
rownames(pca_data) <- net_stats$Name

# Check for zero variance columns (can't scale)
col_vars <- apply(pca_data, 2, var)
if (any(col_vars == 0)) {
  cat("Warning: Zero variance columns found. Removing:",
      names(col_vars)[col_vars == 0], "\n")
  pca_data <- pca_data[, col_vars > 0]
}

pca_res <- prcomp(pca_data, center = TRUE, scale. = TRUE)
print(summary(pca_res))

# Add PCA scores to network stats (handle variable number of components)
n_pcs <- min(ncol(pca_res$x), 6)
for (i in 1:n_pcs) {
  net_stats[[paste0("PCA", i)]] <- pca_res$x[, i]
}
# Fill in remaining PC columns with 0 if fewer than 6 components
for (i in (n_pcs + 1):6) {
  net_stats[[paste0("PCA", i)]] <- 0
}

# Save final network stats -------------------------------------------------
write_csv(net_stats, here("data", "thomas2021_network_stats.csv"))

# Also save to RData with all objects
save(ega_res, g, net_stats, pca_res, file = here("data", "thomas2021_network.RData"))

cat("\n=== Network Summary ===\n")
cat("Nodes:", vcount(g), "\n")
cat("Edges:", ecount(g), "\n")
cat("Communities:", max(V(g)$snack_type), "\n")
cat("Density:", edge_density(g), "\n")

cat("\n=== PCA Variance Explained ===\n")
print(summary(pca_res)$importance[, 1:4])

cat("\n=== Sample Centrality Scores (first 10) ===\n")
print(head(net_stats[, c("Name", "strength", "PCA1", "PCA2")], 10))

cat("\nNetwork saved to: data/thomas2021_network.RData\n")
cat("Network stats saved to: data/thomas2021_network_stats.csv\n")
