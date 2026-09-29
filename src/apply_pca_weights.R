# src/apply_pca_weights.R
# Contains a function to apply a canonical set of PCA weights to new data.

library(tidyverse)
library(here)

#' Apply Canonical PCA Weights to New Data
#'
#' This function takes a dataframe of centrality scores and applies a pre-defined
#' set of PCA loadings to calculate consistent PC scores. This ensures that PC1, PC2, etc.,
#' have the same meaning and direction across different datasets.
#'
#' @param new_data A dataframe with columns for centrality measures (e.g., 'degree', 'strength').
#'                 The column names must match the row names of `reference_loadings`.
#' @param reference_loadings A matrix of PCA loadings (p measures x k components) to apply.
#'
#' @return The original `new_data` dataframe with new columns for PC scores (e.g., PC1, PC2).

apply_pca_weights <- function(new_data, reference_loadings) {
  # Get the names of the measures from the loading matrix
  pca_cols <- rownames(reference_loadings)
  
  # Ensure all required columns exist in the new data
  if (!all(pca_cols %in% names(new_data))) {
    missing_cols <- pca_cols[!pca_cols %in% names(new_data)]
    stop("The following required columns are missing from 'new_data': ", 
         paste(missing_cols, collapse = ", "))
  }
  
  # 1. Select the relevant columns in the correct order
  data_to_transform <- new_data[, pca_cols]
  
  # 2. Standardize the data (scale to mean=0, sd=1)
  scaled_data <- scale(data_to_transform)
  
  # Handle cases where a column has zero variance after filtering, which results in NaNs
  if (any(is.nan(scaled_data))) {
      scaled_data[is.nan(scaled_data)] <- 0
      warning("NaNs produced during scaling (likely due to zero variance in a column). Replaced with 0.", call. = FALSE)
  }
  
  # 3. Apply loadings via matrix multiplication
  # Result is a matrix of n_samples x k_components
  pc_scores <- scaled_data %*% reference_loadings
  
  # 4. Combine with original data and return
  pc_scores_df <- as_tibble(pc_scores)
  
  # Add an informative prefix to the new columns
  names(pc_scores_df) <- paste0("PC", 1:ncol(pc_scores_df))
  
  bind_cols(new_data, pc_scores_df)
}


# =============================================================================
# EXAMPLE USAGE
# This section demonstrates how to use the function.
# It will only run if the script is executed directly via `Rscript`.
# =============================================================================
if (sys.nframe() == 0) {
  
  # This example requires igraph and readxl.
  # If you get an error, run: install.packages(c("igraph", "readxl"))
  library(igraph)
  library(readxl)

  cat("---", "Example: Applying Canonical Weights to Lee Dataset ---
\n")

  # --- 1. Load the Canonical PCA Loadings ---
  cat("Loading canonical PCA weights from 'output/canonical_pca_loadings.rds'...
")
  canonical_loadings_file <- here("output", "canonical_pca_loadings.rds")
  if (!file.exists(canonical_loadings_file)) {
    stop("Canonical loadings file not found. Please run 'src/create_canonical_loadings.R' first.")
  }
  canonical_loadings <- readRDS(canonical_loadings_file)
  
  # --- 2. Load and Prepare the "New" Data (Lee dataset) ---
  cat("\nLoading and preparing Lee dataset...\n")
  
  # Centrality calculation function (copied from previous script)
  calculate_centrality <- function(g) {
      g_dist <- g
      E(g_dist)$weight <- abs(E(g)$weight)
      net_stats <- data.frame(Name = V(g)$name, degree = degree(g, normalized = TRUE),
                              strength = strength(g, weights = E(g)$weight),
                              eigen = igraph::eigen_centrality(g_dist)$vector,
                              transitivity = transitivity(g, type = "local", isolates = "zero"),
                              closeness = igraph::closeness(g_dist, normalized = TRUE), 
                              betweenness = betweenness(g_dist, normalized = TRUE))
      return(net_stats)
  }
  
  # Load the Lee graph
  lee_graph_file <- here("data", "rating_network_graph.RData")
  g_env <- new.env()
  load(lee_graph_file, envir = g_env)
  g <- g_env$g
  g <- igraph::upgrade_graph(g)
  
  # Load item names
  lee_item_names_file <- here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx")
  FoodNames <- readxl::read_excel(lee_item_names_file)
  V(g)$name <- FoodNames$Name
  
  # Calculate centrality for Lee data
  lee_centrality <- calculate_centrality(g)
  
  # --- 3. Apply the function ---
  cat("\nApplying the canonical weights to the Lee centrality data...\n")
  lee_data_with_pcs <- apply_pca_weights(lee_centrality, canonical_loadings)
  
  # --- 4. Show the result (with robust printing) ---
  cat("\nResult: The 'lee_centrality' data now has consistent PC scores:\n")
  final_df_to_print <- head(lee_data_with_pcs[, c("Name", "degree", "strength", "PC1", "PC2")], 10)
  final_df_to_print <- final_df_to_print %>% mutate(across(where(is.numeric), round, 4))
  print(as.data.frame(final_df_to_print))

}