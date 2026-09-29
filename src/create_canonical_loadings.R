# create_canonical_loadings.R
# This script generates and saves the canonical PCA loadings from the Lee/Holyoak
# network (Rating Study 1). These loadings are then applied consistently across
# all other datasets via apply_pca_weights().

library(here)
library(igraph)

cat("Loading Lee/Holyoak network to generate canonical PCA loadings...\n")

# Load the Lee/Holyoak network graph (the original study network)
lee_graph_file <- here("data", "rating_network_graph.RData")
if (!file.exists(lee_graph_file)) {
  stop("Lee/Holyoak graph file not found at: ", lee_graph_file)
}
load(lee_graph_file)
g <- upgrade_graph(g)

# Compute centrality metrics matching calculate_net_stats() in utils.R
G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))

lee_pca_data <- data.frame(
  degree = degree(g, normalized = TRUE),
  strength = strength(g),
  eigen = igraph::eigen_centrality(G)$vector,
  transitivity = transitivity(g, type = "weighted"),
  closeness = igraph::closeness(G, normalized = TRUE, cutoff = -1),
  betweenness = betweenness(G, normalized = TRUE)
)

# Run PCA to get the loadings
lee_pca_res <- prcomp(lee_pca_data, center = TRUE, scale. = TRUE)

# The loadings are the canonical weights
canonical_loadings <- lee_pca_res$rotation

# --- Sign Flipping for Interpretability ---
# Convention (matching the main study direction from utils.R):
# - PC1: Higher values = more "general centrality", so 'degree' loading should be positive.
# - PC2: Higher values = more "local clustering", so 'strength' loading should be positive.

cat("Enforcing consistent sign convention for PC1 and PC2...\n")
if (canonical_loadings["degree", "PC1"] < 0) {
  cat("Flipping sign of PC1.\n")
  canonical_loadings[, "PC1"] <- canonical_loadings[, "PC1"] * -1
}
if (canonical_loadings["strength", "PC2"] < 0) {
  cat("Flipping sign of PC2.\n")
  canonical_loadings[, "PC2"] <- canonical_loadings[, "PC2"] * -1
}

# Save the canonical loadings object for use in other scripts
saveRDS(canonical_loadings, file = here("output", "canonical_pca_loadings.rds"))
cat("\nCanonical PCA loadings saved to output/canonical_pca_loadings.rds\n")
cat("These weights can now be used to calculate consistent PC scores for all datasets.\n\n")

cat("Final Canonical Loadings:\n")
print(canonical_loadings)
