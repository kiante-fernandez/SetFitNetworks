# centrality_validation_analysis.R - Cross-network correspondence of centrality measures (Lee vs Fernandez)
#
#
# Copyright (C) 2024 Kianté Fernandez, <kiantefernan@gmail.com>

library(here)
library(tidyverse)
library(igraph)
library(patchwork)
library(broom)

# =============================================================================
# PART 1: CROSS-NETWORK CORRESPONDENCE (Lee vs Fernandez)
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("PART 1: CROSS-NETWORK CORRESPONDENCE\n")
cat(strrep("=", 70), "\n\n")

# Load networks ----
cat("Loading networks...\n")
load(here::here("data", "rating_network_graph.RData"))
lee_g <- g
ega_lee <- ega_res

load(here::here("data", "fernandez_rating_network_graph.RData"))
fernandez_g <- g
ega_fernandez <- ega_res

# Get food names
FoodNames <- readxl::read_excel(here::here("data", "snackitemnames_nicholas",
                                            "item_image_numbers_exp2_5_nicholas.xlsx"))

# Helper function for centrality
calculate_centrality <- function(g) {
  G <- g
  E(G)$weight <- 2^((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))

  data.frame(
    Name = V(g)$name,
    degree = degree(g, normalized = TRUE),
    strength = strength(g),
    eigen = igraph::eigen_centrality(G)$vector,
    weighted_transitivity = transitivity(g, type = "weighted"),
    closeness = igraph::closeness(G, normalized = TRUE, cutoff = -1),
    betweenness = betweenness(G, normalized = TRUE),
    snack_type = V(g)$snack_type
  )
}

# Compute centrality for both networks
V(lee_g)$name <- FoodNames$Name
V(fernandez_g)$name <- FoodNames$Name

lee_stats <- calculate_centrality(lee_g)
fernandez_stats <- calculate_centrality(fernandez_g)

# Compute PCA for both networks
compute_pca <- function(stats) {
  pca_data <- stats[, c("degree", "strength", "eigen", "weighted_transitivity",
                        "closeness", "betweenness")]
  pca_res <- prcomp(pca_data, center = TRUE, scale. = TRUE)
  stats$PCA1 <- pca_res$x[, 1]
  stats$PCA2 <- pca_res$x[, 2]
  list(stats = stats, pca = pca_res)
}

lee_pca <- compute_pca(lee_stats)
fernandez_pca <- compute_pca(fernandez_stats)

lee_stats <- lee_pca$stats
fernandez_stats <- fernandez_pca$stats

# Network descriptives ----
cat("\n--- Network Descriptives ---\n")
cat("Lee network: ", vcount(lee_g), " items, ", ecount(lee_g), " edges\n", sep = "")
cat("Fernandez network: ", vcount(fernandez_g), " items, ", ecount(fernandez_g), " edges\n", sep = "")

# Merge for comparison
merged_networks <- lee_stats %>%
  select(Name, degree, strength, eigen, weighted_transitivity,
         closeness, betweenness, PCA1, PCA2) %>%
  rename_with(~paste0("lee_", .), -Name) %>%
  inner_join(
    fernandez_stats %>%
      select(Name, degree, strength, eigen, weighted_transitivity,
             closeness, betweenness, PCA1, PCA2) %>%
      rename_with(~paste0("fernandez_", .), -Name),
    by = "Name"
  )

cat("Matched items: ", nrow(merged_networks), "\n\n")

# Cross-network correlations ----
cat("--- Cross-Network Correlations (Lee vs Fernandez) ---\n\n")

measures <- c("degree", "strength", "eigen", "weighted_transitivity",
              "closeness", "betweenness", "PCA1", "PCA2")

cross_network_cors <- map_dfr(measures, function(m) {
  lee_col <- paste0("lee_", m)
  fern_col <- paste0("fernandez_", m)
  test <- cor.test(merged_networks[[lee_col]], merged_networks[[fern_col]])
  tibble(
    measure = m,
    r = test$estimate,
    ci_lower = test$conf.int[1],
    ci_upper = test$conf.int[2],
    p_value = test$p.value,
    n = nrow(merged_networks)
  )
})

print(cross_network_cors %>%
        mutate(across(c(r, ci_lower, ci_upper), ~round(., 3)),
               p_value = format(p_value, digits = 3, scientific = TRUE),
               sig = case_when(
                 as.numeric(p_value) < 0.001 ~ "***",
                 as.numeric(p_value) < 0.01 ~ "**",
                 as.numeric(p_value) < 0.05 ~ "*",
                 TRUE ~ ""
               )))

# Create cross-network figure ----
cat("\n--- Creating Cross-Network Correspondence Figure ---\n")

create_correspondence_plot <- function(df, lee_var, fern_var, title) {
  r_val <- cor(df[[lee_var]], df[[fern_var]])
  p_val <- cor.test(df[[lee_var]], df[[fern_var]])$p.value

  # Calculate common axis range for equal scales
  all_vals <- c(df[[lee_var]], df[[fern_var]])
  axis_min <- min(all_vals, na.rm = TRUE)
  axis_max <- max(all_vals, na.rm = TRUE)
  axis_range <- axis_max - axis_min
  axis_min <- axis_min - 0.05 * axis_range
  axis_max <- axis_max + 0.05 * axis_range

  ggplot(df, aes(x = .data[[lee_var]], y = .data[[fern_var]])) +
    geom_abline(slope = 1, intercept = 0, color = "gray70", linetype = "solid", linewidth = 0.8) +
    geom_point(size = 2.5, alpha = 0.7, color = "#377EB8") +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", linewidth = 0.8) +
    coord_fixed(ratio = 1, xlim = c(axis_min, axis_max), ylim = c(axis_min, axis_max)) +
    labs(
      title = title,
      subtitle = sprintf("r = %.2f, p %s", r_val,
                         ifelse(p_val < 0.001, "< .001", sprintf("= %.3f", p_val))),
      x = "Lee Network",
      y = "Fernandez Network"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9),
      aspect.ratio = 1
    )
}

p_strength <- create_correspondence_plot(merged_networks, "lee_strength", "fernandez_strength", "Strength")
p_eigen <- create_correspondence_plot(merged_networks, "lee_eigen", "fernandez_eigen", "Eigenvector")
p_trans <- create_correspondence_plot(merged_networks, "lee_weighted_transitivity", "fernandez_weighted_transitivity", "Transitivity")
p_pca2 <- create_correspondence_plot(merged_networks, "lee_PCA2", "fernandez_PCA2", "PCA2")

correspondence_fig <- (p_strength + p_eigen) / (p_trans + p_pca2) +
  plot_annotation(
    title = "Cross-Network Correspondence: Lee vs Fernandez",
    subtitle = "Same 60 food items rated by independent participant samples (Lee: n=267, Fernandez: n=184)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10)
    )
  )

ggsave(here::here("output", "cross_network_correspondence.pdf"),
       correspondence_fig, width = 10, height = 9)

write_csv(cross_network_cors, here::here("output", "table_cross_network_correspondence.csv"))
cat("Saved: output/table_cross_network_correspondence.csv\n")
