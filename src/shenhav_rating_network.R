# shenhav_rating_network.R - conducts a bootstrap Exploratory Graph Analysis
#
# Copyright (C) 2024 Kianté Fernandez, <kiantefernan@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Record of Revisions
#
# Date            Programmers                         Descriptions of Change
# ====         ================                       ======================
# 2024/01/17    Kianté Fernandez                       wrote code

# Load required libraries ----
library(tidyverse)
suppressMessages(library(EGAnet)) # For Exploratory Graph Analysis
library(igraph)
library(RColorBrewer)

# Helper function for network statistics ----
calculate_net_stats <- function(g) {
  ## another version of calculating the netstats of various measures for a graph
  G <- g
  E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
  path_lengths <- distances(G)
  diag(path_lengths) <- NA # path length to oneself is zero
  
  adj_temp <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")
  
  # here I calculate a range of metrics on the graph
  net_degree <- data.frame(
    degree = degree(g, normalized = TRUE),
    strength = strength(g),
    eigen = igraph::eigen_centrality(G)$vector,
    weighted_transitivity = transitivity(g, type = "weighted"),
    closeness = igraph::closeness.estimate(G, normalized = TRUE, cutoff = -1),
    betweenness = betweenness(G, normalized = TRUE)
  ) %>% tibble::rownames_to_column("Name")
  
  net_degree$product_type <- V(g)$product_type
  
  return(net_degree)
}

# Data Loading and Processing ----
# Load new data provided by Jason
shengav_rating <- read_csv("data/shengav_rating.csv")
shenhav_item_list <- read_csv("data/shenhav_item_list.csv")

# Process ratings data
ratings <- shengav_rating %>%
  left_join(shenhav_item_list, by = "pic_path") %>%
  group_by(participant, pic_name) %>%
  summarise(value = mean(value), .groups = 'drop') %>% # average the duplicates
  select(participant, pic_name, value) %>%
  pivot_wider(
    names_from = pic_name,  # Create columns from pic_name
    values_from = value     # Fill the cells with the values from the value column
  ) %>% 
  select(-participant)

colnames(ratings) <- tolower(gsub(" ", "_", colnames(ratings)))

# Stage 1: Initial EGA Analysis ----
# Check if the file exists and run first stage EGA if needed
if (!file.exists(here::here("data", "shenhav_rating_network_graphFULL.RData"))) {
  ega_res <- EGAnet::bootEGA(
    ratings,
    iter = 10000,
    n = nrow(ratings),
    model = "glasso",
    algorithm = "walktrap",
    type = "parametric",
    ncores = 10,
    typicalStructure = TRUE
  )
  
  A <- ega_res[["typicalGraph"]][["graph"]]
  dimattributes <- ega_res[["typicalGraph"]][["wc"]]
  g <- igraph::graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
  igraph::V(g)$snack_type <- dimattributes
  
  save(ega_res, g, file = here::here("data", "shenhav_rating_network_graphFULL.RData"))
} else {
  load(file = here::here("data", "shenhav_rating_network_graphFULL.RData"))
}

# Stability Analysis ----
bapq.dimstab <- dimensionStability(ega_res)
bapq.dimstab$dimension.stability$structural.consistency
bapq.dimstab$dimension.stability$average.item.stability
bapq.dimstab$item.stability$plot +
  ggplot2::scale_color_brewer(palette = "Set3")

# Get stable items
test <- bapq.dimstab$item.stability
stable_items <- names(test$item.stability$empirical.dimensions[
  test[["item.stability"]][["empirical.dimensions"]] > .50
])

# Stage 2: Analysis with Stable Items ----
if (!file.exists(here::here("data", "shenhav_rating_network_graphV2.RData"))) {
  # Run second stage EGA
  ega_res2 <- EGAnet::bootEGA(
    ratings[, stable_items],
    iter = 1000,
    n = nrow(ratings),
    model = "glasso",
    algorithm = "walktrap",
    type = "parametric",
    ncores = 10,
    typicalStructure = TRUE
  )
  
  save(ega_res2, g, file = here::here("data", "shenhav_rating_network_graphV2.RData"))
} else {
  load(file = here::here("data", "shenhav_rating_network_graphV2.RData"))
}

# Second Stage Stability Analysis ----
bapq.dimstab <- dimensionStability(ega_res2)
bapq.dimstab$dimension.stability$structural.consistency
bapq.dimstab$dimension.stability$average.item.stability
bapq.dimstab$item.stability$plot +
  ggplot2::scale_color_brewer(palette = "Set3")

# Create stability dataframe
shenhav_item_stability <- data.frame(
  bapq.dimstab[["item.stability"]][["item.stability"]][["all.dimensions"]]
)
shenhav_item_stability <- tibble::rownames_to_column(shenhav_item_stability, var = "Item")

# Rename columns and format
new_names <- paste0(1:21)
colnames(shenhav_item_stability)[-1] <- new_names
shenhav_item_stability[, -1] <- lapply(
  shenhav_item_stability[, -1],
  function(x) if(is.numeric(x)) round(x, 3) else x
)

# Order items
item_order <- names(bapq.dimstab$item.stability$membership$structure[
  order(bapq.dimstab$item.stability$membership$structure)
])
shenhav_item_stability <- shenhav_item_stability[match(item_order, shenhav_item_stability$Item), ]
# write_csv(shenhav_item_stability, "data/shenhav_item_stability.csv")

# Network Visualization Preparation ----
# Get adjacency matrix and create graph
A <- ega_res2[["typicalGraph"]][["graph"]]
memres <- EGAnet::community.consensus(
  A,
  consensus.method = "iterative",
  consensus.iter = 10000
)

# Get clusters and create graph
dimattributes <- ega_res2[["typicalGraph"]][["wc"]]
g <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
V(g)$product_type <- dimattributes

# Calculate network statistics
G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
net_degree <- calculate_net_stats(g)
l <- layout_with_graphopt(g)

# Add colors to network
net_degree <- net_degree %>%
  mutate(colors = case_when(
    product_type == 1 ~ "#A6CEE3",
    product_type == 2 ~ "#1F78B4",
    product_type == 3 ~ "#B2DF8A",
    product_type == 4 ~ "#FF7F00",
    product_type == 5 ~ "#6A3D9A",
    product_type == 6 ~ "#E31A1C",
    product_type == 7 ~ "#FDBF6F",
    product_type == 8 ~ "#33A02C",
    product_type == 9 ~ "#FB9A99",
    product_type == 10 ~ "#CAB2D6"
  ))

V(g)$color <- net_degree$colors
E(g)$color[E(g)$weight > 0] <- "forestgreen"
E(g)$color[E(g)$weight < 0] <- "red2"

# Plot Network Visualizations ----
# Plot without labels
plot(g,
     layout = l,
     margin = .0,
     vertex.label = NA,
     vertex.label.color = "black",
     label.font = 2,
     vertex.frame.color = adjustcolor(net_degree$colors, alpha.f = .1),
     vertex.size = 9,
     vertex.label.family = "Times",
     edge.width = E(g)$weight * 4.7)

legend(x = 1.3,
       y = .6,
       c("convenience", "kitchen", "household", "alcoholic", "recreational",
         "personal", "snack", "COVID", "baby", "exercise"),
       pch = 21,
       pt.bg = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#FF7F00", "#6A3D9A", "#E31A1C",
                 "#FDBF6F", "#33A02C", "#FB9A99", "#CAB2D6"),
       pt.cex = 4,
       cex = 2,
       bty = "n",
       ncol = 1)

# Plot with labels
plot(g,
     layout = l,
     vertex.shape = "none",
     vertex.label.cex = .9,
     vertex.label = V(g)$name,
     vertex.label.font = 2,
     vertex.label.color = net_degree$colors,
     vertex.size = NULL,
     vertex.label.family = "Times",
     edge.width = E(g)$weight)

legend(x = 1.3,
       y = .6,
       c("convenience", "kitchen", "household", "alcoholic", "recreational",
         "personal", "snack", "COVID", "baby", "exercise"),
       pch = 21,
       pt.bg = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#FF7F00", "#6A3D9A", "#E31A1C",
                 "#FDBF6F", "#33A02C", "#FB9A99", "#CAB2D6"),
       pt.cex = 2,
       cex = .8,
       bty = "n",
       ncol = 1)
