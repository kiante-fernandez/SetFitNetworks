# bakkour_rating_network.R - conducts a bootstrap Exploratory Graph Analysis
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
library(readr)
library(tidyverse)
suppressMessages(library(EGAnet)) # For Exploratory Graph Analysis
library(igraph)
library(RColorBrewer)

# Helper function ----
calculate_net_stats <- function(g) {
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
  
  net_degree$snack_type <- V(g)$snack_type
  
  return(net_degree)
}

# Data Loading and Processing ----
# Load experimental data
Exp1B <- read_csv("~/Downloads/mem_dm_share-main/data/Exp1B.csv") %>% 
  filter(ttype == "rating_task") 
Exp2C <- read_csv("~/Downloads/mem_dm_share-main/data/Exp2C.csv") %>% 
  filter(ttype == "rating_task") 
image_word <- read_csv("~/Downloads/mem_dm_share-main/data/image_word.csv")

# Process and combine data
merge_image <- Exp1B %>%
  mutate(url = image) %>%
  left_join(image_word, by = "url") %>%
  select(ID, response, food.item)
merge_image$exp <- 1
merge_image$ID <- merge_image$ID + 100

merge_word <- Exp2C %>%
  mutate(food.item = word) %>%
  select(ID, response, food.item)
merge_word$exp <- 2
merge_word$ID <- merge_word$ID + 200

merged_ratings <- rbind(merge_image, merge_word) %>%
  mutate(
    food.item = gsub("\\s", "", food.item), # Remove all spaces
    food.item = tolower(food.item) # Convert to lowercase
  ) %>%
  pivot_wider(
    names_from = food.item,
    values_from = response
  ) %>% 
  select(-ID, -exp) %>%
  mutate_at(vars(everything()), as.numeric)

# EGA Analysis ----
if (!file.exists(here::here("data", "bakkour_rating_network_graphV4.RData"))) {
  # Run first stage EGA
  ega_res <- EGAnet::bootEGA(
    merged_ratings,
    iter = 10000,
    n = nrow(merged_ratings),
    model = "TMFG",
    algorithm = "walktrap",
    type = "parametric",
    ncores = 10,
    typicalStructure = TRUE
  )
  
  # Conduct stability analysis
  bapq.dimstab <- dimensionStability(ega_res)
  bapq.dimstab$dimension.stability$structural.consistency
  bapq.dimstab$dimension.stability$average.item.stability
  
  # Get stable items
  test <- bapq.dimstab$item.stability
  stable_items <- names(test$item.stability$empirical.dimensions[
    test[["item.stability"]][["empirical.dimensions"]] > .50
  ])
  
  # Run second stage EGA
  ega_rating_res <- EGAnet::bootEGA(
    merged_ratings[,stable_items],
    iter = 10000,
    n = nrow(merged_ratings),
    model = "glasso",
    algorithm = "walktrap",
    type = "parametric",
    ncores = 10,
    typicalStructure = TRUE
  )
  
  # Create network
  A <- ega_rating_res[["typicalGraph"]][["graph"]]
  dimattributes <- ega_rating_res[["typicalGraph"]][["wc"]]
  g <- igraph::graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
  igraph::V(g)$snack_type <- dimattributes
  
  # Save results
  save(ega_rating_res, g, file = here::here("data", "bakkour_rating_network_graphV4.RData"))
} else {
  load(file = here::here("data", "bakkour_rating_network_graphV4.RData"))
}

# Stability Analysis ----
bapq.dimstab <- dimensionStability(ega_rating_res)
bapq.dimstab$dimension.stability$structural.consistency
bapq.dimstab$dimension.stability$average.item.stability

# Create stability plot
bapq.dimstab$item.stability$plot +
  ggplot2::scale_color_brewer(palette = "Set3")

# Create stability dataframe
bakkour_item_stability <- data.frame(
  bapq.dimstab[["item.stability"]][["item.stability"]][["all.dimensions"]]
)
bakkour_item_stability <- tibble::rownames_to_column(bakkour_item_stability, var = "Item")

# Format stability results
new_names <- paste0(1:12)
colnames(bakkour_item_stability)[-1] <- new_names
bakkour_item_stability[, -1] <- lapply(
  bakkour_item_stability[, -1],
  function(x) if(is.numeric(x)) round(x, 3) else x
)
item_order <- names(bapq.dimstab$item.stability$membership$structure[
  order(bapq.dimstab$item.stability$membership$structure)
])
bakkour_item_stability <- bakkour_item_stability[match(item_order, bakkour_item_stability$Item),]

# Save stability results
# write_csv(bakkour_item_stability, "data/bakkour_item_stability.csv")

# Network Visualization  ----
G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
net_degree <- calculate_net_stats(g)
l <- layout_with_graphopt(g)

# Add colors to network
net_degree <- net_degree %>%
  mutate(colors = case_when(
    snack_type == 1 ~ "#7FC97F",
    snack_type == 2 ~ "#BEAED4",
    snack_type == 3 ~ "#FDC086",
    snack_type == 4 ~ "#666666",
    snack_type == 5 ~ "#386CB0",
    snack_type == 6 ~ "#F0027F",
    snack_type == 7 ~ "#BF5B17"
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
     vertex.label.dist = 1,
     vertex.label.cex = 1,
     vertex.size = 9,
     vertex.label.family = "Times",
     edge.width = E(g)$weight * 3.7)

legend(x = 1.3,
       y = .6,
       c("fruits", "sweets", "cheese", "veggie", "meat", "protein", "dairy"),
       pch = 21,
       pt.bg = c("#7FC97F", "#BEAED4", "#FDC086", "#666666", 
                 "#386CB0", "#F0027F", "#BF5B17"),
       pt.cex = 2.5,
       cex = 1.5,
       bty = "n",
       ncol = 1)

# Plot with labels
plot(g,
     layout = l,
     vertex.shape = "none",
     vertex.label.cex = .7,
     vertex.label = V(g)$name,
     vertex.label.font = 2,
     vertex.label.color = net_degree$colors,
     vertex.size = NULL,
     vertex.label.family = "Times",
     edge.width = E(g)$weight)

legend(x = 1.3,
       y = .6,
       c("fruits", "sweets", "cheese", "veggie", "meat", "protein", "dairy"),
       pch = 21,
       pt.bg = c("#7FC97F", "#BEAED4", "#FDC086", "#666666", 
                 "#386CB0", "#F0027F", "#BF5B17"),
       pt.cex = 2,
       cex = .8,
       bty = "n",
       ncol = 1)
