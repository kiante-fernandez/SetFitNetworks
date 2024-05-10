# shenhav_rating_network.R - conducts a bootstrap Exploratory Graph Analysis for 
#
# Frömer, R., Dean Wolf, C.K. & Shenhav, A. 
# Goal congruency dominates reward value in accounting for behavioral 
# and neural correlates of value-based decision-making. 
# Nat Commun 10, 4926 (2019). 
# https://doi.org/10.1038/s41467-019-12931-x
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
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Record of Revisions
#
# Date            Programmers                         Descriptions of Change
# ====         ================                       ======================
# 2024/01/17      Kianté  Fernandez                       wrote code v1

# Load required libraries
library(tidyverse)
suppressMessages(library(EGAnet)) # For Exploratory Graph Analysis
library(igraph)
library(RColorBrewer)


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

# Function Definitions
# Function to clean column names by removing letters at the front, numbers, and file extensions
clean_col_names <- function(names) {
  names <- gsub("^[A-Za-z]+_?[0-9]*_", "", names) # Remove letters at the front and numbers
  names <- gsub("\\.jpeg$|\\.jpg$", "", names)    # Remove the file extension
  return(names)
}

# Data Preparation
allSubBidData <- read_csv("~/allSubBidData.csv")
ratings <- allSubBidData %>% 
  select(subject_ID, item_response, item_name) %>% 
  pivot_wider(names_from = item_name, values_from = item_response) %>% 
  ungroup() %>% select(-subject_ID) %>% 
  select(sample(ncol(.), 150))

# Data Cleaning and Imputation
ratings_cleaned <- ratings %>% select(where(~ !any(is.na(.))))
# ratings_imputed <- ratings %>% mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Column Name Cleaning
col_names <- colnames(ratings_cleaned)
cleaned_col_names <- clean_col_names(col_names)
colnames(ratings_cleaned) <- cleaned_col_names

# Remove Numeric-Only Columns
# numeric_only_cols <- sapply(cleaned_col_names, function(name) grepl("^[0-9]+$", name))
numeric_or_MJK_cols <- sapply(cleaned_col_names, function(name) grepl("^[0-9]+$", name) | grepl("^MJK", name))
ratings_cleaned <- ratings_cleaned[, !numeric_or_MJK_cols]

# Correlation and Network Analysis
cor_product <- SemNeT::similarity(na.omit(ratings_cleaned), method = "cor")
m <- as.matrix(cor_product)
mDim <- length(m[1, ])

# Visualizing Correlations
ggcorrplot::ggcorrplot(m[mDim:1, ], colors = c("red", "white", "green"), ggtheme = ggplot2::theme_classic, outline.color = "white", show.diag = TRUE) +
  ggplot2::theme(axis.text.x = element_text(size = 4), axis.text.y = element_text(size = 4), axis.ticks = element_blank(), legend.position = "left")

# Exploratory Graph Analysis (EGA)
# Check if the file exists
if (!file.exists(here::here("data", "shenhav_rating_network_graph.RData"))) {
  # Preparing Data for EGA
  shenhav_2019_rating <- ratings_cleaned
  
  # Correlation Matrix Computation and Adjustment
  # (this is a check for now given how things are correlating)
  cor_x1 <- cor(na.omit(ratings_cleaned))
  cor_x1 <- matrix(Matrix::nearPD(cor_x1, corr = TRUE, maxit = 500)$mat,   ncol(ratings_cleaned))
  cor_x1 <- (cor_x1 + t(cor_x1)) / 2 # Make symmetric
  colnames(cor_x1) <- colnames(ratings_cleaned)
  
  set.seed(2024)
  
  # Run EGA and Bootstrapped EGA
  test_net <- EGAnet::EGA.fit(cor_x1, n = 30, model = "TMFG", algorithm = "walktrap", corr = "pearson")
  # test_net <- EGAnet::EGA(cor_x1, n = 30, model = "TMFG", algorithm = "walktrap", corr = "pearson")
  test_net <- EGAnet::EGA.fit(cor_x1, n = 30, model = "glasso", algorithm = "walktrap", corr = "pearson",
                              model.args = list(lambda.min.ratio = 0.1,
                                                nlambda = 100,
                                                gamma = 0.01
                              ))
  ega_res <- EGAnet::bootEGA(cor_x1,
                             iter = 4000, 
                             n = nrow(ratings_cleaned), 
                             model = "TMFG", 
                             algorithm = "walktrap",
                             type = "parametric",
                             ncores = 10, 
                             typicalStructure = TRUE)
  # model.args = list(lambda.min.ratio = 0.1,
  #                   nlambda = 300))
  
  # Cleaning and Saving Results
  # ega_res[["plot.typical.ega"]][["layers"]][[6]] <- NULL
  print(ega_res$plot.typical.ega)
  
  # Network Graph Construction
  A <- ega_res[["typicalGraph"]][["graph"]]
  dimattributes <- ega_res[["typicalGraph"]][["wc"]]
  g <- igraph::graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
  igraph::V(g)$snack_type <- dimattributes
  
  save(ega_res, g, file = here::here("data", "shenhav_rating_network_graph.RData"))
} else {
  load(here::here("data", "shenhav_rating_network_graph.RData"))
}


#load new data provided by Jason
shengav_rating <- read_csv("data/shengav_rating.csv")
shenhav_item_list <- read_csv("data/shenhav_item_list.csv")

ratings <- shengav_rating %>% 
  left_join(shenhav_item_list, by= "pic_path") %>% 
  group_by(participant, pic_name) %>%
  summarise(value = mean(value), .groups = 'drop') %>% #average the duplicates
  select(participant, pic_name, value) %>%  # Select only the necessary columns
  pivot_wider(
    names_from = pic_name,  # Create columns from pic_name
    values_from = value  # Fill the cells with the values from the value column
  ) %>% select(-participant)

#can you do a response time network to get categories
# ratings_rt <- shengav_rating %>% 
#   left_join(shenhav_item_list, by= "pic_path") %>% 
#   group_by(participant, pic_name) %>%
#   summarise(value = mean(rating.rt), .groups = 'drop') %>% #average the duplicates
#   select(participant, pic_name, value) %>%  # Select only the necessary columns
#   pivot_wider(
#     names_from = pic_name,  # Create columns from pic_name
#     values_from = value  # Fill the cells with the values from the value column
#   ) %>% select(-participant)

colnames(ratings) <- tolower(gsub(" ", "_", colnames(ratings)))
# colnames(ratings_rt) <- tolower(gsub(" ", "_", colnames(ratings_rt)))

# EGAnet::EGA.fit(ratings_rt, n = 312, model = "TMFG", algorithm = "walktrap", corr = "pearson")
# EGAnet::EGA.fit(ratings_rt, n = 312, model = "glasso", algorithm = "walktrap", corr = "pearson")

                            
ega_res <- EGAnet::bootEGA(ratings,
                           iter = 10000,
                           n = nrow(ratings),
                           model = "glasso",
                           algorithm = "walktrap",
                           type = "parametric",
                           ncores = 10,
                           typicalStructure = TRUE)

A <- ega_res[["typicalGraph"]][["graph"]]
dimattributes <- ega_res[["typicalGraph"]][["wc"]]
g <- igraph::graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
igraph::V(g)$snack_type <- dimattributes
save(ega_res, g, file = here::here("data", "shenhav_rating_network_graphFULL.RData"))
load(file = here::here("data", "shenhav_rating_network_graphFULL.RData"))

bapq.dimstab <- dimensionStability(ega_res)
bapq.dimstab$dimension.stability$structural.consistency
bapq.dimstab$dimension.stability$average.item.stability
bapq.dimstab$item.stability$plot +
  ggplot2::scale_color_brewer(palette = "Set3")

test <- bapq.dimstab$item.stability
stable_items <- names(test$item.stability$empirical.dimensions[test[["item.stability"]][["empirical.dimensions"]] > .50])
ega_simple_res <- EGAnet::EGA.fit(ratings[,stable_items], n = 312, model = "glasso", algorithm = "walktrap", corr = "pearson")
# EGAnet::EGA.fit(ratings_rt[,stable_items], n = 312, model = "glasso", algorithm = "walktrap", corr = "pearson")
# EGAnet::EGA.fit(ratings[,stable_items], n = 312, model = "TMFG", algorithm = "walktrap", corr = "pearson")
EGAnet::hierEGA(ratings[,stable_items], plot.type = "separate")
# EGAnet::hierEGA(ratings, plot.type = "separate")


ega_res2 <- EGAnet::bootEGA(ratings[,stable_items],
                           iter = 1000,
                           n = nrow(ratings),
                           model = "glasso",
                           algorithm = "walktrap",
                           type = "parametric",
                           ncores = 10,
                           typicalStructure = TRUE)

save(ega_res2, g, file = here::here("data", "shenhav_rating_network_graphV2.RData"))
load(file = here::here("data", "shenhav_rating_network_graphV2.RData"))

print(ega_res2$plot.typical.ega)

# get adjacency matrix
A <- ega_res2[["typicalGraph"]][["graph"]]


memres <- EGAnet::community.consensus(A, consensus.method = "iterative", 
                                      consensus.iter = 10000)
# get clusters
dimattributes <- ega_res2[["typicalGraph"]][["wc"]]
# create igraph object
g <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
# add decorate attributes
V(g)$product_type <- dimattributes
# V(g)$product_type <- memres #this would be the community consensus results

# brewer.pal(n = 10, name = 'Paired')

G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))

net_degree <- calculate_net_stats(g)

# l <- layout_nicely(G)
l <- layout_with_graphopt(g)
# l <- layout_with_gem(g)
# l <- layout.mds(g)

net_degree <- net_degree%>%
  mutate(colors =
           case_when(
             product_type == 1 ~ "#A6CEE3",
             product_type == 2 ~ "#1F78B4",
             product_type == 3 ~ "#B2DF8A",
             product_type == 4 ~ "#FF7F00", 
             product_type == 5 ~ "#6A3D9A",
             product_type == 6 ~ "#E31A1C",
             product_type == 7 ~ "#FDBF6F",
             product_type == 8 ~ "#33A02C",
             product_type == 9 ~ "#FB9A99",
             product_type == 10 ~ "#CAB2D6",
           )
  )

V(g)$color <- net_degree$colors

E(g)$color[E(g)$weight > 0] <- "forestgreen"
E(g)$color[E(g)$weight < 0] <- "red2"

plot(g,
     layout = l,
     margin = .0,
     # vertex.label = V(g)$name,
     vertex.label = NA,
     vertex.label.color = "black",
     label.font = 2,
     vertex.frame.color=adjustcolor(net_degree$colors, alpha.f = .1),
     # vertex.label.degree = 0,
     vertex.label.dist	= 1,
     vertex.label.cex = 1,
     vertex.size = 9,
     vertex.label.family = "Times",
     # edge.curved = .1,
     edge.width = E(g)$weight * 4.7
)
#NOTE you need to check labels are correct here
legend(x=1.3, 
       y=.6, 
       c("convenience","kitchen","household","alcoholic","recreational","personal","snack","COVID","baby","exercise"), 
       pch=21, 
       pt.bg=c("#A6CEE3", "#1F78B4", "#B2DF8A", "#FF7F00", "#6A3D9A", "#E31A1C", 
               "#FDBF6F","#33A02C","#FB9A99","#CAB2D6"),
       pt.cex=4, 
       cex=2, 
       bty="n", 
       ncol=1)

# plot(g,
#      layout = l,
#      vertex.shape="none", 
#      vertex.label.cex=.9,
#      vertex.label = V(g)$name,
#      vertex.label.font = 2,
#      vertex.label.color=net_degree$colors,
#      vertex.size = NULL,
#      vertex.label.family = "Times",
#      edge.width = E(g)$weight 
# )
# legend(x=1.3, 
#        y=.6, 
#        c("convenience","kitchen","household","alcoholic","recreational","personal","snack","COVID","baby","exercise"), 
#        pch=21, 
#        pt.bg=c("#A6CEE3", "#1F78B4", "#B2DF8A", "#FF7F00", "#6A3D9A", "#E31A1C", 
#                "#FDBF6F","#33A02C","#FB9A99","#CAB2D6"),
#        pt.cex=2, 
#        cex=.8, 
#        bty="n", 
#        ncol=1)
