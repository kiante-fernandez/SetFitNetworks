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

# Function Definitions
# Function to clean column names by removing letters at the front, numbers, and file extensions
clean_col_names <- function(names) {
  names <- gsub("^[A-Za-z]+_?[0-9]*_", "", names) # Remove letters at the front and numbers
  names <- gsub("\\.jpeg$|\\.jpg$", "", names)    # Remove the file extension
  return(names)
}

# Data Preparation
allSubBidData <- read_csv("~/Downloads/allSubBidData.csv")
ratings <- allSubBidData %>% 
  select(subject_ID, item_response, item_name) %>% 
  pivot_wider(names_from = item_name, values_from = item_response) %>% 
  ungroup() %>% select(-subject_ID)

# Data Cleaning and Imputation
ratings_cleaned <- ratings %>% select(where(~ !any(is.na(.))))
ratings_imputed <- ratings %>% mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

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
ggcorrplot::ggcorrplot(m[mDim:1, ], colors = c("red", "white", "green"), ggtheme = ggplot2::theme_classic, outline.color = "white", show.diag = TRUE, hc.order = TRUE) +
  ggplot2::theme(axis.text.x = element_text(size = 4), axis.text.y = element_text(size = 4), axis.ticks = element_blank(), legend.position = "left")

# Exploratory Graph Analysis (EGA)
# Check if the file exists
if (!file.exists(here::here("data", "shenhav_rating_network_graph.RData"))) {
  # Preparing Data for EGA
  shenhav_2019_rating <- ratings_cleaned
  
  # Correlation Matrix Computation and Adjustment
  cor_x1 <- cor(na.omit(ratings_cleaned))
  cor_x1 <- matrix(Matrix::nearPD(cor_x1, corr = TRUE, maxit = 500)$mat,   ncol(ratings_cleaned))
  cor_x1 <- (cor_x1 + t(cor_x1)) / 2 # Make symmetric
  colnames(cor_x1) <- colnames(ratings_cleaned)
  set.seed(2024)
  
  # Run EGA and Bootstrapped EGA
  test_net <- EGAnet::EGA.fit(cor_x1, n = 30, model = "TMFG", algorithm = "walktrap", corr = "pearson")
  
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
  ega_res[["plot.typical.ega"]][["layers"]][[6]] <- NULL
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
