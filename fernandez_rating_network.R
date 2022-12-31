# Fernandez_rating_network - conducts a bootstrap Exploratory Graph Analysis function for 
# the study one, study two, and data combined.
#
# Copyright (C) 2022 Kianté Fernandez, <kiantefernan@gmail.com>
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
# 2022/12/27      Kianté  Fernandez                       wrote code

# Libraries
library(here)
suppressMessages(library(EGAnet)) # Exploratory Graph Analysis – a Framework for Estimating the Number of Dimensions in Multivariate Data using Network Psychometrics
library(readxl)
library(igraph)

if (!file.exists(here("data", "fernandez_rating_network_graph.RData"))) {
  
  # load the names of the foods
  FoodNames <- readxl::read_excel(here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))
  
  # load the data
  fernandez_2022_rating1 <- readr::read_csv(here("data","fernandez_2022_rating_exp1.csv"), col_names = T, show_col_types = F)
  fernandez_2022_rating2 <- readr::read_csv(here("data","fernandez_2022_rating_exp2.csv"), col_names = T, show_col_types = F)
  
  #take both sets of rating data from study one and two and combine them
  fernandez_2022_rating_combineded <- rbind(fernandez_2022_rating1,fernandez_2022_rating2)
  
  # number of observations
  n <- nrow(fernandez_2022_rating_combineded)
  
  # Set random seed
  set.seed(2022)
  
  # run community detection procedure
  ega_res <- EGAnet::bootEGA(fernandez_2022_rating_combineded,
                             iter = 1000,
                             n = n,
                             model = "glasso",
                             algorithm = "walktrap",
                             ncores = 10, typicalStructure = T
  )
  
  ega_res[["plot.typical.ega"]][["layers"]][[6]] <- NULL
  
  print(ega_res$plot.typical.ega)
  # get adjacency matrix
  A <- ega_res[["typicalGraph"]][["graph"]]
  # get clusters
  dimattributes <- ega_res[["typicalGraph"]][["wc"]]
  # create igraph object
  g <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
  # add decorate attributes
  V(g)$snack_type <- dimattributes
  
  save(ega_res, g, file = here("data", "fernandez_rating_network_graph.RData"))
  
}else {
  load(here::here("data", "fernandez_rating_network_graph.RData"))
}

#####stability analysis 
#computes the stability of dimensions
#he proportion of times the original dimension is exactly replicated in across bootstrap samples
# EGAnet::dimensionStability(ega_res)
# 
# methods.section(
#   ega_res,
#   stats = "dimensionStability"
# )
