# networkStat.R - calculates the network statistics.of interest from a suite

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
# 10/15/22      Kianté  Fernandez                       coded up 

#### subgraph algorithm
networkStat <- function(gt, n_statistic = "assortment") {
  
  #TODO this should be vectorized to just generate n subgrahs and calculate the stat nsubgraphs = 100
  #list of network statistics build into function or to be built 
  
  #assortment.discrete:Assortment on discrete vertex values
  #Calculates the assortativity coefficient for weighted graph with categorical vertex values
  
  #weighted_clustering_coefficient: Weighted clustering coefficient of a weighted graph.
  #Weighted clustering Computed using the definition given by McAssey, M. P. and Bijma, F. in "A clustering coefficient for complete weighted networks" (2015).
  
  #edge_density: Graph density
  #The density of a graph is the ratio of the number of edges and the number of possible edges.
  
  #diversity: Diversity Coefficient
  #Values closer to 1 suggest greater between-community connectivity and values closer to 0 suggest greater within-community connectivity
  
  # transitivity (in some cases this will be similar to the clustering_coefficient)
  #Computes transitivity of a network
  
  #smallworldness Small-worldness Measure
  #Computes the small-worldness measure of a network
  #smallworldness(adj_temp,iter = 100)
  
  #internal_density: Internal Density
  #Internal density of a graph's communities. 
  #That is, the sum of weights of their edges divided by the number of unordered pairs of vertices (which is the number of potential edges).
  #clustAnalytics::internal_density(gt,V(gt)$snack_type)
  
  #average_degree: Average Degree
  #Average degree (weighted degree, if the graph is weighted) of a graph's communities.
  ##clustAnalytics::average_degree(gt,V(gt)$snack_type)

  # nnodes <- length(V(g)) # number of nodes
  # #NOTE THE LENGTH ONE THING IS JUST UNTIL WE DO THE VECTOR SOLUTION
  # 
  # rS <- vector(mode = "numeric", length = 1) # estimated value for statistic
  # 
  # # get subgraph
  # size <- sample(seq_len(nnodes), C, replace = F)
  # gt <- igraph::induced_subgraph(g, size)
  
  adj_temp <- igraph::as_adjacency_matrix(gt, sparse = F, attr = "weight")
      # save value of r
      if (n_statistic == "assortment") {
        rS[[1]] <- assortnet::assortment.discrete(adj_temp, V(gt)$snack_type, weighted = TRUE, SE = F)$r
      } else if (n_statistic == "edge_density") {
        rS[[1]] <- igraph::edge_density(gt)
      } else if (n_statistic == "weighted_clustering_coefficient") {
        rS[[1]] <- clustAnalytics::weighted_clustering_coefficient(gt, upper_bound = 1)
      } else if (n_statistic == "diversity") {
        temp_stat <- NetworkToolbox::diversity(adj_temp, V(gt)$snack_type)$overall
        temp_stat[!is.finite(temp_stat)] <- NA
        rS[[1]] <- mean(temp_stat, na.rm = T)
      } else if (n_statistic == "internal_density") {
        temp_stat <- clustAnalytics::internal_density(gt,V(gt)$snack_type)
        temp_stat[is.nan(temp_stat)] <- NA
        rS[[1]] <-  mean(temp_stat, na.rm = T)
      } else if (n_statistic == "average_degree") {
        temp_stat <- clustAnalytics::average_degree(gt,V(gt)$snack_type)
        temp_stat[is.nan(temp_stat)] <- NA
        rS[[1]] <- mean(temp_stat, na.rm = T)
      } else if (n_statistic == "smallworldness") {
        rS[[1]] <- NetworkToolbox::smallworldness(adj_temp,iter = 100, method = "TJHBL")$swm
      }
  
  return(rS)
}

get_subgraphs <- function(n, g, C){
  nnodes <- length(V(g)) # number of nodes
  gs <-  vector(mode = "list", length = n)
  for (i in 1L:n){
    size <- sample(seq_len(nnodes), C, replace = F)
    gs[[i]] <- igraph::induced_subgraph(g, size) 
    if(ecount(gs[[i]]) == 0){
      gs[[i]] <- igraph::induced_subgraph(g, size) 
    }
  }
  return(gs)
}

# get subgraph
nsubgraphs <- 100
nstats <- 3
C <- 6
network_stats <- c("assortment","edge_density","weighted_clustering_coefficient")

subgraphs <- get_subgraphs(nsubgraphs, g, C) #using function from above

res_nets <- matrix(, nrow = nsubgraphs, ncol = nstats + 1)
colnames(res_nets) <- c("subgraph",paste0(network_stats))

for (subgraph_idx in seq_len(nsubgraphs)){
  res_nets[subgraph_idx, 1] = subgraph_idx
  for(net_stat_idx in seq_len(nstats)){
    res_nets[subgraph_idx, 1 + net_stat_idx] = networkStat(subgraphs[[subgraph_idx]],network_stats[[net_stat_idx]])
  }
}
#correlation::correlation(data.frame(res_nets[,-1]))
TEST <- data.frame(res_nets)
TEST$graphs <- subgraphs #you can add the graph objects to the data frame! nice. 
TEST

library(ggplot2)
ggplot(data.frame(TEST), aes(x = weighted_clustering_coefficient)) +
  geom_histogram(colour = 1, fill = "white") +
  theme_classic() +
  labs(x = "", title = "")

# ggplot(data.frame(TEST), aes(x = assortment)) +
#   geom_histogram(aes(y = ..density..),
#                  colour = 1, fill = "white"
#   ) +
#   geom_density(
#     lwd = 1, colour = 4,
#     fill = 4, alpha = 0.25
#   ) +
#   theme_classic() +
#   labs(x = "", title = "assortment")



