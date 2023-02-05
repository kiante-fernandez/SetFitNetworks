# subgraph_permutation_testing.R - # permutation testing to test if subgraph is significantly different from zero
#
# Copyright (C) 2023 Kianté Fernandez, <kiantefernan@gmail.com>
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
# 2023/02/03      Kianté  Fernandez                   coded up version one
# 2023/02/04      Kianté  Fernandez                   finished modularity test

library(igraph)
library(ggplot2)
library(patchwork)

# create simulation function

simulate_network <- function(g) {
  # permute graph
  # g1 <-igraph::rewire(g, with = igraph::keeping_degseq(loops = FALSE, niter = vcount(g) * 100))
  g1 <- rewire(g, each_edge(p = .5, loops = FALSE))

  # check weighted add weights
  if (!is.null(E(g1)$weight)) {
    E(g1)$weight <- sample(E(g)$weight)
  }
  adj_temp <- igraph::as_adjacency_matrix(g1, sparse = F, attr = "weight")
  # calculate stat
  # assortment.discrete(adj_temp, V(g1)$snack_type, weighted = TRUE, SE = F)$r
  as.numeric(modularity(g1, V(g1)$snack_type))
  # detach(igraph)
}

# g <- subgraphs[[2]]
# sample_degseq
# rewire(g, keeping_degseq(loops = FALSE, niter = vcount(g) * 100))
#
# rewire(g, each_edge(p = .1, loops = FALSE))

# load the subgraphs
# load(file = here::here("data", "LowHighWithinBetween.RData"))

load(file = here::here("data", "modularity_100_6.RData"))

# set number of permutations
nPerm <- 2500
p <- vector(mode = "list", length = 100)
res_sig <- rep(NA, 100)
Switch <- F
# graph_idk <- 1
for (graph_idk in 1:100) {
  # run simulations
  r0 <- replicate(nPerm, simulate_network(subgraphs[[graph_idk]]))
  rS <- modularity(subgraphs[[graph_idk]], V(subgraphs[[graph_idk]])$snack_type)

  if (sign(rS) == 1) {
    if (mean(r0 > rS) < 0.05) {
      Switch <- F
      res_sig[[graph_idk]] <- 1
    } else {
      Switch <- T
      res_sig[[graph_idk]] <- 0
    }
  } else if ((sign(rS) == -1)){
    if (mean(r0 < rS) < 0.05) {
      Switch <- F
      res_sig[[graph_idk]] <- 1
    } else {
      Switch <- T
      res_sig[[graph_idk]] <- 0
    }
  }
  if (rS == 0){
    Switch <- T
    res_sig[[graph_idk]] <- 0
  }

  # plot it
  p[[graph_idk]] <- qplot(r0, bins = 10) +
    geom_vline(xintercept = rS, color = "blue", size = 2) +
    labs(title = paste0(round(rS, 3))) + theme_classic() + {
      if (Switch) theme(panel.background = element_rect(fill = "red"))
    }
  print(p[[graph_idk]])
}

wrap_plots(p) # plots all the plots at once
dput(res_sig)

# res for exp 1 for modularity (NA's were zeros too )

# res_sig <- c(1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1,
#   0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0,
#   0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1,
#   1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1,
#   0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1,
#   0, 1)

# res for exp 2 for modularity
# c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 
#   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#   0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 
#   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
#   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
