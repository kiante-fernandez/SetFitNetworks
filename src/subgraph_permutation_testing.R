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
# 2023/02/01      Kianté  Fernandez                   coded up version one

# create simulation function

simulate_network <- function(g) {
  # permute graph
   g1 <- rewire(g, with = keeping_degseq(niter = vcount(g) * 100))
   # check weighted add weights
   if (!is.null(E(g1)$weight)) {
     E(g1)$weight <- sample(E(g)$weight)
   }
   adj_temp <- as_adjacency_matrix(g1, sparse = F, attr = "weight")
   assortment.discrete(adj_temp, V(g1)$snack_type, weighted = TRUE, SE = F)$r
 }
 # set number of permutations
 nPerm <- 500
 p <- vector(mode = "list", length = 50)
 Switch <- F

 for (graph_idk in 1:50) {
   # run simulations
   r0 <- replicate(nPerm, simulate_network(subgraphs[[graph_idk]]))

   if (1 - mean(r0 < rS[[graph_idk]]) < 0.05) {
     Switch <- F
   } else {
     Switch <- T
   }

   # plot it
   p[[graph_idk]] <- qplot(r0, bins = 100) +
     geom_vline(xintercept = rS[[graph_idk]], color = "red") +
     geom_vline(xintercept = rT[[graph_idk]], color = "blue") +
     labs(title = paste0(round(rS[[graph_idk]], 3))) + theme_classic() + {
       if (Switch) theme(panel.background = element_rect(fill = "red"))
     }
 }
 wrap_plots(p) #plots all the plots at once
 
 