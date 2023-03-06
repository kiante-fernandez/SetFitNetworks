

library(igraph) # Network Analysis and Visualization
library(qgraph) # Graph Plotting Methods, Psychometric Data Visualization and Graphical Model Estimation
library(EGAnet)
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(SemNeT)
# library(assortnet)
library(here)
# library(bootnet)
library(patchwork)


library(readxl)

sim_lsa <- as.matrix(readxl::read_excel(here::here("data", "Lee_Holyoak_LSA.xlsx"))[,-1])

sim_BERT <- as.matrix(readxl::read_excel(here::here("data", "Lee_Holyoak_BERT.xlsx"))[,-1])

sim_word2vec <- as.matrix(readxl::read_excel(here::here("data", "Lee_Holyoak_word2vec.xlsx"))[,-1])
# cor.snack_food <- SemNeT::similarity(sim_word2vec, method = "cor")

net_sim_lsa <- SemNeT::TMFG(sim_lsa)
net_sim_word2vec <- SemNeT::TMFG(sim_word2vec)
net_sim_bert <- SemNeT::TMFG(sim_BERT)

par(mfrow = c(1, 3)) # set the plotting area into a 1*3 array

AJ_graph <- function(matrix){
  # visualize the matrix as a heatmap.
  m <- as.matrix(matrix)
  mDim <- length(m[1, ]) # determine size of one dim of the matrix, which we assume is identical to the other dim.
  
  heatmap(m[mDim:1, ],
          Rowv = NA, Colv = NA, scale = "none",
          margins = c(10, 10),
          col = hcl.colors(300)
  )
  # legend(x = "bottom", legend = c("min", "med", "max", "higher"), fill = hcl.colors(4))
  
}

AJ_graph(net_sim_lsa)
AJ_graph(net_sim_word2vec)
AJ_graph(net_sim_bert)


plot_network <- function(x, name){
  graph <- graph_from_adjacency_matrix(x,
                                           "undirected",
                                           weighted = TRUE,
                                           diag = F
  )
  clp <- cluster_walktrap(graph)
  V(graph)$community <- clp$membership
  plot(graph,
       layout = layout_nicely(graph),
       margin = .0,
       vertex.label = V(graph)$name,
       vertex.label.color = "black",
       vertex.label.cex = 1,
       vertex.label.dist = .5,
       vertex.size = 6,
       vertex.color = V(graph)$community,
       vertex.label.family = "Times",
       main = name,
       edge.width = abs(E(graph)$weight) * 3)
}

plot_network(net_sim_lsa, "lsa")
plot_network(net_sim_word2vec, "word2vec")
plot_network(net_sim_bert, "BERT")


