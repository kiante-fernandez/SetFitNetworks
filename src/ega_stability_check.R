suppressMessages(library(EGAnet)) # Exploratory Graph Analysis – a Framework for Estimating the Number of Dimensions in Multivariate Data using Network Psychometrics
library(igraph)
ega_res$plot.typical.ega
knitr::kable(ega_res$summary.table, digits = 3)
knitr::kable(ega_res$frequency)

# Structural consistency
bapq.dimstab <- dimensionStability(ega_res)

bapq.dimstab$dimension.stability$structural.consistency
bapq.dimstab$dimension.stability$average.item.stability
bapq.dimstab$item.stability$plot +
  ggplot2::scale_color_brewer(palette = "Set1")

# View(bapq.dimstab$item.stability$item.stability$all.dimensions)

gsub(0, " ", knitr::kable(bapq.dimstab$item.stability$item.stability$all.dimensions,digits = 3))
knitr::kable(bapq.dimstab$item.stability$item.stability$all.dimensions,digits = 3)

memres <- EGAnet::community.consensus(ega_res[["typicalGraph"]][["graph"]], consensus.method = "iterative", 
                    consensus.iter = 10000)

B <- ega_res[["typicalGraph"]][["graph"]]
# get clusters
dimattributes <- ega_res[["typicalGraph"]][["wc"]]
# create igraph object
g <- igraph::graph_from_adjacency_matrix(B, "undirected", weighted = TRUE)
# add decorate attributes
V(g)$snack_type <- memres
data.frame(names(memres), memres)[data.frame(names(memres), memres)$memres ==1
,]
# 
# community.homogenize(dimattributes,memres)
# community.homogenize(memres,dimattributes)

