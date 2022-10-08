library(igraph) # Network Analysis and Visualization
library(qgraph) # Graph Plotting Methods, Psychometric Data Visualization and Graphical Model Estimation
library(EGAnet)
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(SemNeT)
library(assortnet)
library(here)
library(jpeg)
library(bootnet)
library(patchwork)
# load task data


lee_2021_rating1 <- read_csv(here("data", "lee_2021_rating1.csv"), col_names = FALSE)
lee_2021_nutrition1 <- read_csv(here("data", "lee_2021_nutrition1.csv"), col_names = FALSE)
lee_2021_pleasure1 <- read_csv(here("data", "lee_2021_pleasure1.csv"), col_names = FALSE)

#reponse times network idea
# lee_2021_rating1 <- readr::read_csv(here("data", "lee_2021_rating1_RT.csv"), col_names = FALSE, show_col_types = F)
# lee_2021_rating1 <- lee_2021_rating1/1000 
# removes <- vector(mode= "numeric", length = 267)
# for (foo in 1:267){
#   if (any(lee_2021_rating1[foo,] > 8)){
#     removes[[foo]] <- foo
#   }
#   if (any(lee_2021_rating1[foo,] < 1)){
#     removes[[foo]] <- foo
#   }
# 
# }
# unique(removes[removes != 0])
# lee_2021_rating1 <- lee_2021_rating1[!(1:267 %in% unique(removes[removes != 0])),]

# load the names of the foods (see nick file)
FoodNames <- readxl::read_excel(here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))

# create cleaned names
names(lee_2021_rating1) <- FoodNames$Name
names(lee_2021_nutrition1) <- FoodNames$Name
names(lee_2021_pleasure1) <- FoodNames$Name

########################
# Network estimation
# compute measure of similarity
# cosine.snack_food <- SemNeT::similarity(lee_2021_rating1, method = "cosine")
# angular.snack_food <- SemNeT::similarity(lee_2021_rating1, method = "angular")
# jaccard.snack_food <- SemNeT::similarity(lee_2021_rating1, method = "jaccard")
# euclid.snack_food <- SemNeT::similarity(lee_2021_rating1, method = "euclid")
cor.snack_food <- SemNeT::similarity(lee_2021_rating1, method = "cor")
cor.snack_food_nut <- SemNeT::similarity(lee_2021_nutrition1, method = "cor")
cor.snack_food_ple <- SemNeT::similarity(lee_2021_pleasure1, method = "cor")

Pcor.snack_food <- ppcor::pcor(lee_2021_rating1)$estimate
Pcor.snack_food_nut <- ppcor::pcor(lee_2021_nutrition1)$estimate
Pcor.snack_food_ple <- ppcor::pcor(lee_2021_pleasure1)$estimate

# parse the network
net.snack_food <- SemNeT::TMFG(Pcor.snack_food)
net.snack_food_nut <- SemNeT::TMFG(Pcor.snack_food_nut)
net.snack_food_ple <- SemNeT::TMFG(Pcor.snack_food_ple)

par(mfrow = c(1, 3)) # set the plotting area into a 1*3 array
L <- averageLayout(net.snack_food, net.snack_food_nut, net.snack_food_ple)
qgraph(net.snack_food, theme = "colorblind", layout = L, cut = 0)
title("Value", line = 2.5)
qgraph(net.snack_food_nut, theme = "colorblind", layout = L, cut = 0)
title("Nutrition", line = 2.5)
qgraph(net.snack_food_ple, theme = "colorblind", layout = L, cut = 0)
title("Pleasure", line = 2.5)


# note you have code here that should NOT work
m <- as.matrix(cor.snack_food)

mDim <- length(m[1, ]) # determine size of one dim of the matrix, which we assume is identical to the other dim.
# calulate the correlations
ggcorrplot::ggcorrplot(m[mDim:1, ],
  colors = c("red", "white", "green"),
  ggtheme = ggplot2::theme_classic,
  outline.color = "white",
  show.diag = T,
  hc.order = F
) + ggplot2::theme(
  axis.text.x = element_text(size = 4),
  axis.text.y = element_text(size = 4),
  axis.ticks = element_blank()
) + theme(legend.position = "left")


# visualize the matrix as a heatmap.
m <- as.matrix(net.snack_food)
# all of the diag(m) are equal to one, not sure why the graphic does not show this
mDim <- length(m[1, ]) # determine size of one dim of the matrix, which we assume is identical to the other dim.

heatmap(m[mDim:1, ],
  Rowv = NA, Colv = NA, scale = "none",
  margins = c(10, 10),
  col = hcl.colors(300)
)
legend(x = "bottom", legend = c("min", "med", "max", "higher"), fill = hcl.colors(4))


# get graph and remove self-connections
graph_ratings <- graph_from_adjacency_matrix(net.snack_food,
  "undirected",
  weighted = TRUE,
  add.colnames = T,
  diag = F,
)
l <- layout_nicely(graph_ratings)

V(graph_ratings)$name <- names(lee_2021_rating1)

plot(graph_ratings,
  layout = l,
  margin = .0,
  vertex.label = V(graph_ratings)$name,
  vertex.label.color = "black",
  vertex.label.cex = .6,
  vertex.size = 2,
  vertex.label.family = "Times",
  edge.curved = .15,
  edge.width = E(graph_ratings)$weight * 10,
  # mark.groups =
)

##########
# estimate network

# Gaussian graphical model
# based on my reading this is the best way to estimate and select the network
# this is based on the sample size we have. Not we have many more false positive edges here,
# so, doing edge wise analysis must be interpreted with caution in this case

# Estimate a network structure, with parameters refitted without LASSO regularization:
net <- estimateNetwork(lee_2021_rating1,
  default = "EBICglasso",
  tuning = 0.5,
  verbose = F
)
# refit = TRUE) #note refit is for simulations for power

plot(net, layout = "spring", cut = 0)
title("Snack Associations", line = 2.5)

# power simulations:
# Simulate 100 repititions in 8 cores under different sampling levels:
Sim1 <- netSimulator(net,
  default = c("EBICglasso", "pcor"),
  nCases = c(250, 500, 750, 1000, 1500),
  nReps = 100,
  nCores = 8
)
# Table of results:
Sim1
# Plot results:
plot(Sim1)

b1 <- bootnet(net, nBoots = 10000, nCores = 8)
b2 <- bootnet(net, nBoots = 10000, nCores = 8, type = "case")
boot_test <- EGAnet::bootEGA(lee_2021_rating1,
  iter = 10000,
  n = 267,
  model = "glasso",
  algorithm = "walktrap",
  ncores = 8
)

plot(b1, plot = "interval", order = "sample", split0 = F, labels = F)
plot(b1, "edge", plot = "difference", order = "sample")
plot(b1, "strength", order = "sample")


plot(b2)
plot(b2, perNode = T, "strength")
corStability(b2)

sega_liking <- EGA(lee_2021_rating1,
  n = 267, plot.EGA = TRUE, model = "glasso", algorithm = "walktrap",
  plot.type = "qgraph",
  plot.args = list(vsize = 4, edge.alpha = 1)
)
boot_test <- EGAnet::bootEGA(lee_2021_rating1,
  plot.type = "qgraph",
  iter = 1000,
  type = "resampling",
  corr = "spearman",
  n = 130,
  model = "glasso",
  algorithm = "walktrap",
  ncores = 8, typicalStructure = T
)

boot_nutrition <- EGAnet::bootEGA(lee_2021_nutrition1,
  plot.type = "qgraph",
  iter = 1000,
  n = 267,
  model = "glasso",
  algorithm = "walktrap",
  ncores = 8, typicalStructure = T
)

boot_pleasure <- EGAnet::bootEGA(lee_2021_pleasure1,
  plot.type = "qgraph",
  iter = 1000,
  n = 267,
  model = "glasso",
  algorithm = "walktrap",
  ncores = 8, typicalStructure = T
)

boot_test$plot.typical.ega
boot_test$summary.table
boot_test$frequency
# you are having an issue with the labeling doubling. Here is a crude way to
# fix that issue for now. Here we are just setting the layer in the plot that is
# related to the issue to null. I am sure it is not everything in the layer.
# But it works.

boot_test[["plot.typical.ega"]][["layers"]][[6]] <- NULL
boot_nutrition[["plot.typical.ega"]][["layers"]][[6]] <- NULL
boot_pleasure[["plot.typical.ega"]][["layers"]][[6]] <- NULL
# plot all the med structures together
boot_test$plot.typical.ega +
  boot_nutrition$plot.typical.ega +
  boot_pleasure$plot.typical.ega

boot_test$typicalGraph$typical.dim.variables$dimension

save(net, b1, b2, boot_test, file = here("data", "lee_2021_network.RData"))

stab <- EGAnet::dimensionStability(boot_test)

compare_plots <- compare.EGA.plots(boot_test$EGA, boot_nutrition$EGA, boot_pleasure$EGA)

# CREATE EXAMPLE NETWORKS FOR THE PRESENTATION
graph_ratings <- graph_from_adjacency_matrix(boot_test$typicalGraph$graph,
  "undirected",
  weighted = TRUE
)
graph_nutrition <- graph_from_adjacency_matrix(boot_nutrition$typicalGraph$graph,
  "undirected",
  weighted = TRUE
)
graph_pleasure <- graph_from_adjacency_matrix(boot_pleasure$typicalGraph$graph,
  "undirected",
  weighted = TRUE
)
# V(graph_ratings)$name <- names(lee_2021_rating1)
# V(graph_nutrition)$name <- names(lee_2021_nutrition1)
# V(graph_pleasure)$name <- names(lee_2021_pleasure1)

value_name_data <- tibble(items = V(graph_ratings)$name) %>%
  left_join(boot_test$typicalGraph$typical.dim.variables,
    by = "items"
  )
nutrition_name_data <- tibble(items = V(graph_nutrition)$name) %>%
  left_join(boot_nutrition$typicalGraph$typical.dim.variables,
    by = "items"
  )
pleasure_name_data <- tibble(items = V(graph_pleasure)$name) %>%
  left_join(boot_pleasure$typicalGraph$typical.dim.variables,
    by = "items"
  )

L <- averageLayout(graph_ratings, graph_nutrition, graph_pleasure)


V(graph_ratings)$color <- value_name_data$dimension
V(graph_nutrition)$color <- nutrition_name_data$dimension
V(graph_pleasure)$color <- pleasure_name_data$dimension

E(graph_ratings)$color[E(graph_ratings)$weight > 0] <- "forestgreen"
E(graph_ratings)$color[E(graph_ratings)$weight < 0] <- "red2"

# E(graph_ratings)[E(graph_ratings)$weight < 0]

E(graph_nutrition)$color[E(graph_nutrition)$weight > 0] <- "forestgreen"
E(graph_nutrition)$color[E(graph_nutrition)$weight < 0] <- "red2"

E(graph_pleasure)$color[E(graph_pleasure)$weight > 0] <- "forestgreen"
E(graph_pleasure)$color[E(graph_pleasure)$weight < 0] <- "red2"

par(mfrow = c(1, 3)) # set the plotting area into a 1*3 array
plot(graph_ratings,
  # layout = L,
  margin = .0,
  vertex.label = V(graph_ratings)$name,
  vertex.label.color = "black",
  vertex.label.cex = 1,
  vertex.label.dist = .5,
  vertex.size = 6,
  # vertex.shape="none",
  # vertex.color = adjustcolor(alpha.f = .5),
  vertex.label.family = "Times",
  main = "How much would you like this as a daily snack?",
  # edge.curved = .15,
  edge.width = abs(E(graph_ratings)$weight) * 5,
  # mark.groups = value_name_data$dimension
)

plot(graph_nutrition,
  layout = L,
  margin = .0,
  vertex.label = V(graph_nutrition)$name,
  vertex.label.color = "black",
  vertex.label.cex = 1,
  vertex.label.dist = .5,
  vertex.size = 6,
  vertex.label.family = "Times",
  main = "How nutritious do you consider this item to be?",
  # edge.curved = .15,
  edge.width = abs(E(graph_ratings)$weight) * 5,
  # mark.groups =
)
plot(graph_pleasure,
  layout = L,
  margin = .0,
  vertex.label = V(graph_pleasure)$name,
  vertex.label.color = "black",
  vertex.label.cex = 1,
  vertex.label.dist = .5,
  vertex.size = 6,
  vertex.label.family = "Times",
  main = "How pleasurable do you consider this item to be?",
  # edge.curved = .15,
  edge.width = abs(E(graph_ratings)$weight) * 5,
  # mark.groups =
)

# testing differences between networks
library(NetworkComparisonTest)
netvalue <- estimateNetwork(lee_2021_rating1,
  default = "EBICglasso",
  tuning = 0.5,
  verbose = F
)
netpleasure <- estimateNetwork(lee_2021_pleasure1,
  default = "EBICglasso",
  tuning = 0.5,
  verbose = F
)

NCT_value_pleasure <- NCT(netvalue, netpleasure,
  it = 5000,
  binary.data = FALSE,
  weighted = TRUE,
  paired = TRUE,
  test.edges = TRUE,
  edges = "all",
  test.centrality = TRUE,
  centrality = "strength",
  verbose = F
)

save(NCT_value_pleasure, file = here("data", "NetworkComparison.RData"))

plot(NCT_value_pleasure, what = "network")
plot(NCT_value_pleasure, what = "strength")
# plot(NCT_value_pleasure, what = "edge")

qplot(NCT_value_pleasure[["nwinv.perm"]], bins = 50) + geom_vline(xintercept = NCT_value_pleasure[["nwinv.real"]], color = "red") +
  labs(title = "Difference in edge weights") + theme_classic() + qplot(NCT_value_pleasure[["glstrinv.perm"]], bins = 50) + geom_vline(xintercept = NCT_value_pleasure[["glstrinv.real"]], color = "red") +
  labs(title = "Difference in Global Strength") + theme_classic()

# calculating
modularity(graph_ratings, boot_test$typicalGraph$typical.dim.variables$dimension)

clustAnalytics::internal_density(graph_ratings, boot_test$typicalGraph$typical.dim.variables$dimension)
clustAnalytics::average_degree(graph_ratings, boot_test$typicalGraph$typical.dim.variables$dimension)

# upper bound of 1 given that they are correlations
clustAnalytics::weighted_clustering_coefficient(graph_ratings, upper_bound = 1)
igraph::edge_density(graph_ratings)

NetworkToolbox::transitivity(graph_ratings, weighted = T) # hihger averager local clustering
# average weighted transitivity?
mean(igraph::transitivity(graph_ratings, type = "weighted")) # higher
igraph::transitivity(graph_ratings, type = "global") # higher

Cluster <- clustcoef_auto(boot_test$typicalGraph$graph)
NetworkToolbox::diversity(boot_test$typicalGraph$graph, comm = boot_test$typicalGraph$typical.dim.variables$dimension)
# Values closer to 1 suggest greater between-community connectivity and
# values closer to 0 suggest greater within-community connectivity

snack_names <- tibble(names = V(graph_ratings)$name)

remove_snacks <- snack_names$names[-c(1:9)]
unlist(remove_snacks)
g2 <- delete_vertices(graph_ratings, unlist(remove_snacks))
l4 <- layout_in_circle(g2)
V(g2)$color <- "deepskyblue1"
plot(g2,
  layout = l4,
  vertex.label.color = "black",
  vertex.label.cex = 1.3,
  vertex.size = 10,
  vertex.label.family = "Times",
  edge.curved = .15,
  edge.width = E(graph_ratings)$weight * 8
)
g2 <- g2 %>%
  set_vertex_attr("snack_type", V(g2), 1)

modularity(g2, membership = c(1, 1, 1, 1, 1, 1, 1, 1, 1))

remove_snacks <- snack_names$names[-c(25, 32, 11, 27, 40, 55, 13, 21, 31)]
unlist(remove_snacks)
g3 <- delete_vertices(graph_ratings, unlist(remove_snacks))
l4 <- layout_in_circle(g3)
V(g3)[c(1, 2)]$color <- "deepskyblue1"
V(g3)[c(3, 6)]$color <- "firebrick1"
V(g3)[c(4, 5, 7)]$color <- "gold"
V(g3)[c(8)]$color <- "darkolivegreen2"
V(g3)[c(9)]$color <- "darkorange"

plot(g3,
  layout = l4,
  vertex.label.color = "black",
  vertex.label.cex = 1.3,
  vertex.size = 10,
  vertex.label.family = "Times",
  edge.curved = .15,
  edge.width = E(graph_ratings)$weight * 8
)
g3 <- g3 %>%
  set_vertex_attr("snack_type", V(g3)[c(1, 2)], 1) %>%
  set_vertex_attr("snack_type", V(g3)[c(3, 6)], 2) %>%
  set_vertex_attr("snack_type", V(g3)[c(4, 5, 7)], 3) %>%
  set_vertex_attr("snack_type", V(g3)[c(8)], 4) %>%
  set_vertex_attr("snack_type", V(g3)[c(9)], 5)

modularity(g3, membership = c(1, 1, 2, 3, 3, 2, 3, 4, 5))


# check assorativity FOR EXAMPLE NETWORKS
adj <- as_adjacency_matrix(g2, sparse = F, attr = "weight")
assortment.discrete(adj, V(g2)$snack_type, weighted = TRUE, SE = F, M = 1)

adj <- as_adjacency_matrix(g3, sparse = F, attr = "weight")
assortment.discrete(adj, V(g3)$snack_type, weighted = TRUE, SE = F, M = 1)



mean(Cluster[c(1:9), "signed_clustWS"])
sum((Cluster[c(1:9), "signed_clustWS"] - mean(Cluster[, "signed_clustWS"]))^2)

mean(Cluster[c(25, 32, 11, 27, 40, 55, 13, 21, 31), "signed_clustWS"])
sum((Cluster[c(25, 32, 11, 27, 40, 55, 13, 21, 31), "signed_clustWS"] - mean(Cluster[, "signed_clustWS"]))^2)
