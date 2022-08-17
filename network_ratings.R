library(corrplot) # Visualization of a Correlation Matrix
library(ggcorrplot) # Visualization of a Correlation Matrix using 'ggplot2'
library(igraph) # Network Analysis and Visualization
library(qgraph) # Graph Plotting Methods, Psychometric Data Visualization and Graphical Model Estimation
library(EGAnet)
library(tidyverse) # Easily Install and Load the 'Tidyverse'
## SemNetCleaner automatically loads SemNetDictionaries
library(SemNetCleaner)
library(SemNeT)
library(SemNetDictionaries)
library(assortnet)
library(here)
# library(reticulate)
# library(leiden)
# TEST <- arules::apriori(rating_network)
# load task data

load("/Users/kiantefernandez/Documents/OSU/Fernandez_2021_EEG_2x2_Choice/data/smith_2020_paper_data/TaskTrial.RData")
load("/Users/kiantefernandez/Documents/OSU/Fernandez_2021_EEG_2x2_Choice/data/smith_2020_paper_data/TaskRatings.RData")

# load the names of the foods
FoodNamesTop100 <- read_csv("~/Documents/OSU/Fernandez_2021_EEG_2x2_Choice/EEG_2x2_Choice/FoodNamesTop100.csv")

# load created dictionary
# snack_foods.dictionary <- readRDS("~/Documents/OSU/Karmarkar_2021_subset_choice/multi_select/markdown/perference_networks/snack_foods.dictionary.rds")

# create cleaned names
for (food_id in 1:100) {
  tasterat.df$Pic[tasterat.df$Pic == food_id] <- as.character(FoodNamesTop100[food_id, ])
}
# make as factor
tasterat.df$Pic <- factor(tasterat.df$Pic)

# tasterat.df %>% filter(SubjectNumber == 85) %>% View()
# wrangle to create the properly formatted data frame
tasterat.df %>%
  select(SubjectNumber, Rating, Pic) %>%
  spread(Pic, Rating) -> rating_network

# binary the dataframe (test both ways)
rating_network <- rating_network %>%
  select(-SubjectNumber) %>%
  map_df(~ ifelse(.x > 0, 1, 0))

#use median instead
median_ratings_subject <- apply(rating_network, 1, median)

# med_rating_network <- rating_network %>%
#   select(-SubjectNumber)
# 
# for (subject_idx in seq_along(median_ratings_subject)){
#   med_rating_network[subject_idx,] <- med_rating_network[subject_idx,] %>% map_df(~ ifelse(.x > median_ratings_subject[[subject_idx]], 1, 0))
# }
#THIS IS WHAT SWITCHES TO THE MED INSTEAD
# rating_network = med_rating_network
network <- bootnet::estimateNetwork(rating_network, 
                                    default = "LoGo",#Local/Global Sparse Inverse Covariance Matrix
                                    graphType = "pcor")

network <- bootnet::estimateNetwork(rating_network, 
                                    default = "TMFG",#Triangulated Maximally Filtered Graph:
                                    graphType = "pcor")
centralityPlot(network)
plot(network, layout = 'spring') 
Results1 <- bootnet::bootnet(network, nBoots = 100, nCores = 8)
Results1
plot(Results1$sample)
plot(Results1)
ress <- summary(Results1) 

# make sure we have mutiple cases of people liking an item
final.snack_food <- finalize(rating_network, minCase = 1)
########################
# Network estimation
# compute measure of similarity
cosine.snack_food <- SemNeT::similarity(med_rating_network, method = "cosine")
angular.snack_food <- SemNeT::similarity(final.snack_food, method = "angular")
jaccard.snack_food <- SemNeT::similarity(final.snack_food, method = "jaccard")
euclid.snack_food <- SemNeT::similarity(final.snack_food, method = "euclid")
cor.snack_food <- SemNeT::similarity(med_rating_network, method = "cor")

# parse the network
net.snack_food <- SemNeT::TMFG(cor.snack_food)

# get graph and remove self-connections
graph_ratings <- graph_from_adjacency_matrix(net.snack_food,
  "undirected",
  weighted = TRUE,
  add.colnames = T,
  diag = F
)

# visualize the matrix as a heatmap.
m <- as.matrix(net.snack_food)
# all of the diag(m) are equal to one, not sure why the graphic does not show this
mDim <- length(m[1, ]) # determine size of one dim of the matrix, which we assume is identical to the other dim.
heatmap(m[mDim:1, ],
  Rowv = NA, Colv = NA, scale = "column",
  margins = c(14, 13)
)

# calulate the correlations
r <- cor(rating_network, use = "complete.obs")

ggcorrplot(r,
  type = "upper",
  colors = c("blue", "white", "red"),
  ggtheme = ggplot2::theme_classic,
  outline.color = "white",
  show.diag = T
) + ggplot2::theme(
  axis.text.x = element_text(size = 4),
  axis.text.y = element_text(size = 4),
  axis.ticks = element_blank()
)

########################
# Network analysis

## global statistics
# NetworkToolbox::clustcoeff(graph_ratings)
# Degree <- degree(graph_ratings)
# strength(graph_ratings)
# Eig <- evcent(graph_ratings)$vector
# eigens <- as.data.frame(Eig)
# Hub <- hub.score(graph_ratings)$vector
# Authority <- authority.score(graph_ratings)$vector
# Closeness <- closeness(graph_ratings)
# Betweenness <- betweenness(graph_ratings)
#
# edge_density(graph_ratings)
# components(graph_ratings)
# par(mar=c(0,0,0,0) + 7)
# hist(degree(graph_ratings), breaks=10, col="gray")
#
# mean_distance(graph_ratings)
# transitivity(graph_ratings, "global")
# transitivity(graph_ratings, "local")
graph_ratings <- graph_from_adjacency_matrix(network$graph,
                            "undirected",
                            weighted = TRUE,
                            add.colnames = T,
                            diag = F)
########################
## community detection
c1 <- cluster_fast_greedy(graph_ratings)
c2 <- cluster_edge_betweenness(graph_ratings)
c3 <- cluster_infomap(graph_ratings)
c4 <- cluster_leading_eigen(graph_ratings)
c5 <- cluster_louvain(graph_ratings)
c6 <- cluster_walktrap(graph_ratings, steps = 4)
# c7 <- leiden(graph_ratings)

get_clustering_stats <- function(c) {
  # modularity measure for each algorithm
  print(modularity(c))
  # memberships of nodes
  # print(membership(c))
  # number of communities
  print(length(c))
  # size of communities
  print(sizes(c))
}
for (method in list(c1, c2, c3, c4, c5, c6)) {
  get_clustering_stats(method)
}

for (node_name in 1:100) {
  V(graph_ratings)$name[[node_name]] <- as.character(FoodNamesTop100[node_name, ])
}

# V(graph_ratings)[53]$color <- "dodgerblue1"
V(graph_ratings)$color <- "slateblue"

# The Fruchterman-Reingold layout algorithm
l2 <- layout_with_fr(graph_ratings)
# nicely also chooses this one too
l3 <- layout_nicely(graph_ratings)
l4 <- layout_with_dh(graph_ratings)

par(mar = c(0, 0, 0, 0))

# de <- igraph::degree(graph_ratings)
# plot(graph_ratings, vertex.label = "", vertex.color = "gold", edge.color = "slateblue", vertex.size = de)

plot(graph_ratings,
  layout = l3,
  margin = .0,
  vertex.label = V(graph_ratings)$name,
  vertex.label.color = "black",
  vertex.label.cex = .6,
  vertex.size = 2,
  vertex.label.family = "Times",
  edge.curved = .15,
  edge.width = .4,
  mark.groups = communities(c6)
)

sega_liking <- EGA(net.snack_food,
  n = 42, plot.EGA = TRUE, model = "TMFG", algorithm = "walktrap",
  plot.type = "qgraph",
  plot.args = list(vsize = 4, edge.alpha = 1)
)

summary(sega_liking)

boot_test <- EGAnet::bootEGA(net.snack_food, 
                             iter = 10,
                             n = 42,
                             model = "TMFG",
                             corr = "spearman")

# we can also simulate a network of the same size to work with
simulated_network <- sim.fluency(nodes = 100, cases = 42)


EL <- as_data_frame(net.snack_food, what = "edges")
?delete_edges
# if you want to look at a subset of the graph and calculate a statistics
?delete_vertices
snack_names <- tibble(names = V(graph_ratings)$name)
# try using subgraph instead
# ?igraph::subgraph()

remove_snacks <- snack_names$names[-c(48, 19, 62, 23, 67, 60, 75, 79, 97, 35, 27, 13, 83, 44, 93)]
unlist(remove_snacks)
g2 <- delete_vertices(graph_ratings, unlist(remove_snacks))
l4 <- layout_in_circle(g2)
V(g2)$color <- "forestgreen"
V(g2)[c(1, 3, 7, 12, 14)]$color <- "darkorange"
V(g2)[c(2, 4, 5, 8, 10)]$color <- "deeppink"

plot(g2,
  layout = l4,
  margin = .0,
  vertex.label.color = "black",
  vertex.label.cex = .8,
  vertex.size = 8,
  vertex.label.family = "Times",
  edge.curved = .15,
  edge.width = 4,
)

g2 <- g2 %>%
  set_vertex_attr("snack_type", V(g2)[c(1, 3, 7, 12, 14)], 1) %>%
  set_vertex_attr("snack_type", V(g2)[c(2, 4, 5, 8, 10)], 2) %>%
  set_vertex_attr("snack_type", V(g2)[c(6, 9, 11, 13, 15)], 3)

remove_snacks <- snack_names$names[-c(42, 48, 29, 22, 23, 28, 93, 43, 24, 92, 30, 78, 79, 49, 13)]
unlist(remove_snacks)
g3 <- delete_vertices(graph_ratings, unlist(remove_snacks))
l4 <- layout_in_circle(g3)
V(g3)$color <- "darkorange"

plot(g3,
  layout = l4,
  margin = .0,
  vertex.label.color = "black",
  vertex.label.cex = .8,
  vertex.size = 8,
  vertex.label.family = "Times",
  edge.curved = .15,
  edge.width = 4,
)
g3 <- g3 %>%
  set_vertex_attr("snack_type", V(g3), 1)

# parsed networks metrics

edge_density(g2)
edge_density(g3) # more dense

mean_distance(g2)
mean_distance(g3) # more separation

transitivity(g2, "global") # higher clustering
transitivity(g3, "global")

transitivity(g2, "localaverage") # hihger averager local clustering
transitivity(g3, "localaverage")


remove_snacks <- snack_names$names[c(48, 19, 62, 23, 67, 60, 75, 79, 97, 35, 27, 13, 83, 44, 93)]
net.snack_food_g2 <- net.snack_food[unlist(remove_snacks), unlist(remove_snacks)]
NetworkToolbox::clustcoeff(net.snack_food_g2)

remove_snacks <- snack_names$names[c(42, 48, 29, 22, 23, 28, 93, 43, 24, 92, 30, 78, 79, 49, 13)]
length(net.snack_food[unlist(remove_snacks), unlist(remove_snacks)])
net.snack_food_g3 <- net.snack_food[unlist(remove_snacks), unlist(remove_snacks)]
NetworkToolbox::clustcoeff(net.snack_food_g3)

# check assorativity
adj <- as_adjacency_matrix(g2, sparse = F, attr = "weight")
assortment.discrete(adj, V(g2)$snack_type, weighted = TRUE, SE = F, M = 1)
adj <- as_adjacency_matrix(g3, sparse = F, attr = "weight")
assortment.discrete(adj, V(g3)$snack_type, weighted = TRUE, SE = F, M = 1)


g <- graph_ratings
deg <- degree(g, mode = "all")
par(mar = c(0, 0, 0, 0) + 7)
deg.dist <- degree_distribution(g, cumulative = T, mode = "all")
plot(
  x = 0:max(deg), y = 1 - deg.dist, pch = 19, cex = 1.2, col = "orange",
  xlab = "Degree", ylab = "Cumulative Frequency"
)
