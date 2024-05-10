library(readr)
library(tidyverse)
suppressMessages(library(EGAnet)) # For Exploratory Graph Analysis
library(igraph)
library(RColorBrewer)

calculate_net_stats <- function(g) {
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
  
  net_degree$snack_type <- V(g)$snack_type
  
  return(net_degree)
}

Exp1B <- read_csv("~/Downloads/mem_dm_share-main/data/Exp1B.csv") %>% filter(ttype == "rating_task") 
Exp2C <- read_csv("~/Downloads/mem_dm_share-main/data/Exp2C.csv") %>% filter(ttype == "rating_task") 
image_word <- read_csv("~/Downloads/mem_dm_share-main/data/image_word.csv")

merge_image <- Exp1B %>% 
  mutate(url = image) %>% 
  left_join(image_word, by = "url") %>% 
  # select(ID, response, food.item, mem)
  select(ID, response, food.item)


merge_image$exp <- 1
merge_image$ID <- merge_image$ID + 100

merge_word <- Exp2C %>% 
  mutate(food.item = word) %>% 
  select(ID, response, food.item)
  # select(ID, response, food.item, mem)

merge_word$exp <- 2
merge_word$ID <- merge_word$ID + 200

merged_ratings <- rbind(merge_image, merge_word) %>% 
  # mutate_at(vars(mem), as.numeric) %>% 
  mutate(
    food.item = gsub("\\s", "", food.item), # Remove all spaces
    food.item = tolower(food.item) # Convert to lowercase
  ) %>% 
  pivot_wider(
    names_from = food.item,  # Create columns from pic_name
    values_from = response  # Fill the cells with the values from the value column
  ) %>% select(-ID, -exp) %>% 
  mutate_at(vars(everything()), as.numeric)

EGAnet::EGA.fit(merged_ratings, model = "TMFG", algorithm = "walktrap", corr = "pearson")
EGAnet::EGA.fit(merged_ratings, model = "glasso", algorithm ="louvain", corr = "spearman")

ega_res <- EGAnet::bootEGA(merged_ratings,
                           iter = 5000,
                           n = nrow(merged_ratings),
                           model = "TMFG",
                           algorithm = "walktrap",
                           type = "parametric",
                           ncores = 10,
                           typicalStructure = TRUE)

bapq.dimstab <- dimensionStability(ega_res)
bapq.dimstab$dimension.stability$structural.consistency
bapq.dimstab$dimension.stability$average.item.stability
# bapq.dimstab$item.stability$plot +
#   ggplot2::scale_color_brewer(palette = "Set3")

test <- bapq.dimstab$item.stability
stable_items <- names(test$item.stability$empirical.dimensions[test[["item.stability"]][["empirical.dimensions"]] > .50])
EGAnet::EGA.fit(merged_ratings[,stable_items], model = "glasso", algorithm = "walktrap", corr = "pearson")
# EGAnet::EGA.fit(merged_ratings[,stable_items], model = "TMFG", algorithm = "walktrap")
# EGAnet::hierEGA(merged_ratings[,stable_items], plot.type = "separate")

ega_rating_res <- EGAnet::bootEGA(merged_ratings[,stable_items],
                           iter = 3000,
                           n = nrow(merged_ratings),
                           model = "glasso",
                           algorithm = "walktrap",
                           type = "parametric",
                           ncores = 10,
                           typicalStructure = TRUE)

bapq.dimstab <- dimensionStability(ega_rating_res)
bapq.dimstab$dimension.stability$structural.consistency
bapq.dimstab$dimension.stability$average.item.stability

A <- ega_rating_res[["typicalGraph"]][["graph"]]
dimattributes <- ega_rating_res[["typicalGraph"]][["wc"]]
g <- igraph::graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
igraph::V(g)$snack_type <- dimattributes

memres <- EGAnet::community.consensus(A, consensus.method = "iterative", consensus.iter = 10000)
# V(g)$snack_type <- memres #this would be the community consensus results

save(ega_rating_res, g, file = here::here("data", "bakkour_rating_network_graphV4.RData"))
# load(file = here::here("data", "bakkour_rating_network_graphV4.RData"))

# dput(brewer.pal(n = 8, name = 'Accent'))
# display.brewer.pal(8,"Accent")

G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))

net_degree <- calculate_net_stats(g)

# l <- layout_nicely(g)
l <- layout_with_graphopt(g)
# l <- layout_with_gem(g)
# l <- layout.mds(g)

net_degree <- net_degree%>%
  mutate(colors =
           case_when(
             snack_type == 1 ~ "#7FC97F",
             snack_type == 2 ~ "#BEAED4",
             snack_type == 3 ~ "#FDC086",
             snack_type == 4 ~ "#666666", 
             snack_type == 5 ~ "#386CB0",
             snack_type == 6 ~ "#F0027F",
             snack_type == 7 ~ "#BF5B17",
           )
  )

V(g)$color <- net_degree$colors

E(g)$color[E(g)$weight > 0] <- "forestgreen"
E(g)$color[E(g)$weight < 0] <- "red2"

plot(g,
     layout = l,
     margin = .0,
     vertex.label = NA,
     vertex.label.color = "black",
     label.font = 2,
     vertex.frame.color=adjustcolor(net_degree$colors, alpha.f = .1),
     vertex.label.dist	= 1,
     vertex.label.cex = 1,
     vertex.size = 9,
     vertex.label.family = "Times",
     edge.width = E(g)$weight * 3.7
)
#NOTE you need to check labels are correct here
legend(x=1.3, 
       y=.6, 
       c("fruits", "sweets","cheese","veggie","meat","protein","dairy"), 
       pch=21, 
       pt.bg=c("#7FC97F", "#BEAED4", "#FDC086", "#666666", "#386CB0", "#F0027F", 
               "#BF5B17"),
       pt.cex=2.5, 
       cex=1.5, 
       bty="n", 
       ncol=1)

plot(g,
     layout = l,
     vertex.shape="none",
     vertex.label.cex=.7,
     vertex.label = V(g)$name,
     vertex.label.font = 2,
     vertex.label.color=net_degree$colors,
     vertex.size = NULL,
     vertex.label.family = "Times",
     edge.width = E(g)$weight
)
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

