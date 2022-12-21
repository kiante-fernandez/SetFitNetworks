# subgraph_selection.R - algorithm for selecting sub graphs from preference network

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
# 2022/10/08      Kianté  Fernandez                    coded up cleaned version one
# 2022/12/15      Kianté  Fernandez                    modularity detection code
# 2022/12/19      Kianté  Fernandez                    general plot diagnositcs

# Libraries

library(igraph) # Network Analysis and Visualization
library(assortnet) # Calculate the Assortativity Coefficient of Weighted and Binary Networks
library(EGAnet) # Exploratory Graph Analysis – a Framework for Estimating the Number of Dimensions in Multivariate Data using Network Psychometrics
library(here) # A Simpler Way to Find Your Files

source("exploratory_graph_analysis.R")

#### subgraph algorithm
make_subgraphs <- function(g, C = 6, stat_range, nsubgraphs = 25, n_statistic = "assortment", epsilon = 0.05, interations = 250) {
  # list of network statistics build into function or to be built

  # assortment.discrete:Assortment on discrete vertex values
  # Calculates the assortativity coefficient for weighted graph with categorical vertex values

  # weighted_clustering_coefficient: Weighted clustering coefficient of a weighted graph.
  # Weighted clustering Computed using the definition given by McAssey, M. P. and Bijma, F. in "A clustering coefficient for complete weighted networks" (2015).

  # edge_density: Graph density
  # The density of a graph is the ratio of the number of edges and the number of possible edges.

  # diversity: Diversity Coefficient
  # Values closer to 1 suggest greater between-community connectivity and values closer to 0 suggest greater within-community connectivity

  # transitivity (in some cases this will be similar to the clustering_coefficient)
  # Computes transitivity of a network

  # smallworldness Small-worldness Measure
  # Computes the small-worldness measure of a network
  # smallworldness(adj_temp,iter = 100)

  # internal_density: Internal Density
  # Internal density of a graph's communities.
  # That is, the sum of weights of their edges divided by the number of unordered pairs of vertices (which is the number of potential edges).
  # clustAnalytics::internal_density(gt,V(gt)$snack_type)

  # average_degree: Average Degree
  # Average degree (weighted degree, if the graph is weighted) of a graph's communities.
  ## clustAnalytics::average_degree(gt,V(gt)$snack_type)
  library(progress)

  pb <- progress_bar$new(
    format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
    total = nsubgraphs,
    complete = "=", # Completion bar character
    incomplete = "-", # Incomplete bar character
    current = ">", # Current bar character
    clear = FALSE, # If TRUE, clears the bar when finish
    width = 100
  ) # Width of the progress bar

  nnodes <- length(V(g)) # number of nodes

  rT <- seq(from = stat_range[[1]], to = stat_range[[2]], length.out = nsubgraphs) # target value for statistic
  # rT <- round(seq(from = stat_range[[1]], to = stat_range[[2]], length.out = nsubgraphs),3) # target value for statistic

  rS <- vector(mode = "numeric", length = nsubgraphs) # estimated value for  statistic

  subgraphs <- vector(mode = "list", length = nsubgraphs)
  # difference between the target value and calculated r
  r_diff <- NULL

  for (graph_idk in seq_len(nsubgraphs)) {
    # Updates the current state
    pb$tick()
    dign_tic <- 1
    diganostic_rS <- vector(mode = "numeric", length = interations)

    repeat {
      # get starting subgraph
      size <- sample(seq_len(nnodes), C, replace = F)
      # pull out a candidate subgraph
      gt <- igraph::induced_subgraph(g, size)
      adj_temp <- igraph::as_adjacency_matrix(gt, sparse = F, attr = "weight")
      # save value of r
      if (n_statistic == "assortment") {
        rS[[graph_idk]] <- assortnet::assortment.discrete(adj_temp, V(gt)$snack_type, weighted = TRUE, SE = F)$r
      } else if (n_statistic == "edge_density") {
        rS[[graph_idk]] <- igraph::edge_density(gt)
      } else if (n_statistic == "weighted_clustering_coefficient") {
        rS[[graph_idk]] <- clustAnalytics::weighted_clustering_coefficient(gt, upper_bound = 1)
      } else if (n_statistic == "diversity") {
        temp_stat <- NetworkToolbox::diversity(adj_temp, V(gt)$snack_type)$overall
        temp_stat[!is.finite(temp_stat)] <- NA
        rS[[graph_idk]] <- mean(temp_stat, na.rm = T)
      } else if (n_statistic == "internal_density") {
        temp_stat <- clustAnalytics::internal_density(gt, V(gt)$snack_type)
        temp_stat[is.nan(temp_stat)] <- NA
        rS[[graph_idk]] <- mean(temp_stat, na.rm = T)
      } else if (n_statistic == "average_degree") {
        temp_stat <- clustAnalytics::average_degree(gt, V(gt)$snack_type)
        temp_stat[is.nan(temp_stat)] <- NA
        rS[[graph_idk]] <- mean(temp_stat, na.rm = T)
      } else if (n_statistic == "smallworldness") {
        rS[[graph_idk]] <- NetworkToolbox::smallworldness(adj_temp, iter = 100, method = "TJHBL")$swm
      } else if (n_statistic == "modularity") {
        rS[[graph_idk]] <- igraph::modularity(gt, V(gt)$snack_type)
        V(gt)$mod <- rS[[graph_idk]]
      }


      print(rS[[graph_idk]])
      diganostic_rS[[dign_tic]] <- rS[[graph_idk]]
      dign_tic <- dign_tic + 1

      if (dign_tic == interations) {
        temp <- diganostic_rS
        plt <- ggplot2::qplot(temp) + ggplot2::theme_classic()
        warning("ERROR: could not find a graph within range, check graph")
        # tryCatch(stop(e), error = function(e) e, finally = print("Hello"))

        return(list(temp, plt))
      }
      # store a set of them to examine a range visually:
      # use a tick system and have it break and give a graph if the solution
      # does not give an answer
      # throw an error message:"could not find a graph within range,
      # please select a new epsilon of range of values

      if (is.na(rS[[graph_idk]])) {
        next
      }
      r_diff <- abs(rS[[graph_idk]] - rT[[graph_idk]])
      if (r_diff < epsilon && round(rS[[graph_idk]], 5) != 0) { #check if close and non-zero
        break
      }
    }

    subgraphs[[graph_idk]] <- gt
  }
  return(subgraphs)
}

# initial random sample of graph of size C
C <- 6
# number sub graphs to generate
nsubgraphs <- 100
# list of names of each network statistic to calculate
# network_stats <- c("assortment","edge_density","weighted_clustering_coefficient","average_degree","internal_density","diversity")
# network_stats <- c("assortment","edge_density","weighted_clustering_coefficient")
network_stats <- c("modularity")


# range of target value for each statistic
stat_ranges <- list(
  c(-.95, .95),
  c(0, .7),
  c(0.05, .95), # epsilon = 0.06
  c(0, 0.15), # epsilon = .005
  c(0, 0.4), # note epsilon = .005
  c(-4, .8)
) # note eepsilon = .15

epsilons <- list(0.05, 0.05, 0.06, 0.005, 0.005, .15)

stat_ranges <- list(c(-.5, 0.70)) #modularity range
subgraphs <- make_subgraphs(g, C,
  stat_range = stat_ranges[[1]],
  nsubgraphs = nsubgraphs,
  n_statistic = "modularity",
  epsilon = .079,
  interations = 4000
)

# check how many unique values of modularity we have
length(unique(round(unlist(purrr::map(subgraphs, function(.) {
  V(.)$mod[[1]]
})), 4))) # 36


# ?make_subgraphs
# get image of all the sim subgraphs together
# needs to be adjusted for changes in nsubgraphs
library(jpeg)
food_folder <- here::here("data", "snackitemnames_nicholas", "Lee_Holyoak_2021_images")
FoodNames <- readxl::read_excel(here::here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))

temp <- list.files(path = food_folder, pattern = "*.jpg", full.names = T)
foods_in_image <- stringr::str_extract(temp, "item\\d+")
foods_in_image <- stringr::str_extract(foods_in_image, "\\d+")
# get row idx for each of the image numbers
foods_in_image <- tibble::rowid_to_column(data.frame(Image = as.numeric(foods_in_image)))
foods_in_image <- dplyr::left_join(FoodNames, foods_in_image, "Image")

par(mfrow = c(3, 10)) # set the plotting area into a 1*2 array

network_stat_idx <- 1
nsubgraphs <- 100
epsilons <- list( .079)
for (network_stat_idx in seq_len(length(network_stats))) {
  print(paste0("GENERATING SUBGRAPHS FOR: ", network_stats[[network_stat_idx]]))

  subgraphs <- make_subgraphs(g, C, stat_range = stat_ranges[[network_stat_idx]], nsubgraphs = nsubgraphs, n_statistic = network_stats[[network_stat_idx]], epsilon = epsilons[[network_stat_idx]], interations = 4000)
  rT <- seq(from = stat_ranges[[network_stat_idx]][[1]], to = stat_ranges[[network_stat_idx]][[2]], length.out = nsubgraphs) # target value for statistic

  for (graph_idk in 1:nsubgraphs) {
    l <- layout_in_circle(subgraphs[[graph_idk]])
    V(subgraphs[[graph_idk]])$color <- V(subgraphs[[graph_idk]])$snack_type
    E(subgraphs[[graph_idk]])$color[E(subgraphs[[graph_idk]])$weight > 0] <- "forestgreen"
    E(subgraphs[[graph_idk]])$color[E(subgraphs[[graph_idk]])$weight < 0] <- "red2"

    # plot(subgraphs[[graph_idk]],
    #   layout = l,
    #   margin = .0,
    #   vertex.label.color = "black",
    #   vertex.label.cex = 1.2,
    #   vertex.label.dist = .9,
    #   vertex.size = 20,
    #   vertex.label.family = "Times",
    #   edge.curved = .01,
    #   edge.width = abs(E(subgraphs[[graph_idk]])$weight) * 8,
    #   main = paste0(network_stats[[network_stat_idx]], " = ", round(V(subgraphs[[graph_idk]])$mod[[1]],5))
    # )
    #with images
    adj_temp <- igraph::as_adjacency_matrix(subgraphs[[graph_idk]], sparse = F, attr = "weight")
    nodes_temp <- temp[foods_in_image$rowid[foods_in_image$Name %in% V(subgraphs[[graph_idk]])$name]]
    
    qgraph::qgraph(adj_temp,
                   layout = l,
                   palette = "ggplot2",
                   labels = F,
                   images = nodes_temp,
                   edge.width = abs(E(subgraphs[[graph_idk]])$weight) * .7,
                   title = paste0(network_stats[[network_stat_idx]], " = ", round(V(subgraphs[[graph_idk]])$mod[[1]],5))
    )
    dev.copy(png, filename = here("figures", paste0(network_stats[[network_stat_idx]], "_", graph_idk, "_", C, ".png")), width = 18, height = 12, units = "in", res = 300)
    dev.off()
  }
  # dev.copy(png, filename = here("figures", paste0(network_stats[[network_stat_idx]], "_", nsubgraphs, "_", C, ".png")), width = 18, height = 12, units = "in", res = 300)
  # dev.off()
  # now take the value for each generated sub graph and create a stimuli
  res <- tibble::tibble(rep(0, C))
  for (graph_idk in seq_len(nsubgraphs)) {
    temp <- tibble::tibble(stim_set = V(subgraphs[[graph_idk]])$name)
    # tmep <- dplyr::rename(temp, paste0("stim", graph_idk) = stim_set)
    res <- cbind(res, temp)
  }
  # remove the temp
  res <- res[, -1]
  #TO SAVE this must be on
  save(subgraphs,res, file = here("data",   paste0(network_stats[[network_stat_idx]],"_",nsubgraphs,"_", C, ".RData")))
}
# now you can take the res results over to the generate_image_group.R



# permutation testing to find significant graphs at that estimate
# create simulation function

# simulate_network <- function(g) {
#   # permute graph
#   g1 <- rewire(g, with = keeping_degseq(niter = vcount(g) * 100))
#   # check weighted add weights
#   if (!is.null(E(g1)$weight)) {
#     E(g1)$weight <- sample(E(g)$weight)
#   }
#   adj_temp <- as_adjacency_matrix(g1, sparse = F, attr = "weight")
#   assortment.discrete(adj_temp, V(g1)$snack_type, weighted = TRUE, SE = F)$r
# }
# # set number of permutations
# nPerm <- 500
# p <- vector(mode = "list", length = 50)
# Switch <- F
#
# for (graph_idk in 1:50) {
#   # run simulations
#   r0 <- replicate(nPerm, simulate_network(subgraphs[[graph_idk]]))
#
#   if (1 - mean(r0 < rS[[graph_idk]]) < 0.05) {
#     Switch <- F
#   } else {
#     Switch <- T
#   }
#
#   # plot it
#   p[[graph_idk]] <- qplot(r0, bins = 100) +
#     geom_vline(xintercept = rS[[graph_idk]], color = "red") +
#     geom_vline(xintercept = rT[[graph_idk]], color = "blue") +
#     labs(title = paste0(round(rS[[graph_idk]], 3))) + theme_classic() + {
#       if (Switch) theme(panel.background = element_rect(fill = "red"))
#     }
# }
# wrap_plots(p)

############# alternative to generating subgraphs with certain stats

library(dplyr)

food_folder <- here::here("data", "snackitemnames_nicholas", "Lee_Holyoak_2021_images")
FoodNames <- readxl::read_excel(here::here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))

temp <- list.files(path = food_folder, pattern = "*.jpg", full.names = T)
foods_in_image <- stringr::str_extract(temp, "item\\d+")
foods_in_image <- stringr::str_extract(foods_in_image, "\\d+")
# get row idx for each of the image numbers
foods_in_image <- tibble::rowid_to_column(data.frame(Image = as.numeric(foods_in_image)))
foods_in_image <- dplyr::left_join(FoodNames, foods_in_image, "Image")

G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
TEST <- distances(G)
diag(TEST) <- NA
apply(TEST, 2, mean, na.rm = T)
adj_temp <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")

# here I calculate a range of metrics on the graph
net_degree <- data.frame(
  degree = degree(g),
  strength = strength(g),
  eigen = igraph::eigen_centrality(g)$vector,
  weighted_transitivity = transitivity(g, type = "weighted"),
  closeness = NetworkToolbox::closeness(adj_temp, weighted = TRUE),
  closeness2 = closeness(G), betweenness = betweenness(G),
  average_path_length = apply(TEST, 2, mean, na.rm = T)
) %>%
  tibble::rownames_to_column("Name") %>%
  dplyr::left_join(foods_in_image, "Name")

net_degree$snack_type <- V(g)$snack_type

net_degree$group <- cut(degree(g), 3, labels = c("low", "mid", "high"))

net_degree %>%
  dplyr::group_by(snack_type) %>%
  dplyr::summarise(n = n())

nhigh <- net_degree[net_degree$group != "high", ]
nlow <- net_degree[net_degree$group != "low", ]
# sample from only a single cluster

nsubgraphs <- 25
# initial random sample of graph of size C
C <- 6

subgraphs1 <- vector(mode = "list", length = nsubgraphs)
subgraphs2 <- vector(mode = "list", length = nsubgraphs)
subgraphs3 <- vector(mode = "list", length = nsubgraphs)
subgraphs4 <- vector(mode = "list", length = nsubgraphs)

# pull out a candidate subgraph
for (graph_idk in 1:nsubgraphs) {
  # get subgraph sample
  # #"sample the snacks with low degree" within cluster
  # lwsize <- try(sample(nhigh[nhigh$snack_type == sample(1:7, 1, replace = T),]$Item, C, replace = F))
  # if("try-error" %in% class(lwsize)){
  #   lwsize <- sample(nhigh[nhigh$snack_type %in% c(1,5,6,7),]$Item, C, replace = F)
  #   }
  #
  #
  # #"sample the snacks with high degree" within cluster
  # hwsize <- try(sample(nlow[nlow$snack_type %in% c(1,5,6,7),]$Item, C, replace = F))
  # if("try-error" %in% class(hwsize)){
  #   hwsize <- sample(nlow[nlow$snack_type  %in% c(1,5,6,7),]$Item, C, replace = F)
  # }
  #
  # get a random set of foods from the smaller clusters
  rsize <- try(sample(net_degree[net_degree$snack_type %in% c(1, 5, 6, 7), ]$Item, C, replace = F))
  # get a random set of foods from only two of the largest cluster
  wsize <- try(sample(net_degree[net_degree$snack_type %in% c(2, 3), ]$Item, C, replace = F))

  # "sample the snacks with high degree"
  hosize <- try(sample(nlow$Item, C, replace = F))
  # "sample the snacks with low degree"
  losize <- try(sample(nhigh$Item, C, replace = F))

  subgraphs1[[graph_idk]] <- igraph::induced_subgraph(g, rsize)
  subgraphs2[[graph_idk]] <- igraph::induced_subgraph(g, wsize)
  subgraphs3[[graph_idk]] <- igraph::induced_subgraph(g, losize)
  subgraphs4[[graph_idk]] <- igraph::induced_subgraph(g, hosize)
}

subgraphs <- c(subgraphs1, subgraphs2, subgraphs3, subgraphs4)

# save the graphs names to create the images
# now take the value for each generated sub graph and create a stimuli
res <- tibble::tibble(rep(0, C))
for (graph_idk in seq_len(100)) {
  temp <- tibble::tibble(stim_set = V(subgraphs[[graph_idk]])$name)
  res <- cbind(res, temp)
}
# remove the temp
res <- res[, -1]
# save the set of graphs
save(subgraphs, res, file = here("data", "LowHighWithinBetween.RData"))

# par(mfrow = c(5, 10)) # set the plotting area into a 1*2 array
#
# for (graph_idk in 51:100) {
#   l <- layout_in_circle(subgraphs[[graph_idk]])
#   V(subgraphs[[graph_idk]])$color <- V(subgraphs[[graph_idk]])$snack_type
#   E(subgraphs[[graph_idk]])$color[E(subgraphs[[graph_idk]])$weight > 0] <- "forestgreen"
#   E(subgraphs[[graph_idk]])$color[E(subgraphs[[graph_idk]])$weight < 0] <- "red2"
#
#   plot(subgraphs[[graph_idk]],
#        layout = l,
#        margin = .0,
#        vertex.label.color = "black",
#        vertex.label.cex = 1,
#        vertex.label.dist = .7,
#        vertex.size = 21,
#        vertex.label.family = "Times",
#        edge.curved = .05,
#        edge.width = abs(E(subgraphs[[graph_idk]])$weight) * 7)
# }
