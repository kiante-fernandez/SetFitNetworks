# utils

library(purrr) # Functional Programming Tools
suppressPackageStartupMessages(library(tidyverse)) # Easily Install and Load the 'Tidyverse'
library(jsonlite) # A Simple and Robust JSON Parser and Generator for R

# helper functions for working with lists
list.do <- function(.data, fun, ...) {
  do.call(what = fun, args = as.list(.data), ...)
}
list.cbind <- function(.data) {
  list.do(.data, "cbind")
}

load_food_names <- function(){
  # load all the images to calculate the value for a group of foods
  food_folder <- here::here("data", "snackitemnames_nicholas", "Lee_Holyoak_2021_images")
  FoodNames <- readxl::read_excel(here::here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))
  temp <- list.files(path = food_folder, pattern = "*.jpg", full.names = T)
  foods_in_image <- stringr::str_extract(temp, "item\\d+")
  foods_in_image <- stringr::str_extract(foods_in_image, "\\d+")
  # get row idx for each of the image numbers
  foods_in_image <- tibble::rowid_to_column(data.frame(Image = as.numeric(foods_in_image)))
  foods_in_image <- dplyr::left_join(FoodNames, foods_in_image, "Image")
  return(list(FoodNames = FoodNames,foods_in_image = foods_in_image))
}


NetworkStat <- function(subgraph) {
  ## %######################################################%##
  #                                                          #
  ### calculate a bunch of micro and mesoscale measures#####
  #                                                          #
  ## %######################################################%##
  G <- subgraph
  E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
  adj_temp <- igraph::as_adjacency_matrix(subgraph, sparse = F, attr = "weight")

  net_stat_temp <- data.frame(
    degree = degree(subgraph),
    strength = strength(subgraph),
    eigen = igraph::eigen_centrality(G)$vector,
    page_rank = page_rank(subgraph)$vector, # weighted
    weighted_transitivity = transitivity(subgraph, type = "weighted"),
    closeness = NetworkToolbox::closeness(adj_temp, weighted = TRUE),
    closeness2 = closeness(G), # weighted
    betweenness = betweenness(G),
    participation = NetworkToolbox::participation(adj_temp, comm = V(subgraph)$snack_type)$overall
  ) %>%
    tibble::rownames_to_column("Name")

  return(net_stat_temp)
}

calculate_net_stats <- function(g){
  ##another version of calculating the netstats of various measures for a graph
  G <- g
  E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
  path_lengths <- distances(G)
  diag(path_lengths) <- NA # path length to oneself is zero
  
  adj_temp <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")
  
  # here I calculate a range of metrics on the graph
  net_degree <- data.frame(
    degree = degree(g),
    strength = strength(g),
    eigen = igraph::eigen_centrality(G)$vector,
    page_rank = page_rank(g)$vector, # weighted
    weighted_transitivity = transitivity(g, type = "weighted"),
    closeness = NetworkToolbox::closeness(adj_temp, weighted = TRUE),
    closeness2 = closeness(G), # weighted
    betweenness = betweenness(G),
    participation = NetworkToolbox::participation(adj_temp, comm = V(g)$snack_type)$overall,
    sds = apply(cor_snack_food, 2, sd)
  ) %>%
    tibble::rownames_to_column("Name") %>%
    dplyr::left_join(load_food_names()$foods_in_image, "Name")
  
  net_degree$snack_type <- V(g)$snack_type
  return(net_degree)
}

network_stats <- c("assortment", "edge_density", "weighted_clustering_coefficient", 
                   "LowHighWithinBetween","modularity")

organize_group_data <- function(file_idx = 75, net_stat = "modularity") {
  ############################
  ## organize the data and calculate value of group of items and net stats for each subject
  ##
  subject_df <- vector(mode = "list", length = file_idx)
  for (pp in seq_len(file_idx)) {
    # load the  subjects data
    subject_temp <- jsonlite::parse_json(jsonlite::read_json(temp_files[[pp]]), simplifyVector = T)
    # this gets the ratings in check
    subject_rating_temp <- subject_temp %>%
      filter(screen_id == "ratings") %>%
      select(stimulus, response) %>%
      mutate(
        Image = stringr::str_remove(stimulus, pattern = "../../img/60Foods/item"),
        Image = as.numeric(stringr::str_remove(Image, pattern = ".jpg"))
      ) %>%
      dplyr::left_join(load_food_names()$foods_in_image, by = "Image")
    
    #normalize the ratings?
    # subject_rating_temp$responsenormalized <- (unlist(subject_rating_temp$response)  - min(unlist(subject_rating_temp$response) )) / range(unlist(subject_rating_temp$response)) # xnormalized = (x - min(x)) / range(x)
    # for(trial in 1:60){subject_rating_temp$response[[trial]][[1]] <- subject_rating_temp$responsenormalized[trial]}
    
    ns <- which.max(map_dbl(map(network_stats, grepl, x = subject_temp$options[subject_temp$screen_id == "task"]), sum))
    
    set_values_temp <- vector(mode = "numeric", length = 100)
    set_network_temp <- vector(mode = "numeric", length = 100)
    set_cluster_temp <- vector(mode = "numeric", length = 100)
    set_correlations_temp <- vector(mode = "numeric", length = 100)
    set_sd_temp <- vector(mode = "numeric", length = 100)
    
    # TODO try also the sum SD of the ratings
    
    # foo = 1
    for (foo in 1:100) {
      # select which stat to calculate
      
      #this section calculates each of the subgraph stats based on the induced 
      #subgraph rather than the larger network. To get node importance 
      #measures at the level of the entire graph, this code would need to change
      
      # pull out a candidate sub graph
      size <- net_degree[net_degree$Name %in% res[[foo]], ]$Item
      subgraph <- igraph::induced_subgraph(g, size)
      graph_stats <- NetworkStat(subgraph)
      
      if (net_stat == "degree") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$degree)
        
      } else if (net_stat == "strength") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$strength)

      } else if (net_stat == "weighted_transitivity") {
        # pull out a candidate sub graph
        size <- net_degree[net_degree$Name %in% res[[foo]], ]$Item
        gt <- igraph::induced_subgraph(g, size)
        # calculate stat
        clust_temp <- transitivity(gt, "global")
        set_network_temp[[foo]] <- clust_temp

      } else if (net_stat == "eigen") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$eigen)

      } else if (net_stat == "closeness") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$closeness)

      } else if (net_stat == "betweenness") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$betweenness)

      } else if (net_stat == "page_rank") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$page_rank)

      } else if (net_stat == "participation") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$participation)

      } else if (net_stat == "assortment") {
        # assortnet
        # pull out a candidate sub graph
        size <- net_degree[net_degree$Name %in% res[[foo]], ]$Item
        gt <- igraph::induced_subgraph(g, size)
        adj_temp <- igraph::as_adjacency_matrix(gt, sparse = F, attr = "weight")
        # calculate stat
        assort_temp <- assortnet::assortment.discrete(adj_temp, V(gt)$snack_type, weighted = TRUE, SE = F)$r
        set_network_temp[[foo]] <- assort_temp
      } else if (net_stat == "edge_density") {
        # calculate stat
        ed_temp <- as.numeric(edge_density(subgraph))
        set_network_temp[[foo]] <- ed_temp
      } else if (net_stat == "diversity") {
        # pull out a candidate sub graph
        size <- net_degree[net_degree$Name %in% res[[foo]], ]$Item
        gt <- igraph::induced_subgraph(g, size)
        adj_temp <- igraph::as_adjacency_matrix(gt, sparse = F, attr = "weight")
        
        # calculate stat
        diverse_temp <- NetworkToolbox::diversity(adj_temp, V(gt)$snack_type)$overall
        diverse_temp[!is.finite(diverse_temp)] <- NA
        
        set_network_temp[[foo]] <- sum(diverse_temp, na.rm = T)
      } else if (net_stat == "efficiency") {
        # pull out a candidate sub graph
        
        E(subgraph)$weight <- 2**((E(subgraph)$weight - min(E(subgraph)$weight)) / diff(range(E(subgraph)$weight)))
        # calculate stat
        leverage_temp <- as.numeric(igraph::global_efficiency(subgraph, directed = F))
        set_network_temp[[foo]] <- as.numeric(leverage_temp)
      } else if (net_stat == "modularity") {
        # pull out a candidate sub graph
        impact_temp <- as.numeric(modularity(subgraph, V(subgraph)$snack_type))
        
        set_network_temp[[foo]] <- impact_temp
        
        # print(impact_temp)
      }
      #for modularity you want to subtract a constant so the scale is all positive
      set_network_temp <- set_network_temp - min(set_network_temp)
      
      set_values_temp[[foo]] <- sum(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
      # set_values_temp[[foo]] <- mean(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
      
      set_correlations_temp[[foo]] <- sum(apply(cor_snack_food[colnames(cor_snack_food) %in% res[[foo]], ], 2, mean, na.rm = T)[res[[foo]]])
      set_sd_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$sds)
      
    }
    
    task_temp <- subject_temp %>%
      filter(screen_id == "task") %>%
      select(subject_id, rt, options, key_press) %>%
      mutate(key_press = ifelse(key_press == "f", 1, 0)) # if left 1, ow right 0
    xxxx <- as.data.frame(do.call(rbind, task_temp$options)) %>% mutate(subject_id = pp)
    xxxx[, 1] <- as.numeric(str_remove(str_remove(xxxx[, 1], pattern = paste0("../../img/grid_stimuli/grid_6_", network_stats[[ns]], "_")), ".jpg"))
    xxxx[, 2] <- as.numeric(str_remove(str_remove(xxxx[, 2], pattern = paste0("../../img/grid_stimuli/grid_6_", network_stats[[ns]], "_")), ".jpg"))
    xxxx$rt <- task_temp$rt
    xxxx$choice <- task_temp$key_press
    names(xxxx) <- c("left", "right", "subject_id", "rt", "choice")
    xxxx$network_statistic <- network_stats[[ns]]
    
    # define variable names for left and right
    xxxx$left_rating <- NULL
    xxxx$right_rating <- NULL
    xxxx$left_net <- NULL
    xxxx$right_net <- NULL
    xxxx$left_correlation <- NULL
    xxxx$right_correlation <- NULL
    xxxx$left_sd <- NULL
    xxxx$right_sd <- NULL
    # foo <- 1
    for (foo in seq_len(nrow(xxxx))) {
      xxxx$left_rating[[foo]] <- as.numeric(set_values_temp[xxxx$left[[foo]]])
      xxxx$right_rating[[foo]] <- as.numeric(set_values_temp[xxxx$right[[foo]]])
      xxxx$left_net[[foo]] <- as.numeric(set_network_temp[xxxx$left[[foo]]])
      xxxx$right_net[[foo]] <- as.numeric(set_network_temp[xxxx$right[[foo]]])
      xxxx$left_correlation[[foo]] <- as.numeric(set_correlations_temp[xxxx$left[[foo]]])
      xxxx$right_correlation[[foo]] <- as.numeric(set_correlations_temp[xxxx$right[[foo]]])
      xxxx$left_sd[[foo]] <- as.numeric(set_sd_temp[xxxx$left[[foo]]])
      xxxx$right_sd[[foo]] <- as.numeric(set_sd_temp[xxxx$right[[foo]]])
    }
    xxxx$value_network_corr <- cor(set_values_temp, set_network_temp)
    xxxx$value_network_corr_p <- cor.test(set_values_temp, set_network_temp)$p.value
    
    subject_df[[pp]] <- xxxx
  }
  
  df <- as.data.frame(do.call(rbind, subject_df)) %>%
    unnest(cols = c(
      left_rating, right_rating, left_net, right_net,
      left_correlation, right_correlation, left_sd, right_sd
    ))
  #add the correct response col
  df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))
  
  return(df)
}


