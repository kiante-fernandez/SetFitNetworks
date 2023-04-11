# weight = "betweenness"
experiment = 2
organize_group_data <- function(experiment, weight = "degree") {
  
  # which data set are we working with?
  if (experiment == 1) {
    temp_files <- list.files(path = here::here("data", "pilot_30"), pattern = ".json", full.names = T)
    network_stats <- "LowHighWithinBetween"
    # load subgraphs
    load(file = here::here("data", "LowHighWithinBetween.RData"))
    #if you exclude certain "non- significant graphs" which ones? (see subgraph_permutation_testing.R)
    res_sig <- c(1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1,
                 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1,
                 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1,
                 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1,
                 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1)
    
  } else if (experiment == 2) {
    temp_files <- list.files(path = here::here("data", "exp_2"), pattern = ".json", full.names = T)
    network_stats <- "modularity"
    # load subgraphs
    load(file = here::here("data", "modularity_100_6.RData"))
    #if you exclude certain "non- significant graphs" which ones? (see subgraph_permutation_testing.R)
    res_sig<- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1,
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    
  }
  # will get used to calculate network meausres
  G <- g
  E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
  mem <- membership(cluster_leading_eigen(G))
  
  file_idx <- length(temp_files) # how many subjects data to preprocess
  ############################
  ## organize the data and calculate value of group of items and net stats for each subject
  ##
  subject_df <- vector(mode = "list", length = file_idx)
  # pp =1
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
    
    ns <- which.max(map_dbl(map(network_stats, grepl, x = subject_temp$options[subject_temp$screen_id == "task"]), sum))
    
    set_values_temp <- vector(mode = "numeric", length = 100)
    set_values_MAX_temp <- vector(mode = "numeric", length = 100)
    set_values_MIN_temp <- vector(mode = "numeric", length = 100)
    set_network_temp <- vector(mode = "numeric", length = 100)
    #micro
    set_degree_temp <- vector(mode = "numeric", length = 100)
    set_strength_temp <- vector(mode = "numeric", length = 100)
    set_eigen_temp <- vector(mode = "numeric", length = 100)
    set_weighted_transitivity_temp <- vector(mode = "numeric", length = 100)
    set_closeness_temp <- vector(mode = "numeric", length = 100)
    set_betweenness_temp <- vector(mode = "numeric", length = 100)
    #meso
    set_edge_density_temp <- vector(mode = "numeric", length = 100)
    set_modularity_temp <- vector(mode = "numeric", length = 100)
    set_conductance_temp <- vector(mode = "numeric", length = 100)

    set_pca1_temp <- vector(mode = "numeric", length = 100)
    set_pca2_temp <- vector(mode = "numeric", length = 100)
    
    set_cluster_temp <- vector(mode = "numeric", length = 100)
    set_correlations_temp <- vector(mode = "numeric", length = 100)
    set_sd_temp <- vector(mode = "numeric", length = 100)
    set_similarity_temp <- vector(mode = "numeric", length = 100)
    
    set_weighted_values_temp <- vector(mode = "numeric", length = 100)
    
    set_fruit_temp <- vector(mode = "numeric", length = 100)
    
    # similarity ratings
    subject_similarity_temp <- subject_temp %>%
      filter(screen_id == "similarity") %>%
      select(stimulus, response) %>% # think about RT
      mutate(
        stimulus = stringr::str_remove(stimulus, pattern = "../../img/grid_stimuli/grid_6_modularity_"),
        stimulus = as.numeric(stringr::str_remove(stimulus, pattern = ".jpg"))
      ) %>%
      unnest(response)
    # foo = 1
    for (foo in 1:100) {
      # select which stat to calculate
      
      # this section calculates each of the subgraph stats based on the induced
      # subgraph rather than the larger network. To get node importance
      # measures at the level of the entire graph, this code would need to change
      
      # pull out a candidate sub graph
      size <- net_degree[net_degree$Name %in% res[[foo]], ]$Item
      subgraph <- igraph::induced_subgraph(g, size)
      # graph_stats <- NetworkStat(subgraph)
      # calculate centrality with respect to larger graph
      graph_stats <- net_degree[net_degree$Name %in% res[[foo]], ]
      adj_temp <- igraph::as_adjacency_matrix(subgraph, sparse = F, attr = "weight")
      
      if (2 %in% graph_stats[graph_stats$Name %in% res[[foo]], ]$snack_type){
        set_fruit_temp[[foo]] <-  sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$snack_type == 2)
      } else {set_fruit_temp[[foo]] <- 0}

      set_degree_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$degree)
      set_strength_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$strength)
      # set_weighted_transitivity_temp[[foo]] <- NetworkToolbox::clustcoeff(adj_temp, weighted = T)$CC
      # ifelse(is.nan(set_weighted_transitivity_temp[[foo]]), set_weighted_transitivity_temp[[foo]] <- 0, set_weighted_transitivity_temp[[foo]] <- set_weighted_transitivity_temp[[foo]])
      set_weighted_transitivity_temp[[foo]] <-sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$weighted_transitivity)
      
      set_eigen_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$eigen)
      set_closeness_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$closeness)
      set_betweenness_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$betweenness)
        
      set_edge_density_temp[[foo]] <- as.numeric(edge_density(subgraph))
      set_modularity_temp[[foo]] <- as.numeric(modularity(subgraph, V(subgraph)$snack_type))
        # for testing the permutation method
        # if (res_sig[[foo]] == 0){
        #   set_network_temp[[foo]] <- 0
        #   }
      mem[names(mem)] <- 1
      mem[names(mem) %in% V(subgraphs[[foo]])$name] <- 2
      conductance_temp <- clustAnalytics::conductance(g, mem)[2]
      set_conductance_temp[[foo]] <- as.numeric(conductance_temp)
      
      set_pca1_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$PCA1)
      set_pca2_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$PCA2)
      # set_pca3_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$PCA3)
      
      # calculated the weighted value of the set
      x <- do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response)
      
      if (weight == "degree") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$degree
      } else if (weight == "strength") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$strength
      } else if (weight == "weighted_transitivity") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$weighted_transitivity
      } else if (weight == "eigen") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$eigen
      } else if (weight == "closeness") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$closeness
      } else if (weight == "betweenness") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$betweenness
      } else if (weight == "PCA1") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$PCA1
      } else if (weight == "PCA2") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$PCA2
      }
      # wt <- (wt - min(wt)) / diff(range(wt)) #normalize (corrected the issue)
      
      # use weight to get weighted average
      # set_weighted_values_temp[[foo]] <- weighted.mean(x, wt)
      set_weighted_values_temp[[foo]] <- NA
      
      
      set_values_temp[[foo]] <- sum(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
      # set_values_temp[[foo]] <- mean(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
      
      set_values_MAX_temp[[foo]] <- max(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
      set_values_MIN_temp[[foo]] <- min(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))

      #
      set_correlations_temp[[foo]] <- sum(apply(cor_snack_food[colnames(cor_snack_food) %in% res[[foo]], ], 2, mean, na.rm = T)[res[[foo]]])
      set_sd_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$sds)
      
      if (experiment == 1) {
        next
      }
      set_similarity_temp[[foo]] <- subject_similarity_temp$response[[foo]]
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
    xxxx$left_wtrating <- NULL
    xxxx$right_wtrating <- NULL
    
    xxxx$left_net_degree <- NULL
    xxxx$right_net_degree <- NULL
    xxxx$left_net_strength <- NULL
    xxxx$right_net_strength  <- NULL
    xxxx$left_net_eigen <- NULL
    xxxx$right_net_eigen <- NULL
    xxxx$left_net_betweenness <- NULL
    xxxx$right_net_betweenness <- NULL
    xxxx$left_net_closeness <- NULL
    xxxx$right_net_closeness <- NULL
    xxxx$left_net_weighted_transitivity <- NULL
    xxxx$right_net_weighted_transitivity <- NULL
    xxxx$left_net_edge_density <- NULL
    xxxx$right_net_edge_density <- NULL
    xxxx$left_net_modularity <- NULL
    xxxx$right_net_modularity <- NULL
    xxxx$left_net_conductance <- NULL
    xxxx$right_net_conductance <- NULL
    
    xxxx$left_net_pca1 <- NULL
    xxxx$right_net_pca1 <- NULL
    xxxx$left_net_pca2 <- NULL
    xxxx$right_net_pca2 <- NULL
    
    xxxx$left_sim <- NULL
    xxxx$right_sim <- NULL
    xxxx$left_correlation <- NULL
    xxxx$right_correlation <- NULL
    xxxx$left_sd <- NULL
    xxxx$right_sd <- NULL
    xxxx$left_MAX <- NULL
    xxxx$right_MAX <- NULL
    xxxx$left_MIN <- NULL
    xxxx$right_MIN <- NULL
    
    xxxx$left_fruit <- NULL
    xxxx$right_fruit <- NULL
    
    for (foo in seq_len(nrow(xxxx))) {
      xxxx$left_rating[[foo]] <- as.numeric(set_values_temp[xxxx$left[[foo]]])
      xxxx$right_rating[[foo]] <- as.numeric(set_values_temp[xxxx$right[[foo]]])
      xxxx$left_wtrating[[foo]] <- as.numeric(set_weighted_values_temp[xxxx$left[[foo]]])
      xxxx$right_wtrating[[foo]] <- as.numeric(set_weighted_values_temp[xxxx$right[[foo]]])
      
      xxxx$left_net_degree[[foo]] <- as.numeric(set_degree_temp[xxxx$left[[foo]]])
      xxxx$right_net_degree[[foo]] <- as.numeric(set_degree_temp[xxxx$right[[foo]]])
      xxxx$left_net_strength[[foo]] <- as.numeric(set_strength_temp[xxxx$left[[foo]]])
      xxxx$right_net_strength[[foo]]  <- as.numeric(set_strength_temp[xxxx$right[[foo]]])
      xxxx$left_net_eigen[[foo]] <- as.numeric(set_eigen_temp[xxxx$left[[foo]]])
      xxxx$right_net_eigen[[foo]] <- as.numeric(set_eigen_temp[xxxx$right[[foo]]])
      xxxx$left_net_betweenness[[foo]] <- as.numeric(set_betweenness_temp[xxxx$left[[foo]]])
      xxxx$right_net_betweenness[[foo]] <- as.numeric(set_betweenness_temp[xxxx$right[[foo]]])
      xxxx$left_net_closeness[[foo]] <- as.numeric(set_closeness_temp[xxxx$left[[foo]]])
      xxxx$right_net_closeness[[foo]] <- as.numeric(set_closeness_temp[xxxx$right[[foo]]])
      xxxx$left_net_weighted_transitivity[[foo]] <- as.numeric(set_weighted_transitivity_temp[xxxx$left[[foo]]])
      xxxx$right_net_weighted_transitivity[[foo]] <- as.numeric(set_weighted_transitivity_temp[xxxx$right[[foo]]])
      xxxx$left_net_edge_density[[foo]] <- as.numeric(set_edge_density_temp[xxxx$left[[foo]]])
      xxxx$right_net_edge_density[[foo]] <- as.numeric(set_edge_density_temp[xxxx$right[[foo]]])
      xxxx$left_net_modularity[[foo]] <- as.numeric(set_modularity_temp[xxxx$left[[foo]]])
      xxxx$right_net_modularity[[foo]] <- as.numeric(set_modularity_temp[xxxx$right[[foo]]])
      xxxx$left_net_conductance[[foo]] <- as.numeric(set_conductance_temp[xxxx$left[[foo]]])
      xxxx$right_net_conductance[[foo]] <- as.numeric(set_conductance_temp[xxxx$right[[foo]]])
      
      xxxx$left_net_pca1[[foo]] <- as.numeric(set_pca1_temp[xxxx$left[[foo]]])
      xxxx$right_net_pca1[[foo]] <- as.numeric(set_pca1_temp[xxxx$right[[foo]]])
      xxxx$left_net_pca2[[foo]] <- as.numeric(set_pca2_temp[xxxx$left[[foo]]])
      xxxx$right_net_pca2[[foo]] <- as.numeric(set_pca2_temp[xxxx$right[[foo]]])
      
      xxxx$left_sim[[foo]] <- as.numeric(set_similarity_temp[xxxx$left[[foo]]])
      xxxx$right_sim[[foo]] <- as.numeric(set_similarity_temp[xxxx$right[[foo]]])
      xxxx$left_correlation[[foo]] <- as.numeric(set_correlations_temp[xxxx$left[[foo]]])
      xxxx$right_correlation[[foo]] <- as.numeric(set_correlations_temp[xxxx$right[[foo]]])
      xxxx$left_sd[[foo]] <- as.numeric(set_sd_temp[xxxx$left[[foo]]])
      xxxx$right_sd[[foo]] <- as.numeric(set_sd_temp[xxxx$right[[foo]]])
      xxxx$left_MAX[[foo]] <- as.numeric(set_values_MAX_temp[xxxx$left[[foo]]])
      xxxx$right_MAX[[foo]] <- as.numeric(set_values_MAX_temp[xxxx$right[[foo]]])
      xxxx$left_MIN[[foo]] <- as.numeric(set_values_MIN_temp[xxxx$left[[foo]]])
      xxxx$right_MIN[[foo]] <- as.numeric(set_values_MIN_temp[xxxx$right[[foo]]])
      
      xxxx$left_fruit[[foo]] <-  as.numeric(set_fruit_temp[xxxx$left[[foo]]])
      xxxx$right_fruit[[foo]] <-  as.numeric(set_fruit_temp[xxxx$right[[foo]]])
    }
    # xxxx$value_network_corr <- cor(set_values_temp, set_network_temp)
    # xxxx$value_network_corr_p <- cor.test(set_values_temp, set_network_temp)$p.value
    
    subject_df[[pp]] <- xxxx
  }
  
  df <- as.data.frame(do.call(rbind, subject_df)) %>%
    unnest(cols = c(
      left_rating, right_rating, left_wtrating, right_wtrating, 
      left_net_degree, right_net_degree, 
      left_net_strength, right_net_strength,
      left_net_eigen, right_net_eigen,
      left_net_betweenness, right_net_betweenness,
      left_net_closeness, right_net_closeness,
      left_net_weighted_transitivity,right_net_weighted_transitivity,
      left_net_edge_density,right_net_edge_density,
      left_net_modularity,right_net_modularity,
      left_net_conductance,right_net_conductance,
      left_net_pca1,right_net_pca1,
      left_net_pca2,right_net_pca2,
      left_sim, right_sim,
      left_correlation, right_correlation, left_sd, right_sd,
      left_MAX, right_MAX, left_MIN, right_MIN,
      left_fruit, right_fruit
    ))
  # add the correct response col and choose max and not choose min col
  df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))
  
  df$correctwt <- as.numeric((df$left_wtrating > df$right_wtrating & df$choice == 1) | (df$left_wtrating < df$right_wtrating & df$choice == 0))
  
  df$choose_max <- factor(as.numeric((df$left_MAX > df$right_MAX & df$choice == 1) | (df$left_MAX < df$right_MAX & df$choice == 0)))
  df$choose_min <- factor(as.numeric((df$left_MIN < df$right_MIN & df$choice == 0) | (df$left_MIN > df$right_MIN & df$choice == 1)))
  
  return(df)
}

