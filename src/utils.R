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

load_food_names <- function() {
  # load all the images to calculate the value for a group of foods
  food_folder <- here::here("data", "snackitemnames_nicholas", "Lee_Holyoak_2021_images")
  FoodNames <- readxl::read_excel(here::here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))
  temp <- list.files(path = food_folder, pattern = "*.jpg", full.names = T)
  foods_in_image <- stringr::str_extract(temp, "item\\d+")
  foods_in_image <- stringr::str_extract(foods_in_image, "\\d+")
  # get row idx for each of the image numbers
  foods_in_image <- tibble::rowid_to_column(data.frame(Image = as.numeric(foods_in_image)))
  foods_in_image <- dplyr::left_join(FoodNames, foods_in_image, "Image")
  return(list(FoodNames = FoodNames, foods_in_image = foods_in_image))
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

calculate_net_stats <- function(g) {
  ## another version of calculating the netstats of various measures for a graph
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

# network_stats <- c(
#   "assortment", "edge_density", "weighted_clustering_coefficient",
#   "LowHighWithinBetween", "modularity"
# )

organize_group_data <- function(experiment, net_stat = "modularity") {

  # which data set are we working with?
  if (experiment == 1) {
    temp_files <- list.files(path = here::here("data", "pilot_30"), pattern = ".json", full.names = T)
    network_stats <- "LowHighWithinBetween"
    # load subgraphs
    load(file = here::here("data", "LowHighWithinBetween.RData"))
  } else if (experiment == 2) {
    temp_files <- list.files(path = here::here("data", "exp_2"), pattern = ".json", full.names = T)
    network_stats <- "modularity"
    # load subgraphs
    load(file = here::here("data", "modularity_100_6.RData"))
  }

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
    set_cluster_temp <- vector(mode = "numeric", length = 100)
    set_correlations_temp <- vector(mode = "numeric", length = 100)
    set_sd_temp <- vector(mode = "numeric", length = 100)
    set_similarity_temp <- vector(mode = "numeric", length = 100)

    # similarity ratings
    subject_similarity_temp <- subject_temp %>%
      filter(screen_id == "similarity") %>%
      select(stimulus, response) %>% # think about RT
      mutate(
        stimulus = stringr::str_remove(stimulus, pattern = "../../img/grid_stimuli/grid_6_modularity_"),
        stimulus = as.numeric(stringr::str_remove(stimulus, pattern = ".jpg"))
      ) %>%
      unnest(response)

    # TODO try also the sum SD of the ratings

    for (foo in 1:100) {
      # select which stat to calculate

      # this section calculates each of the subgraph stats based on the induced
      # subgraph rather than the larger network. To get node importance
      # measures at the level of the entire graph, this code would need to change

      # pull out a candidate sub graph
      size <- net_degree[net_degree$Name %in% res[[foo]], ]$Item
      subgraph <- igraph::induced_subgraph(g, size)
      graph_stats <- NetworkStat(subgraph)

      if (net_stat == "degree") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$degree)
      } else if (net_stat == "strength") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$strength)
      } else if (net_stat == "weighted_transitivity") {
        set_network_temp[[foo]] <- transitivity(subgraph, "global")
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
        adj_temp <- igraph::as_adjacency_matrix(subgraph, sparse = F, attr = "weight")
        assort_temp <- assortnet::assortment.discrete(adj_temp, V(subgraph)$snack_type, weighted = TRUE, SE = F)$r
        set_network_temp[[foo]] <- assort_temp
      } else if (net_stat == "edge_density") {
        set_network_temp[[foo]] <- as.numeric(edge_density(subgraph))
      } else if (net_stat == "diversity") {
        adj_temp <- igraph::as_adjacency_matrix(subgraph, sparse = F, attr = "weight")
        diverse_temp <- NetworkToolbox::diversity(adj_temp, V(subgraph)$snack_type)$overall
        diverse_temp[!is.finite(diverse_temp)] <- NA
        set_network_temp[[foo]] <- sum(diverse_temp, na.rm = T)
      } else if (net_stat == "efficiency") {
        E(subgraph)$weight <- 2**((E(subgraph)$weight - min(E(subgraph)$weight)) / diff(range(E(subgraph)$weight)))
        leverage_temp <- as.numeric(igraph::global_efficiency(subgraph, directed = F))
        set_network_temp[[foo]] <- as.numeric(leverage_temp)
      } else if (net_stat == "modularity") {
        # pull out a candidate sub graph
        set_network_temp[[foo]] <- as.numeric(modularity(subgraph, V(subgraph)$snack_type))
      }

      set_values_temp[[foo]] <- sum(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))

      set_values_MAX_temp[[foo]] <- max(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
      set_values_MIN_temp[[foo]] <- min(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))


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
    xxxx$left_net <- NULL
    xxxx$right_net <- NULL
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

    # foo <- 1
    for (foo in seq_len(nrow(xxxx))) {
      xxxx$left_rating[[foo]] <- as.numeric(set_values_temp[xxxx$left[[foo]]])
      xxxx$right_rating[[foo]] <- as.numeric(set_values_temp[xxxx$right[[foo]]])
      xxxx$left_net[[foo]] <- as.numeric(set_network_temp[xxxx$left[[foo]]])
      xxxx$right_net[[foo]] <- as.numeric(set_network_temp[xxxx$right[[foo]]])
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
    }
    xxxx$value_network_corr <- cor(set_values_temp, set_network_temp)
    xxxx$value_network_corr_p <- cor.test(set_values_temp, set_network_temp)$p.value

    subject_df[[pp]] <- xxxx
  }

  df <- as.data.frame(do.call(rbind, subject_df)) %>%
    unnest(cols = c(
      left_rating, right_rating, left_net, right_net, left_sim, right_sim,
      left_correlation, right_correlation, left_sd, right_sd,
      left_MAX, right_MAX, left_MIN, right_MIN
    ))
  # add the correct response col
  df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))

  return(df)
}

create_dataset <- function(df, type, standardized = TRUE) {
  # creates the regressors for the analysis and can scale all the variables

  if (type == "choice") {
    model_dat <- df %>%
      exlusions() %>%
      group_by(subject_id) %>%
      mutate(
        nd = scale(left_net - right_net, center = standardized, scale = standardized),
        vd = scale(left_rating - right_rating, center = standardized, scale = standardized),
        sd = scale(left_sim - right_sim, center = standardized, scale = standardized),
        ov = scale(left_rating + right_rating, center = standardized, scale = standardized),
        on = scale(left_net + right_net, center = standardized, scale = standardized),
        os = scale(left_sim + right_sim, center = standardized, scale = standardized)
      ) %>%
      ungroup() %>%
      select(subject_id, choice, nd, vd, sd, ov, on, os)
  } else if (type == "correct/rt") {
    model_dat <- df %>%
      exlusions() %>%
      group_by(subject_id) %>%
      mutate( # take absolute value
        nd = scale(abs(left_net - right_net), center = standardized, scale = standardized),
        vd = scale(abs(left_rating - right_rating), center = standardized, scale = standardized),
        sd = scale(abs(left_sim - right_sim), center = standardized, scale = standardized),
        ov = scale(left_rating + right_rating, center = standardized, scale = standardized),
        on = scale(left_net + right_net, center = standardized, scale = standardized),
        os = scale(left_sim + right_sim, center = standardized, scale = standardized)
      ) %>%
      select(subject_id, correct, rt, vd, nd, sd, ov, on, os)
  }
  return(model_dat)
}

estimate_brms <- function(df, outcome = "choice") {
  # TODO add a saving feature to avoid redundant model estimation
  # note you could just write out the formula for each and not repeat so much here
  # but this seems okay so long as we aim to be explicit
  #
  # TODO just create a folder for each network statistic, then add an argument that places each model in what ever name you write
  #     create a error too. if the folder name does not exist in the directory then throw an error and don't run the models 'could not find folder to save models'
  if (outcome == "choice") {
    # base model
    model1 <- brm(choice ~ vd + ov + (vd + ov | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_choice01"))
    # add network difference
    model2A <- brm(choice ~ vd + ov + nd + (vd + ov + nd | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_choice02A"))
    model2B <- brm(choice ~ vd + ov + sd + (vd + ov + sd | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_choice02B"))
    # add similarity difference
    model3 <- brm(choice ~ vd + ov + nd + sd + (vd + ov + nd + sd | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_choice03"))
    # add overall network
    model4 <- brm(choice ~ vd + ov + nd + sd + on + (vd + ov + nd + sd + on | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_choice04"))
    # add overall similarity
    model5 <- brm(choice ~ vd + ov + nd + sd + on + os + (vd + ov + nd + sd + on + os | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_choice05"))
    # add interaction vd:nd (write out to be explicit)
    model6 <- brm(choice ~ vd + ov + nd + sd + on + os + vd:nd + (vd + ov + nd + sd + on + os + vd:nd | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_choice06"))
    # add interaction vd:sd
    model7 <- brm(choice ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + (vd + ov + nd + sd + on + os + vd:nd + vd:sd | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_choice07"))
    # add interaction ov:on
    model8 <- brm(choice ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_choice08"))
    # add interaction ov:os
    model9 <- brm(choice ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_choice09"))
  } else if (outcome == "correct") {
    # the coded as correct models (which take the absolute value for the regressors)
    model1 <- brm(correct ~ vd + ov + (vd + ov | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct01"))
    # add network difference
    model2A <- brm(correct ~ vd + ov + nd + (vd + ov + nd | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct02A"))
    model2B <- brm(correct ~ vd + ov + sd + (vd + ov + sd | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct02B"))
    # add similarity difference
    model3 <- brm(correct ~ vd + ov + nd + sd + (vd + ov + nd + sd | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct03"))
    # add overall network
    model4 <- brm(correct ~ vd + ov + nd + sd + on + (vd + ov + nd + sd + on | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct04"))
    # add overall similarity
    model5 <- brm(correct ~ vd + ov + nd + sd + on + os + (vd + ov + nd + sd + on + os | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct05"))
    # add interaction vd:nd
    model6 <- brm(correct ~ vd + ov + nd + sd + on + os + vd:nd + (vd + ov + nd + sd + on + os + vd:nd | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct06"))
    # add interaction vd:sd
    model7 <- brm(correct ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + (vd + ov + nd + sd + on + os + vd:nd + vd:sd | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct07"))
    # add interaction ov:on
    model8 <- brm(correct ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct08"))
    # add interaction ov:os
    model9 <- brm(correct ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct09"))
  } else if (outcome == "rt") {
    # the coded as response time models (which take the absolute value for the regressors)
    model1 <- brm(log(rt) ~ vd + ov + (vd + ov | subject_id), data = df, cores = 4, iter = 10000, file = here::here("fits", "fit_rt01"))
    # add network difference
    model2A <- brm(log(rt) ~ vd + ov + nd + (vd + ov + nd | subject_id), data = df, cores = 4, iter = 10000, file = here::here("fits", "fit_rt02A"))
    model2B <- brm(log(rt) ~ vd + ov + sd + (vd + ov + sd | subject_id), data = df, cores = 4, iter = 10000, file = here::here("fits", "fit_rt02B"))
    # add similarity difference
    model3 <- brm(log(rt) ~ vd + ov + nd + sd + (vd + ov + nd + sd | subject_id), data = df, cores = 4, iter = 10000, file = here::here("fits", "fit_rt03"))
    # add overall network
    model4 <- brm(log(rt) ~ vd + ov + nd + sd + on + (vd + ov + nd + sd + on | subject_id), data = df, cores = 4, iter = 10000, file = here::here("fits", "fit_rt04"))
    # add overall similarity
    model5 <- brm(log(rt) ~ vd + ov + nd + sd + on + os + (vd + ov + nd + sd + on + os | subject_id), data = df, cores = 4, iter = 10000, file = here::here("fits", "fit_rt05"))
    # add interaction vd:nd
    model6 <- brm(log(rt) ~ vd + ov + nd + sd + on + os + vd:nd + (vd + ov + nd + sd + on + os + vd:nd | subject_id), data = df, cores = 4, iter = 10000, file = here::here("fits", "fit_rt06"))
    # add interaction vd:sd
    model7 <- brm(log(rt) ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + (vd + ov + nd + sd + on + os + vd:nd + vd:sd | subject_id), data = df, cores = 4, iter = 10000, file = here::here("fits", "fit_rt07"))
    # add interaction ov:on
    model8 <- brm(log(rt) ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on | subject_id), data = df, cores = 4, iter = 10000, file = here::here("fits", "fit_rt08"))
    # add interaction ov:os
    model9 <- brm(log(rt) ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os | subject_id), data = df, cores = 4, iter = 10000, file = here::here("fits", "fit_rt09"))
  }
  # list(model1, model2, model3, model9)

  # create a list of the model outputs for further analysis
  list(model1, model2A, model2B, model3, model4, model5, model6, model7, model8, model9)
}

estimate_mlms <- function(df, outcome = "choice") {
  # estimate the mixed effect regressions using lme4 package
  if (outcome == "choice") {
    # base model
    model1 <- glmer(choice ~ vd + ov + (vd + ov | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add network difference
    model2 <- glmer(choice ~ vd + ov + nd + (vd + ov + nd | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add similarity difference
    model3 <- glmer(choice ~ vd + ov + nd + sd + (vd + ov + nd + sd | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add overall network
    model4 <- glmer(choice ~ vd + ov + nd + sd + on + (vd + ov + nd + sd + on | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add overall similarity
    model5 <- glmer(choice ~ vd + ov + nd + sd + on + os + (vd + ov + nd + sd + on + os | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction vd:nd (write out to be explicit)
    model6 <- glmer(choice ~ vd + ov + nd + sd + on + os + vd:nd + (vd + ov + nd + sd + on + os + vd:nd | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction vd:sd
    model7 <- glmer(choice ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + (vd + ov + nd + sd + on + os + vd:nd + vd:sd | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction ov:on
    model8 <- glmer(choice ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction ov:os
    model9 <- glmer(choice ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  } else if (outcome == "correct") {
    # base model
    model1 <- glmer(correct ~ vd + ov + (vd + ov | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add network difference
    model2 <- glmer(correct ~ vd + ov + nd + (vd + ov + nd | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add similarity difference
    model3 <- glmer(correct ~ vd + ov + nd + sd + (vd + ov + nd + sd | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add overall network
    model4 <- glmer(correct ~ vd + ov + nd + sd + on + (vd + ov + nd + sd + on | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add overall similarity
    model5 <- glmer(correct ~ vd + ov + nd + sd + on + os + (vd + ov + nd + sd + on + os | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction vd:nd (write out to be explicit)
    model6 <- glmer(correct ~ vd + ov + nd + sd + on + os + vd:nd + (vd + ov + nd + sd + on + os + vd:nd | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction vd:sd
    model7 <- glmer(correct ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + (vd + ov + nd + sd + on + os + vd:nd + vd:sd | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction ov:on
    model8 <- glmer(correct ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction ov:os
    model9 <- glmer(correct ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  } else if (outcome == "rt") {
    # base model
    model1 <- lmer(log(rt) ~ vd + ov + (vd + ov | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add network difference
    model2 <- lmer(log(rt) ~ vd + ov + nd + (vd + ov + nd | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add similarity difference
    model3 <- lmer(log(rt) ~ vd + ov + nd + sd + (vd + ov + nd + sd | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add overall network
    model4 <- lmer(log(rt) ~ vd + ov + nd + sd + on + (vd + ov + nd + sd + on | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add overall similarity
    model5 <- lmer(log(rt) ~ vd + ov + nd + sd + on + os + (vd + ov + nd + sd + on + os | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction vd:nd (write out to be explicit)
    model6 <- lmer(log(rt) ~ vd + ov + nd + sd + on + os + vd:nd + (vd + ov + nd + sd + on + os + vd:nd | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction vd:sd
    model7 <- lmer(log(rt) ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + (vd + ov + nd + sd + on + os + vd:nd + vd:sd | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction ov:on
    model8 <- lmer(log(rt) ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add interaction ov:os
    model9 <- lmer(log(rt) ~ vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os + (vd + ov + nd + sd + on + os + vd:nd + vd:sd + ov:on + ov:os | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  }
  # create a list of the model outputs for further analysis
  # list(model1, model2, model3, model9)

  list(model1, model2, model3, model4, model5, model6, model7, model8, model9)
}

generate_table <- function(ms, type, net_stat, save = F) {
  # function for generating the regression tables for experiment two
  # the names for the labels will NOT work for both exp one and two
  # TODO fix the labeling issue between the two
  # create proper file name with the filename
  file_name <- here::here("tables", paste0(type, "_", net_stat, ".html"))

  title <- paste0("mixed model for ", type)
  dv_labels <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9")
  pred_labels <- c(
    "Value Difference (vd)", "Overall Value (ov)",
    "Nework Difference (nd)", "Similarity Difference (sd)",
    "Overall Network (on)", "Overall Similarity (os)",
    "vd:nd", "vd:sd", "ov:on", "ov:os"
  )

  if (save == TRUE) {
    table_temp <- tab_model(ms,
      show.intercept = F,
      show.aic = T,
      show.re.var = F,
      show.ci = FALSE,
      show.r2 = FALSE,
      show.icc = FALSE,
      dv.labels = dv_labels,
      pred.labels = pred_labels,
      title = title,
      file = file_name
    )
  } else {
    table_temp <- tab_model(ms,
      show.intercept = F,
      show.aic = T,
      show.re.var = F,
      show.ci = FALSE,
      show.r2 = FALSE,
      show.icc = FALSE,
      dv.labels = dv_labels,
      pred.labels = pred_labels,
      title = title
    )
  }
  print("did you fix the labeling issue?")
  return(table_temp)
}
