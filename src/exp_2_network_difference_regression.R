# exp2_network_difference_regression_analysis.R - analysis of experiment two
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
# 2022/12/22      Kianté  Fernandez                   coded up version one

# Libraries
library(purrr) # Functional Programming Tools
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(jsonlite) # A Simple and Robust JSON Parser and Generator for R

library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4
library(lmerTest) # Tests in Linear Mixed Effects Models

library(gghalves) # Compose Half-Half Plots Using Your Favorite Geoms
library(ggforce) # Accelerating 'ggplot2'
library(ggdist) # Visualizations of Distributions and Uncertainty
library(patchwork) # The Composer of Plots

library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(brms)

source(here::here("src", "utils.R"))

##### loading the data#####
temp_files <- list.files(path = here::here("data", "exp_2"), pattern = ".json", full.names = T)
#load subgraphs
load(file = here::here("data", "modularity_100_6.RData"))

load_food_names()

# get correlations between items
lee_2021_rating1 <- read_csv(here::here("data", "lee_2021_rating1.csv"), col_names = FALSE)
cor.snack_food <- SemNeT::similarity(lee_2021_rating1, method = "cor")
cor_snack_food <- data.frame(matrix(cor.snack_food[cor.snack_food != 1], 59, 60))
names(cor_snack_food) <- load_food_names()$FoodNames$Name


######
# calculate a bunch of network measures to look at relationship to stuff
source("exploratory_graph_analysis.R")

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

file_idx <- 75
net_stat <- "modularity"
organize_group_data <- function(file_idx = 75, net_stat) {
  ############################
  ## organize the data and calculate value of group of items and net stats for each subject
  ##
  # pp = 1
  subject_df <- vector(mode = "list", length = file_idx)
  for (pp in seq_len(file_idx)) {
    # load the  subjects data
    subject_temp <- parse_json(read_json(temp_files[[pp]]), simplifyVector = T)
    
    # this gets the ratings in check
    subject_rating_temp <- subject_temp %>%
      filter(screen_id == "ratings") %>%
      select(stimulus, response) %>%
      mutate(
        Image = stringr::str_remove(stimulus, pattern = "../../img/60Foods/item"),
        Image = as.numeric(stringr::str_remove(Image, pattern = ".jpg"))
      ) %>%
      dplyr::left_join(foods_in_image, by = "Image")
      
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
      
      # pull out a candidate sub graph
      size <- net_degree[net_degree$Name %in% res[[foo]], ]$Item
      subgraph <- igraph::induced_subgraph(g, size)
      graph_stats <- NetworkStat(subgraph)
      
      if (net_stat == "degree") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$degree)
        
        # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$degree)
      } else if (net_stat == "strength") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$strength)
        
        # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$strength)
      } else if (net_stat == "weighted_transitivity") {
        # pull out a candidate sub graph
        size <- net_degree[net_degree$Name %in% res[[foo]], ]$Item
        gt <- igraph::induced_subgraph(g, size)
        # calculate stat
        clust_temp <- transitivity(gt, "global")
        set_network_temp[[foo]] <- clust_temp
        
        # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$weighted_transitivity)
      } else if (net_stat == "eigen") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$eigen)
        # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$eigen)
      } else if (net_stat == "closeness") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$closeness)
        # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$closeness)
      } else if (net_stat == "betweenness") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$betweenness)
        # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$betweenness)
      } else if (net_stat == "page_rank") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$page_rank)
        # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$page_rank)
      } else if (net_stat == "participation") {
        set_network_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$participation)
        # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$participation)
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
        # pull out a candidate sub graph
        # size <- net_degree[net_degree$Name %in% res[[foo]], ]$Item
        # gt <- igraph::induced_subgraph(g, size)
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
        # igraph::global_efficiency(subgraph,directed = F)
        leverage_temp <- as.numeric(igraph::global_efficiency(subgraph, directed = F))
        set_network_temp[[foo]] <- as.numeric(leverage_temp)
      } else if (net_stat == "modularity") {
        # pull out a candidate sub graph
        # size <- net_degree[net_degree$Name %in% res[[foo]], ]$Item
        # gt <- igraph::induced_subgraph(g, size)
        # adj_temp <- igraph::as_adjacency_matrix(subgraph, sparse = F, attr = "weight")
        # calculate stat
        # impact_temp <- networktools::bridge(gt,  V(gt)$snack_type)$Strength
        impact_temp <- as.numeric(modularity(subgraph, V(subgraph)$snack_type))
        
        # cohesion(g)
        # impact_temp[!is.finite(impact_temp)] <- NA
        
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
  return(df)
}

#check for exclusions
for (subject_idx in 1:75){
  
  temp_df <- df %>% 
    filter(subject_id == subject_idx) %>% 
    mutate(Q1 = quantile(rt, .25),
           Q3 = quantile(rt, .75),
           IQR = IQR(rt)) %>%
    filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
    filter(!rt <= 300) %>% # response times cutoffs
    filter(!rt >= 9000) %>% 
    summarise(pct_excluded = (100 - n())/100)
  if (temp_df$pct_excluded > 0.2){
    print(paste0("######## subject: ", subject_idx, " #######"))
    # print(paste0("######## percent trials excluded: ", temp_df$pct_excluded, " #######"))
  }
}

##value difference exclusion
p_values <- vector(mode = "numeric", length = 75)
for (subject_idx in 1:75){

  if(subject_idx %in% c(17,29,35,37,38,56,65,68)){
    p_values[[subject_idx]] <- NA
    next}  
  df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))
  
  temp_df <- df %>% 
    filter(subject_id == subject_idx) %>% 
    mutate(Q1 = quantile(rt, .25),
           Q3 = quantile(rt, .75),
           IQR = IQR(rt)) %>%
    filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
    filter(!rt <= 300) %>% # response times cutoffs
    filter(!rt >= 9000) %>% 
    mutate(
      vd = scale(abs(left_rating - right_rating)))
  
  # print(paste0("######## subject: ", subject_idx, " #######"))
  # print(summary(glm(correct ~ vd*nd, family = binomial, data = temp_df)))
  # print(broom::tidy(glm(correct ~ vd*nd, family = binomial, data = temp_df)))
  # temp_res <- broom::tidy(glm(choice ~ vd, family = binomial, data = temp_df))
  temp_res <- broom::tidy(glm(correct ~ vd, family = binomial, data = temp_df))
  
  # temp_res <- broom::tidy(lm(rt ~ eq, data = temp_df))
  temp_res$p.value <-  round(temp_res$p.value, 4)
  if (temp_res[2,5][[1]] > 0.05){ #check p-value
    # print(paste0("######## subject: ", unique(temp_df$subject_id), " #######"))
    # print(unique(temp_df$subject_id))
    # print(paste0("######## percent trials excluded: ", temp_df$pct_excluded, " #######"))
    p_values[[subject_idx]] <- unique(temp_df$subject_id)

  }else (  p_values[[subject_idx]] <- NA)
}
#so far we have a 40%! exclusion rate...
# as.numeric(na.omit(p_values))

exlusions <- function(df) {
  # function for data exclusions
  temp <- df %>%
     filter(subject_id != 17) %>%
     filter(subject_id != 29) %>%
     filter(subject_id != 35) %>%
     filter(subject_id != 37) %>%
     filter(subject_id != 38) %>%
     filter(subject_id != 56) %>%
     filter(subject_id != 65) %>%
     filter(subject_id != 68) %>% #rt exclusions
    filter(!subject_id %in%  as.numeric(na.omit(p_values))) %>%
    group_by(subject_id) %>% # response times
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    ungroup() %>%
    filter(!rt <= 300) %>% # response times cutoffs
    filter(!rt >= 9000)
  return(temp)
}


net_stats <- c("strength", "eigen", "edge_density", "modularity")

plts <- vector("list", length = length(net_stats))

net_idx <- 4
for (net_idx in 1:length(net_stats)) {
  print(paste0("############### ", net_stats[[net_idx]], " ###############"))
  df <- organize_group_data(net_stat = net_stats[[net_idx]])
  
  # df <- df[is.nan(df$left_net) == F & is.nan(df$right_net) == F,]
  
  # print(unique(df$left_net))
  
  ### code it as correct incorrect instead
  df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))
  
  
  # df %>%
  #   select(subject_id,value_network_corr, value_network_corr_p) %>%
  #   distinct() %>%
  #   filter(value_network_corr_p > .05)
  
  # robustness check for correlation between net stat and value
  # df <- df %>% filter(value_network_corr_p > .05)
  
  ## make a plot of the vd:nd interaction
  # plt <- df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>%
  #   mutate(
  #     vd = left_rating - right_rating,
  #     nd = left_net - right_net
  #   ) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd, 7)) - 4,
  #   ) %>%
  #   group_by(subject_id, binned_value_diff) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd, 3)) - 2
  #   ) %>%
  #   group_by(binned_net_diff, binned_value_diff) %>%
  #   mutate(
  #     n = n(),
  #     m_left = mean(choice),
  #     se = sqrt(var(choice) / length(choice))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_value_diff, y = m_left, color = factor(binned_net_diff))) +
  #   geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   geom_hline(yintercept = .5, linetype = "dashed") +
  #   scale_color_brewer(palette = "Set1") +
  #   scale_y_continuous(limits = c(0, 1.01)) +
  #   labs(
  #     title = paste0(net_stats[[net_idx]]),
  #     y = "Probability of Choosing Left",
  #     x = "Value Difference (L-R) bins",
  #     color = "Network Difference (L-R) bins"
  #   ) +
  #   theme(legend.position = "none")
  # #
  # print(plt)
  # plts[[net_idx]] <- plt
  # 
  # df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>%
  #   mutate(
  #     vd = abs(left_rating - right_rating),
  #     nd = abs(left_net - right_net)
  #   ) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd, 6)) - 1,
  #   ) %>%
  #   group_by(subject_id, binned_value_diff) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd, 2)) - 1
  #   ) %>%
  #   group_by(binned_net_diff, binned_value_diff) %>%
  #   mutate(
  #     n = n(),
  #     m_rt = mean(rt),
  #     se = sqrt(var(rt) / length(rt))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(binned_net_diff))) +
  #   geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   scale_color_brewer(palette = "Set1") +
  #   labs(
  #     y = "RT(ms)",
  #     x = "Abs Value Difference (L-R)",
  #     color = "Abs Network Difference (L-R)"
  #   )
  # 
  # df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>%
  #   mutate(
  #     vd = left_rating + right_rating,
  #     nd = left_net + right_net
  #   ) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd, 6)) - 1,
  #   ) %>%
  #   group_by(subject_id, binned_value_diff) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd, 2)) - 1
  #   ) %>%
  #   group_by(binned_net_diff, binned_value_diff) %>%
  #   mutate(
  #     n = n(),
  #     m_rt = mean(rt),
  #     se = sqrt(var(rt) / length(rt))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(binned_net_diff))) +
  #   geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   scale_color_brewer(palette = "Set1") +
  #   labs(
  #     y = "RT(ms)",
  #     x = "Value Magnitude (L+R)",
  #     color = "Network Magnitude (L+R)"
  #   )
  
  # next
  # correct absolute value plot
  # print(df %>%
  #         exlusions() %>%
  #         group_by(subject_id) %>%
  #         mutate(
  #           vd = abs(left_rating - right_rating),
  #           nd = abs(left_net - right_net)
  #         ) %>%
  #         mutate(
  #           binned_value_diff = as.numeric(cut_number(vd, 4)) - 1,
  #         ) %>%
  #         group_by(binned_value_diff) %>%
  #         mutate(
  #           binned_net_diff = as.numeric(cut_number(nd, 6)) - 1
  #         ) %>%
  #         group_by(binned_net_diff, binned_value_diff) %>%
  #         mutate(
  #           n = n(),
  #           m_correct = mean(correct),
  #           se = sqrt(var(correct) / length(correct))
  #         ) %>%
  #         ungroup() %>%
  #         ggplot(aes(x = binned_net_diff, y = m_correct, color = factor(binned_value_diff))) +
  #         geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
  #         theme_classic() +
  #         geom_line(size = 1) +
  #         geom_hline(yintercept = .5, linetype = "dashed") +
  #         scale_color_brewer(palette = "Set1") +
  #         scale_y_continuous(limits = c(0, 1.01)) +
  #         labs(
  #           title = paste0(net_stats[[net_idx]]),
  #           y = "Accuracy",
  #           x = "Absolute Network Difference (L-R)",
  #           color = "Absolute Value Difference (L-R)"
  #         ) +
  #         theme(legend.position = "top"))
  #

  # print(df %>%
  #         exlusions() %>%
  #         group_by(subject_id) %>%
  #         # mutate(eq = left_cluster_condition == right_cluster_condition) %>%
  #         # filter(eq == 0) %>%
  #         mutate(nd = abs(left_net - right_net)) %>%
  #         mutate(
  #           binned_net_diff = as.numeric(cut_number(nd, 10)) - 1
  #         ) %>%
  #         group_by(binned_net_diff) %>%
  #         mutate(
  #           n = n(),
  #           m_correct = mean(correct),
  #           se = sqrt(var(correct) / length(correct))
  #         ) %>%
  #         ungroup() %>%
  #         ggplot(aes(x = binned_net_diff, y = m_correct)) +
  #         geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
  #         theme_classic() +
  #         geom_line(size = 1) +
  #         geom_hline(yintercept = .5, linetype = "dashed") +
  #         scale_color_brewer(palette = "Set1") +
  #         scale_y_continuous(limits = c(0, 1.01)) +
  #         labs(
  #           title = paste0(net_stats[[net_idx]]),
  #           y = "Accuracy",
  #           x = "Absolute Network Difference (L-R)"
  #         ) +
  #         theme(legend.position = "top"))

  # ##make a plot of the vd:nd interaction
  # plt <- df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>%
  #   mutate(vd = left_rating - right_rating,
  #          nd = left_net - right_net) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd,5)) - 3,
  #   ) %>%
  #   group_by(subject_id,binned_value_diff) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,5 )) - 3
  #   ) %>%
  #   group_by(binned_net_diff,binned_value_diff) %>%
  #   mutate(n = n(),
  #          m_left = mean(choice),
  #          se = sqrt(var(choice) / length(choice))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_value_diff, y = m_left, color = factor(binned_net_diff))) +
  #   geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   geom_hline(yintercept = .5, linetype = "dashed") +
  #   scale_color_brewer(palette = "Set1") +
  #   scale_y_continuous(limits = c(0, 1.01)) +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #     y = "Probability of Choosing Left",
  #     x = "Value Difference (L-R) bins",
  #     color = "Network Difference (L-R) bins"
  #   )
  # print(plt)
  # df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>%
  #   mutate(vd = abs(left_rating - right_rating),
  #          nd = abs(left_net - right_net)) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,3)) - 1
  #       ) %>%
  #   group_by(subject_id,binned_net_diff) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd,5)) - 1,
  #   ) %>%
  #   group_by(binned_net_diff,binned_value_diff) %>%
  #   mutate(n = n(),
  #          m_c = mean(correct),
  #          se = sqrt(var(correct) / length(correct))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_value_diff, y = m_c, color = factor(binned_net_diff))) +
  #   geom_pointrange(aes(ymin = m_c - se, ymax = m_c + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   geom_hline(yintercept = .5, linetype = "dashed") +
  #   scale_color_brewer(palette = "Set1") +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #        y = "Accuracy",
  #        x = "Absolute Value Difference (L-R)",
  #        color = "Absolute Network Difference (L-R)"
  #   )
  # df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>%
  #   mutate(vd = abs(left_rating - right_rating),
  #          nd = abs(left_net - right_net)) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd,5)) - 1,
  #   ) %>%
  #   group_by(subject_id,binned_value_diff) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,3)) - 1
  #   ) %>%
  #   group_by(binned_net_diff,binned_value_diff) %>%
  #   mutate(n = n(),
  #          m_rt = mean(rt),
  #          se = sqrt(var(rt) / length(rt))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(binned_net_diff))) +
  #   geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   scale_color_brewer(palette = "Set1") +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #     y = "RT(ms)",
  #     x = "Absolute Value Difference (L-R)",
  #     color = "Absolute Network Difference (L-R)"
  #   )
  # df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>%
  #   mutate(vd = left_rating + right_rating,
  #          nd = left_net + right_net) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd,5)) - 1,
  #   ) %>%
  #   group_by(subject_id,binned_value_diff) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,3)) - 1
  #   ) %>%
  #   group_by(binned_net_diff,binned_value_diff) %>%
  #   mutate(n = n(),
  #          m_rt = mean(rt),
  #          se = sqrt(var(rt) / length(rt))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(binned_net_diff))) +
  #   geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   scale_color_brewer(palette = "Set1") +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #        y = "RT(ms)",
  #        x = "Absolute Value Magnitude (L+R)",
  #        color = "Absolute Network Magnitude (L+R)"
  #   )
  #
  # print(plt)
  #
  # plts[[net_idx]] <- plt

  # #correct absolute value plot
  # print(df %>%
  #   exlusions() %>%
  #   # mutate(eq = left_cluster_condition == right_cluster_condition) %>%
  #   # filter(eq == 1) %>%
  #   mutate(vd = abs(left_rating - right_rating),
  #          nd = abs(left_net - right_net)) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd,4)) - 1,
  #   ) %>%
  #   group_by(binned_value_diff) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,4)) - 1
  #   ) %>%
  #   group_by(binned_net_diff,binned_value_diff) %>%
  #   mutate(n = n(),
  #          m_correct = mean(correct),
  #          se = sqrt(var(correct) / length(correct))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_net_diff, y = m_correct, color = factor(binned_value_diff))) +
  #   geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   geom_hline(yintercept = .5, linetype = "dashed") +
  #   scale_color_brewer(palette = "Set1") +
  #   scale_y_continuous(limits = c(0, 1.01)) +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #        y = "Accuracy",
  #        x = "Absolute Network Difference (L-R)",
  #        color = "Absolute Value Difference (L-R)"
  #   ) +  theme(legend.position="top"))
  # #
  # #
  # print(df %>%
  #   exlusions() %>%
  #   # mutate(eq = left_cluster_condition == right_cluster_condition) %>%
  #   # filter(eq == 0) %>%
  #   mutate(nd = abs(left_net - right_net)) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,4)) - 1
  #   ) %>%
  #   group_by(binned_net_diff) %>%
  #   mutate(n = n(),
  #          m_correct = mean(correct),
  #          se = sqrt(var(correct) / length(correct))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_net_diff, y = m_correct)) +
  #   geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   geom_hline(yintercept = .5, linetype = "dashed") +
  #   scale_color_brewer(palette = "Set1") +
  #   scale_y_continuous(limits = c(0, 1.01)) +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #        y = "Accuracy",
  #        x = "Absolute Network Difference (L-R)"
  #   ) +  theme(legend.position="top"))
  
  #### data analysis
  
  # find the trials with condition one involved
  # df$degree_condition <- df$left_cluster_condition %in% c(1) | df$right_cluster_condition %in% c(1)
  # df <- df %>% filter(degree_condition != 1) #remove them
  
  model_dat <- df %>%
    exlusions() %>%
    group_by(subject_id) %>% # what if we do variable wise standardization?
    mutate(
      nd = scale(left_net - right_net),
      vd = scale(left_rating - right_rating),
      cd = scale(left_correlation - right_correlation),
      sds = scale(left_sd - right_sd),
      ov = scale(left_rating + right_rating),
      on = scale(left_net + right_net)
    )
  # without normalization
  # model_dat <- df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>% #what if we do variable wise standardization?
  #   mutate(
  #     nd = (left_net - right_net),
  #     vd = (left_rating - right_rating),
  #     cd = (left_correlation - right_correlation),
  #     sds = (left_sd - right_sd),
  #     ov = (left_rating + right_rating),
  #     on = (left_net + right_net)
  #   )
  
  # pca_res <- prcomp(model_dat[,c("nd","vd", "cd","sds","ov","on")],center = F, scale. = FALSE)
  # summary(pca_res)
  # plot(pca_res)
  # biplot(pca_res, scale = 0,choices = c(1,3))
  # model_dat$PC1 <- pca_res$x[,1]
  # model_dat$PC2 <- pca_res$x[,3]
  
  # check correlations
  # model_dat %>%
  #   ungroup() %>%
  #   select(vd, nd, cd,sds,ov,on) %>%
  #   correlation::correlation() %>%
  #   print()
  
  # mlm2 <- glmer(choice ~ vd*nd*cd + (vd*nd*cd | subject_id), data = model_dat,
  #               family=binomial(link="logit"),
  #               control=glmerControl(optimizer="bobyqa",
  #                                    optCtrl=list(maxfun=2e5)))
  #### choice
  # mlm2 <- glmer(choice ~ vd*nd + ov + (vd*nd + ov| subject_id), data = model_dat,
  #               family=binomial(link="logit"),
  #               control=glmerControl(optimizer="bobyqa",
  #                                    optCtrl=list(maxfun=2e5)))
  
  # mlm2_0 <- glmer(choice ~ vd + ov + (vd + ov | subject_id), data = model_dat,
  #               family=binomial(link="logit"),
  #               control=glmerControl(optimizer="bobyqa",
  #                                    optCtrl=list(maxfun=2e5)))
  # mlm2_1 <- glmer(choice ~ vd + ov + nd + (vd + ov + nd| subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_2 <- glmer(choice ~ vd + ov + nd + on + (vd + ov + nd + on| subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  
  mlm2_3 <- glmer(choice ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id),
                  data = model_dat,
                  family = binomial(link = "logit"),
                  control = glmerControl(
                    optimizer = "bobyqa",
                    optCtrl = list(maxfun = 2e5)
                  )
  )
  # mlm2_4 <- glmer(choice ~ vd*nd + ov*on + (vd*nd + ov*on | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_5 <- glmer(choice ~ vd*nd + ov*on + cd + (vd*nd + ov*on + cd | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_6 <- glmer(choice ~ vd*nd + ov*on + sds + ( vd*nd + ov*on + sds | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_7 <- glmer(choice ~ vd*nd + ov*on + sds + cd + ( vd*nd + ov*on + sds + cd| subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  
  file_name <- here::here("tables", paste0("choice_", net_stats[[net_idx]], ".html"))
  
  print(tab_model(mlm2_0, mlm2_1, mlm2_2, mlm2_3, mlm2_4, mlm2_5, mlm2_6, mlm2_7,
                  show.intercept = F,
                  show.aic = T,
                  show.re.var = F,
                  show.ci = FALSE,
                  dv.labels = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8"),
                  pred.labels = c(
                    "Value Difference (vd)", "Overall Value (ov)",
                    "Nework Difference (nd)", "Overall Network (on)",
                    "vd:nd", "ov:on", "Correlation Difference",
                    "Standard-Deviation Difference"
                  ),
                  file = file_name
  ))
  
  print(tab_model(mlm2_3,
                  show.intercept = T,
                  show.aic = T,
                  show.re.var = F,
                  show.ci = F,
                  digits = 4,
                  dv.labels = paste0(net_stats[[net_idx]]),
                  pred.labels = c(
                    "Intercept", "Value Difference (vd)", "Overall Value (ov)",
                    "Nework Difference (nd)", "Overall Network (on)",
                    "vd:nd"
                  ),
                  file = file_name
  ))
  # get_prior(choice ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id), data = model_dat, family = "bernoulli", cores = 10, iter = 10000)
  fit1 <- brm(choice ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id), data = model_dat, family = "bernoulli", cores = 10, iter = 10000)
  # summary(fit1)
  print(bayestestR::sexit(fit1, significant = "default", large = "default", ci = 0.95))
  
  
  # report::report(mlm2_3)
  # print(tab_model(mlm2_0,mlm2_1,mlm2_2,mlm2_3,mlm2_4,
  #           show.intercept = F,
  #           show.aic = T,
  #           show.re.var = F,
  #           show.ci = FALSE,
  #           digits = 4,
  #           dv.labels = c("M1", "M2", "M3", "M4", "M5"),
  #           pred.labels = c("Value Difference (vd)", "Overall Value (ov)",
  #                           "Nework Difference (nd)", "Overall Network (on)",
  #                           "vd:nd","ov:on"
  #                           ),
  #           file = file_name))
  
  # summary(mlm2_3)
  # temp_res <- broom.mixed::tidy(mlm2_4)
  # temp_res$p.value <-  round(temp_res$p.value, 4)
  # print(knitr::kable(temp_res[temp_res$effect == "fixed",3:7],digits = 3,
  #                    caption = paste0("Choice ",net_stats[[net_idx]])))
  #### RT
  model_dat <- df %>%
    exlusions() %>%
    group_by(subject_id) %>%
    mutate(
      nd = scale(abs(left_net - right_net)),
      vd = scale(abs(left_rating - right_rating)),
      cd = scale(abs(left_correlation - right_correlation)),
      sds = scale(abs(left_sd - right_sd)),
      ov = scale(left_rating + right_rating),
      on = scale(left_net + right_net)
    )
  # without normalizations
  # model_dat <- df %>%
  #   exlusions() %>%
  #   mutate(
  #     nd = (abs(left_net - right_net)),
  #     vd = (abs(left_rating - right_rating)),
  #     cd = (abs(left_correlation - right_correlation)),
  #     sds = (abs(left_sd - right_sd)),
  #     ov = (left_rating + right_rating),
  #     on = (left_net + right_net)
  #   )
  
  # mlm1_0 <- lmer(log(rt) ~ vd + ov + (vd + ov | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm1_1 <- lmer(log(rt)  ~ vd + ov + nd + (vd + ov + nd| subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  mlm1_2 <- lmer(log(rt) ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id),
                 data = model_dat,
                 control = lmerControl(
                   optimizer = "bobyqa",
                   optCtrl = list(maxfun = 2e5)
                 )
  )
  # mlm1_3 <- lmer(log(rt)  ~ vd*nd + ov + on + (vd*nd + ov + on | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm1_4 <- lmer(log(rt)  ~ vd*nd + ov*on + (vd*nd + ov*on | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm1_5 <- lmer(log(rt)  ~ vd*nd + ov*on + cd + (vd*nd + ov*on + cd | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm1_6 <- lmer(log(rt)  ~ vd*nd + ov*on + sds + ( vd*nd + ov*on + sds | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm1_7 <- lmer(log(rt)  ~ vd*nd + ov*on + sds + cd + ( vd*nd + ov*on + sds + cd| subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  
  file_name <- here::here("tables", paste0("rt_", net_stats[[net_idx]], ".html"))
  
  print(tab_model(mlm1_2,
                  show.intercept = T,
                  show.aic = T,
                  show.re.var = F,
                  show.ci = FALSE,
                  show.icc = FALSE,
                  digits = 4,
                  dv.labels = paste0(net_stats[[net_idx]]),
                  pred.labels = c(
                    "Intercept", "Value Difference (vd)", "Overall Value (ov)",
                    "Nework Difference (nd)", "Overall Network (on)"
                  ),
                  file = file_name
  ))
  
  fit2 <- brm(log(rt) ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id), data = model_dat, cores = 10, iter = 10000)
  # summary(fit2)
  print(bayestestR::sexit(fit2, significant = "default", large = "default", ci = 0.95))
  # print(tab_model(mlm1_0,mlm1_1,mlm1_2,mlm1_3,mlm1_4,
  #           show.intercept = F,
  #           show.aic = T,
  #           show.re.var = F,
  #           show.ci = FALSE,
  #           digits = 4,
  #           dv.labels = c("M1", "M2", "M3", "M4", "M5"),
  #           pred.labels = c("Value Difference (vd)", "Overall Value (ov)",
  #                           "Nework Difference (nd)", "Overall Network (on)",
  #                           "vd:nd","ov:on"),
  #           file = file_name))
  
  
  model_dat <- df %>%
    exlusions() %>%
    group_by(subject_id) %>%
    mutate(
      nd = scale(abs(left_net - right_net)),
      vd = scale(abs(left_rating - right_rating)),
      cd = scale(abs(left_correlation - right_correlation)),
      sds = scale(abs(left_sd - right_sd)),
      ov = scale(left_rating + right_rating),
      on = scale(left_net + right_net)
    ) %>%
    select(choice, correct, left_rating, right_rating, vd, nd, cd, sds, ov, on, rt, subject_id)
  
  # model_dat <- df %>%
  #   exlusions() %>%
  #   # group_by(subject_id) %>%
  #   mutate(
  #     nd = (abs(left_net - right_net)),
  #     vd = (abs(left_rating - right_rating)),
  #     cd = (abs(left_correlation - right_correlation)),
  #     sds = (abs(left_sd - right_sd)),
  #     ov = (left_rating + right_rating),
  #     on = (left_net + right_net)
  #   ) %>%
  #   select(choice,correct,left_rating,right_rating,vd,nd,cd,sds,ov,on,rt,subject_id)
  
  # mlm2_0 <- glmer(correct ~ vd + ov + (vd + ov | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_1 <- glmer(correct ~ vd + ov + nd + (vd + ov + nd| subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_2 <- glmer(correct ~ vd + ov + nd + on + (vd + ov + nd + on| subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  mlm2_3 <- glmer(correct ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id),
                  data = model_dat,
                  family = binomial(link = "logit"),
                  control = glmerControl(
                    optimizer = "bobyqa",
                    optCtrl = list(maxfun = 2e5)
                  )
  )
  # mlm2_4 <- glmer(correct ~ vd*nd + ov*on + (vd*nd + ov*on | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_5 <- glmer(correct ~ vd*nd + ov*on + cd + (vd*nd + ov*on + cd | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_6 <- glmer(correct ~ vd*nd + ov*on + sds + ( vd*nd + ov*on + sds | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_7 <- glmer(correct ~ vd*nd + ov*on + sds + cd + ( vd*nd + ov*on + sds + cd| subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # create table for the correct incorrect model
  file_name <- here::here("tables", paste0("correct_", net_stats[[net_idx]], ".html"))
  print(tab_model(mlm2_3,
                  show.intercept = T,
                  show.aic = T,
                  show.re.var = F,
                  show.ci = FALSE,
                  digits = 4,
                  dv.labels = paste0(net_stats[[net_idx]]),
                  pred.labels = c(
                    "Intercept", "Value Difference (vd)", "Overall Value (ov)",
                    "Nework Difference (nd)", "Overall Network (on)",
                    "vd:nd"
                  ),
                  file = file_name
  ))
  
  fit3 <- brm(correct ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id), data = model_dat, family = "bernoulli", cores = 10, iter = 10000)
  # summary(fit3)
  print(bayestestR::sexit(fit3, significant = "default", large = "default", ci = 0.95))
  
  # print(tab_model(mlm2_0,mlm2_1,mlm2_2,mlm2_3,mlm2_4,
  #                 show.intercept = F,
  #                 show.aic = T,
  #                 show.re.var = F,
  #                 show.ci = FALSE,
  #                 digits = 4,
  #                 dv.labels = c("M1", "M2", "M3", "M4", "M5"),
  #                 pred.labels = c("Value Difference (vd)", "Overall Value (ov)",
  #                                 "Nework Difference (nd)", "Overall Network (on)",
  #                                 "vd:nd","ov:on"
  #                 ),
  #                 file = file_name))
  
  
  # summary(mlm2_3)
  # temp_res <- broom.mixed::tidy(mlm1_6)
  # temp_res$p.value <-  round(temp_res$p.value, 4)
  # print(knitr::kable(temp_res[temp_res$effect == "fixed",3:8],digits = 3,
  #                    caption = paste0("RT ",net_stats[[net_idx]])))
  file_name <- here::here("tables", paste0("bayes_", net_stats[[net_idx]], ".html"))
  tab_model(fit1, fit3, fit2, dv.labels = c("choice", "correct", "RT"), show.re.var = F, show.icc = FALSE, file = file_name)
}

