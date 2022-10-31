# network_difference_regression_analysis.R - algorithm for selecting sub graphs from preference network

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
# 10/28/22      Kianté  Fernandez                       coded up version one

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

# helper functions for working with lists
list.do <- function(.data, fun, ...) {
  do.call(what = fun, args = as.list(.data), ...)
}
list.cbind <- function(.data) {
  list.do(.data, "cbind")
}
#####loading the data#####
temp_files <- list.files(path = here::here("data", "pilot_30"), pattern = ".json", full.names = T)

# load all the images to calculate the value for a group of foods
food_folder <- here::here("data", "snackitemnames_nicholas", "Lee_Holyoak_2021_images")
FoodNames <- readxl::read_excel(here::here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))

# NOTE NEXT TIME YOU WILL USE THIS FILE INSTEAD. THE 'RES' FILE (BC YOU DID THE NAMES RIGHT)
network_stats <- "LowHighWithinBetween"

temp <- list.files(path = food_folder, pattern = "*.jpg", full.names = T)
foods_in_image <- stringr::str_extract(temp, "item\\d+")
foods_in_image <- stringr::str_extract(foods_in_image, "\\d+")
# get row idx for each of the image numbers
foods_in_image <- tibble::rowid_to_column(data.frame(Image = as.numeric(foods_in_image)))
foods_in_image <- dplyr::left_join(FoodNames, foods_in_image, "Image")
#get correlations between items
lee_2021_rating1 <- read_csv(here::here("data", "lee_2021_rating1.csv"), col_names = FALSE)
cor.snack_food <- SemNeT::similarity(lee_2021_rating1, method = "cor")
cor_snack_food <- data.frame(matrix(cor.snack_food[cor.snack_food != 1], 59, 60))
names(cor_snack_food) <- FoodNames$Name

######
#calculate a bunch of network measures to look at relationship to stuff

source("exploratory_graph_analysis.R")
#get non-negative weights for certain measures
G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
path_lengths <- distances(G)
diag(path_lengths)=NA
apply(path_lengths, 2, mean, na.rm = T)
adj_temp <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")

#here I calculate a range of metrics on the graph 
net_degree <- data.frame(degree= degree(g), 
                         strength = strength(g),
                         eigen = igraph::eigen_centrality(G)$vector,
                         page_rank = page_rank(g)$vector, #weighted
                         weighted_transitivity = transitivity(g, type = "weighted"),
                         closeness = NetworkToolbox::closeness(adj_temp, weighted = TRUE),
                         closeness2 = closeness(G), #weighted
                         betweenness = betweenness(G),
                         participation = NetworkToolbox::participation(adj_temp, comm = V(g)$snack_type)$overall) %>%
  tibble::rownames_to_column("Name") %>%
  left_join(foods_in_image, "Name")

net_degree$snack_type <- V(g)$snack_type
#correlations between stats
net_degree %>% 
  select("degree", "strength", "eigen", "weighted_transitivity", 
         "closeness", "closeness2", "betweenness", "page_rank","participation") %>% 
  correlation::correlation()

# correlogram
net_degree %>% 
  select("degree", "strength", "eigen", "weighted_transitivity", 
         "closeness", "closeness2", "betweenness","page_rank","participation") %>% 
  ggstatsplot::ggcorrmat(
    type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
    colors = c("darkred", "white", "steelblue") # change default colors
  )


file_idx <- 30
subject_df <- vector(mode = "list", length = file_idx)

for (pp in seq_len(file_idx)) {
  #load the  subjects data
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

  ns <- which.max(map_dbl(map(network_stats, grepl, x = subject_temp$options[subject_temp$screen_id == "task"]),sum))
  
  set_values_temp <- vector(mode = "numeric", length = 100)
  set_network_temp <- vector(mode = "numeric", length = 100)
  set_cluster_temp <- vector(mode = "numeric", length = 100)
  set_correlations_temp <- vector(mode = "numeric", length = 100)
  #TODO try also the sum SD of the ratings
  
  #LOAD THE generated subgraphs
  load(file = here::here("data", paste0(network_stats[[1]], ".RData")))
  
  for (foo in 1:100) {
    #select which stat to calculate
    
    # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$degree)
    set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$strength)
    # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$weighted_transitivity)
    # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$eigen)
    # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$closeness)
    # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$betweenness)
    # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$page_rank)
    # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$participation)
    
    set_values_temp[[foo]] <-  sum(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
    set_correlations_temp[[foo]] <-        sum(apply(cor_snack_food[colnames(cor_snack_food) %in% res[[foo]],],2,mean, na.rm = T)[res[[foo]]])
    # print(set_correlations_temp)
    
    if (foo %in% 1:25){
      #rsize
      set_cluster_temp[[foo]] <- 1
    }else if(foo %in% 26:50){
      #wsize
      set_cluster_temp[[foo]] <- 2
    }else if(foo %in% 51:75){
      #losize
      set_cluster_temp[[foo]] <- 3
    }else {
      #hosize
      set_cluster_temp[[foo]] <- 4
    }
  }
  # print(cor.test(set_values_temp,set_network_temp))

  task_temp <- subject_temp %>%
    filter(screen_id == "task") %>%
    select(subject_id, rt, options, key_press) %>%
    mutate(key_press = ifelse(key_press == "f", 1, 0))
  xxxx <- as.data.frame(do.call(rbind, task_temp$options)) %>% mutate(subject_id = pp)
  xxxx[, 1] <- as.numeric(str_remove(str_remove(xxxx[, 1], pattern =   paste0("../../img/grid_stimuli/grid_6_",network_stats[[ns]],"_")), ".jpg"))
  xxxx[, 2] <- as.numeric(str_remove(str_remove(xxxx[, 2], pattern =   paste0("../../img/grid_stimuli/grid_6_",network_stats[[ns]],"_")), ".jpg"))
  xxxx$rt <- task_temp$rt
  xxxx$choice <- task_temp$key_press
  names(xxxx) <- c("left", "right", "subject_id", "rt", "choice")
  xxxx$network_statistic <- network_stats[[ns]]
  
  xxxx$left_rating <- NULL
  xxxx$right_rating <- NULL
  xxxx$left_net <- NULL
  xxxx$right_net <- NULL
  xxxx$left_correlation <- NULL
  xxxx$right_correlation <- NULL
  xxxx$left_cluster_condition <- NULL
  xxxx$left_cluster_condition <- NULL
  
  for (foo in seq_len(nrow(xxxx))) {
    xxxx$left_rating[[foo]] <- as.numeric(set_values_temp[xxxx$left[[foo]]])
    xxxx$right_rating[[foo]] <- as.numeric(set_values_temp[xxxx$right[[foo]]])
    xxxx$left_net[[foo]] <- as.numeric(set_network_temp[xxxx$left[[foo]]])
    xxxx$right_net[[foo]] <- as.numeric(set_network_temp[xxxx$right[[foo]]])
    xxxx$left_correlation[[foo]] <- as.numeric(set_correlations_temp[xxxx$left[[foo]]])
    xxxx$right_correlation[[foo]] <- as.numeric(set_correlations_temp[xxxx$right[[foo]]])
    xxxx$left_cluster_condition[[foo]] <- as.numeric(set_cluster_temp[xxxx$left[[foo]]])
    xxxx$right_cluster_condition[[foo]] <- as.numeric(set_cluster_temp[xxxx$right[[foo]]])
  }
  xxxx$value_network_corr <- cor(set_values_temp,set_network_temp)
  xxxx$value_network_corr_p <-   cor.test(set_values_temp,set_network_temp)$p.value
  
  subject_df[[pp]] <- xxxx
}

df <- as.data.frame(do.call(rbind, subject_df)) %>%
  unnest(cols = c(left_rating, right_rating, left_net, right_net, left_cluster_condition, right_cluster_condition,
                  left_correlation,right_correlation))

df$cluster_condition <- df$left_cluster_condition %in% c(3,4) | df$right_cluster_condition %in% c(3,4)
# df$degree_condition <- df$left_cluster_condition %in% c(1,2) | df$right_cluster_condition %in% c(1,2)
df$degree_condition <- df$left_cluster_condition %in% c(1) | df$right_cluster_condition %in% c(1)

# df$degree_condition <- df$left_cluster_condition %in% c(2) | df$right_cluster_condition %in% c(2)

df %>%
  select(subject_id,value_network_corr, value_network_corr_p) %>%
  distinct() %>%
  filter(value_network_corr_p > .05)

df$correct <- df$left_rating > df$right_rating & df$choice == 1

# df$degree_condition <- df$left_cluster_condition %in% c(4) & df$right_cluster_condition %in% c(4)
# df$degree_condition <- df$left_cluster_condition %in% c(1) | df$right_cluster_condition %in% c(1)
# df <- df %>% filter(degree_condition != 1)
# df$degree_condition <- df$left_cluster_condition %in% c(3) | df$right_cluster_condition %in% c(3)
# df <- df %>% filter(degree_condition != 1)

# df <- df %>% filter(cluster_condition != 1)
#robustness check for correlation between net stat and value
# df <- df %>% filter(value_network_corr_p > .05)

####data analysis
exlusions <- function (df){
  #function for data exclusions
  temp <- df %>% 
    group_by(subject_id) %>% #response times
    mutate(Q1 = quantile(rt, .25),
           Q3 = quantile(rt, .75),
           IQR = IQR(rt)) %>% 
    filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
    # filter(subject_id != 8) %>% #subject level exclusions?
    # filter(subject_id != 9) %>%
    # filter(subject_id != 13) %>%
    # filter(subject_id != 4) %>%
    ungroup() %>%
    filter(!rt <= 250) %>% #response times cutoffs
    filter(!rt >= 10000)
  return(temp)
}
  
model_dat <- df %>% 
  exlusions() %>% 
  mutate(eq = left_cluster_condition == right_cluster_condition) %>% 
  group_by(subject_id) %>%
  mutate(
    nd = scale(left_net - right_net),
    vd = scale(left_rating - right_rating),
    cd = scale(left_correlation - right_correlation)
  )

mlm2 <- glmer(choice ~ vd*nd +  (vd*nd | subject_id), data = model_dat, 
              family=binomial(link="logit"),
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(mlm2)


mlm2_00 <- glmer(choice ~ vd + (vd | subject_id), data = model_dat, 
                family=binomial(link="logit"),
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
mlm2_0 <- glmer(choice ~ vd + nd +  (vd + nd | subject_id), data = model_dat, 
              family=binomial(link="logit"),
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
mlm2 <- glmer(choice ~ vd*nd +  (vd*nd | subject_id), data = model_dat, 
              family=binomial(link="logit"),
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(mlm2)
# mlm2_1 <- glmer(choice ~ vd*cd +  (vd*cd | subject_id), data = model_dat, 
#                 family=binomial(link="logit"),
#                 control=glmerControl(optimizer="bobyqa",
#                                      optCtrl=list(maxfun=2e5)))
mlm2_2 <- glmer(choice ~ vd*nd + cd +  (vd*nd + cd | subject_id), data = model_dat, 
                family=binomial(link="logit"),
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
mlm2_3 <- glmer(choice ~ vd*nd*cd +  (vd*nd*cd | subject_id), data = model_dat, 
                family=binomial(link="logit"),
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
# anova(mlm2_00,mlm2_0,mlm2,mlm2_2,mlm2_3)

# summary(mlm2_3)
# summary(mlm2)
# summary(mlm2_1)
summary(mlm2_2)
map(list(mlm2_0,mlm2,mlm2_2,mlm2_3),summary)
# library(brms)
# fit<- brm(choice ~ vd*nd +  (vd*nd | subject_id), data = model_dat, family = "bernoulli", cores = 10)
# summary(fit)

#check correlations
df %>%
  exlusions() %>% 
  mutate(vd = left_rating - right_rating,
         nd = left_net - right_net, 
         cd = left_correlation - right_correlation
  ) %>% 
  select(vd, nd, cd) %>% 
  correlation::correlation()


#when you look at comparisons of groups between the stim, you do not get the effect

###response times
model_dat <- df %>% 
  exlusions() %>% 
  mutate(eq = left_cluster_condition == right_cluster_condition) %>% 
  group_by(subject_id) %>%
  mutate(
    nd = scale(abs(left_net - right_net)),
    vd = scale(abs(left_rating - right_rating)),
    cd = scale(abs(left_correlation - right_correlation))
  )
model_dat$eq = factor(model_dat$eq,labels = c("Distinct", "Same")) #groups of items sampled

mlm1 <- lmer(log(rt) ~ vd*nd + (vd*nd | subject_id), data = model_dat,
             control=lmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
# mlm1 <- lmer(log(rt) ~  poly(vd,degree = 2, raw = TRUE)*poly(nd,degree = 2, raw = TRUE) + (vd*nd| subject_id), data = model_dat)
# mlm1 <- lmer(log(rt) ~ poly(vd,degree = 2, raw = TRUE)*poly(nd,degree = 2, raw = TRUE)*cluster_condition*eq + (1| subject_id), data = model_dat)
mlm1 <- lmer(log(rt) ~ vd*nd*eq  + (vd*nd*eq| subject_id), data = model_dat,
             control=lmerControl(optimizer="bobyqa",
                                 optCtrl=list(maxfun=2e5)))
# mlm1 <- lmer(log(rt) ~ vd*nd*cd + (vd*nd*cd| subject_id), data = model_dat)
summary(mlm1)


