# get_data.R - get the data from goolge drive
#
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
# 11/09/22      Kianté  Fernandez                       wrote code
# 12/10/22      Kianté  Fernandez                       added plotting
# 29/10/22      Kianté  Fernandez                       updated analysis


library(googledrive) # An Interface to Google Drive
library(jsonlite) # A Simple and Robust JSON Parser and Generator for R
library(purrr) # Functional Programming Tools
library(tidyverse) # Easily Install and Load the 'Tidyverse'

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

differenceNet <- function(dat, subject = 1, cut.off = T){
  #get the individual level network using value difference matrix
  individual_rate_diff <- matrix(,nrow = ncol(dat), ncol = ncol(dat))
  for (col_idx in 1:ncol(individual_rate_diff)){
    food1_temp <- dat[[subject,col_idx]]
    
    for (row_idx in 1:nrow(individual_rate_diff)){
      food2_temp <- dat[[subject,row_idx]]
      individual_rate_diff[row_idx,col_idx] <- as.numeric(abs(food1_temp - food2_temp))
    }
  }
  colnames(individual_rate_diff) <- FoodNames$Name
  rownames(individual_rate_diff) <- FoodNames$Name
  
  g_temp <- SemNeT::similarity(individual_rate_diff,method = "cosine")
  graph_individual <- graph_from_adjacency_matrix(g_temp,
                                                  "undirected",
                                                  weighted = TRUE,
                                                  diag = F
  )
  if (cut.off == T){
    cut.off <- mean(abs(E(graph_individual)$weight))
    graph_individual <- delete_edges(graph_individual, E(graph_individual)[abs(E(graph_individual)$weight) < cut.off])
  }
  return(graph_individual)
}

# drive_auth_configure(api_key = "AIzaSyBO6kKhgHGvepPphJw4Y6XtGBG98hcwaNs")
# drive_api_key()
# # 
# # # apply to a browser URL for, e.g., a Google Sheet
# my_url <- "https://drive.google.com/drive/folders/1PFHm5wI7hOz4eu1gRppwtgVFZkl5M_tk"
# # 
# for (file_idx in seq_len(dim(drive_ls(drive_get(my_url)))[[1]])) {
#   temp <- drive_ls(drive_get(my_url))$drive_resource[[file_idx]]$originalFilename
#   drive_download(temp, here::here("data", "pilot_5", temp), overwrite = TRUE)
# }

temp_files <- list.files(path = here::here("data", "pilot_30"), pattern = ".json", full.names = T)

# load all the images to calculate the value for a group of foods
food_folder <- here::here("data", "snackitemnames_nicholas", "Lee_Holyoak_2021_images")
FoodNames <- readxl::read_excel(here::here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))

# pilot_5_stimuli_sets <- read_csv("data/pilot_5/pilot_5_stimuli.csv", col_names = FALSE)

# NOTE NEXT TIME YOU WILL USE THIS FILE INSTEAD. THE 'RES' FILE (BC YOU DID THE NAMES RIGHT)
network_stats <- c("assortment", "edge_density", "weighted_clustering_coefficient", 
                   "LowHighWithinBetween")

# load(file = here::here("data", paste0(network_stats[[1]], "_", 30, "_", 6, ".RData")))
# The 60 items we have in this data set.
images <- c(
  1, 5, 8, 12, 13, 16, 17, 18, 25, 26, 28, 33, 39, 51, 55, 60,
  61, 63, 65, 70, 77, 80, 93, 96, 98, 99, 100, 102, 103, 104, 105,
  106, 114, 115, 118, 119, 123, 124, 131, 142, 150, 153, 154, 155,
  158, 159, 160, 161, 164, 165, 168, 170, 172, 173, 174, 176, 184,
  187, 188, 200
)

temp <- list.files(path = food_folder, pattern = "*.jpg", full.names = T)


foods_in_image <- stringr::str_extract(temp, "item\\d+")
foods_in_image <- stringr::str_extract(foods_in_image, "\\d+")
# get row idx for each of the image numbers
foods_in_image <- tibble::rowid_to_column(data.frame(Image = as.numeric(foods_in_image)))
foods_in_image <- dplyr::left_join(FoodNames, foods_in_image, "Image")

lee_2021_rating1 <- read_csv(here::here("data", "lee_2021_rating1.csv"), col_names = FALSE)
names(lee_2021_rating1) <- FoodNames$Name

cor.snack_food <- SemNeT::similarity(lee_2021_rating1, method = "cor")
cor_snack_food <- data.frame(matrix(cor.snack_food[cor.snack_food != 1], 59, 60))
names(cor_snack_food) <- FoodNames$Name

######
#caculate a bunch of network measures to look at relationship to stuff

source("exploratory_graph_analysis.R")

G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
path_lengths <- distances(G)
# diag(TEST)=NA
# apply(TEST, 2, mean, na.rm = T)
adj_temp <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")


#here I calculate a range of metrics on the graph 
net_degree <- data.frame(degree= degree(g), 
                         strength = strength(g),
                         eigen = igraph::eigen_centrality(G)$vector,
                         page_rank = page_rank(g)$vector, #weighted
                         weighted_transitivity = transitivity(g, type = "weighted"),
                         closeness = NetworkToolbox::closeness(adj_temp, weighted = TRUE),
                         closeness2 = closeness(G), 
                         betweenness = betweenness(G),
                         average_path_length = apply(path_lengths, 2, mean, na.rm = T)) %>%
  tibble::rownames_to_column("Name") %>%
  left_join(foods_in_image, "Name")

net_degree$snack_type <- V(g)$snack_type

net_degree %>% 
  select("degree", "strength", "eigen", "weighted_transitivity", 
           "closeness", "closeness2", "betweenness", "average_path_length", "page_rank") %>% 
  correlation::correlation()

# correlogram
# net_degree %>% 
#   select("degree", "strength", "eigen", "weighted_transitivity", 
#          "closeness", "closeness2", "betweenness", "average_path_length","page_rank") %>% 
#   ggstatsplot::ggcorrmat(
#   type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
#   colors = c("darkred", "white", "steelblue") # change default colors
# )

file_idx <- 30
subject_df <- vector(mode = "list", length = file_idx)
#we want the ratings with the condition indicator to test for rating bias
# subject_ratings <- vector(mode = "list", length = file_idx)
subject_ratings <- matrix(NA,nrow = file_idx*100, ncol = 5)
colnames(subject_ratings) <- c("stimuli", "subject_idx","sum_rating","condition","net_stat")
# pp = 1

subject_individual_ratings <- matrix(NA,nrow = file_idx*60, ncol = 5)
colnames(subject_individual_ratings) <- c("subject_idx", "response", "Image", "Item", "Name" )

rate_counter <- 0
sub_rat_counter <- 0

# pp <- 2
for (pp in seq_len(file_idx)) {
# for (pp in 7:file_idx) {
   # pp =7 
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
  #zero out the fav item
  
  # subject_rating_temp$response[which.max(subject_rating_temp$response)][[1]] <- 0
  
  sub_rat_idx <- seq(1,60) + sub_rat_counter
  subject_individual_ratings[sub_rat_idx,1] <- rep(pp,60)
  subject_individual_ratings[sub_rat_idx,5] <-  unlist(subject_rating_temp[,5])
  subject_individual_ratings[sub_rat_idx,2] <-  unlist(subject_rating_temp[,2])
  subject_individual_ratings[sub_rat_idx,3] <-  unlist(subject_rating_temp[,3])
  subject_individual_ratings[sub_rat_idx,4] <-  unlist(subject_rating_temp[,4])

  # we need to score each of the stimuli for a subject:
  # BUT somehow the networks are not the ones we saved as images,
  # so, we cannot do this until we generate an excel sheet of the images
  # sort of like the res file, but correct ,
  # this gets the choices in check use old fig temp todo this
  
  #use individual networks?
  # subject_individual_ratings <- as.data.frame(subject_individual_ratings)
  # subject_individual_ratings$subject_idx <- as.numeric(subject_individual_ratings$subject_idx)
  # subject_individual_ratings$response <- as.numeric(subject_individual_ratings$response)
  # subject_individual_ratings$Image <- as.numeric(subject_individual_ratings$Image)
  # subject_individual_ratings$Item <- as.numeric(subject_individual_ratings$Item)
  # subject_individual_ratings$Name <- as.factor(subject_individual_ratings$Name)
  # 
  # TEST <- subject_individual_ratings %>% 
  #   filter(subject_idx == pp) %>% 
  #   select(response,Name) %>% 
  #   pivot_wider(names_from = Name, values_from = response) %>% 
  #   unnest(everything())
  # #make the order of the names the same
  # TEST<-TEST[names(lee_2021_rating1)]
  # 
  # individual_net_temp <- differenceNet(TEST[1,], subject = 1, cut.off = T)
  # g <- individual_net_temp
  # G <- g
  # E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
  # path_lengths <- distances(G)
  # adj_temp <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")
  # 
  # #do the test for each subejct
  # net_degree <- data.frame(degree= degree(g), 
  #                          strength = strength(g),
  #                          eigen = igraph::eigen_centrality(G)$vector,
  #                          page_rank = page_rank(g)$vector, #weighted
  #                          weighted_transitivity = transitivity(g, type = "weighted"),
  #                          closeness = NetworkToolbox::closeness(adj_temp, weighted = TRUE),
  #                          closeness2 = closeness(G), 
  #                          betweenness = betweenness(G),
  #                          average_path_length = apply(path_lengths, 2, mean, na.rm = T)) %>% 
  #   tibble::rownames_to_column("Name")
    
  # plot(individual_net_temp)
  
  #what network wise was the subject?
  ns <- which.max(map_dbl(map(network_stats, grepl, x = subject_temp$options[subject_temp$screen_id == "task"]),sum))
  
  set_values_temp <- vector(mode = "numeric", length = 100)
  set_network_temp <- vector(mode = "numeric", length = 100)
  set_cluster_temp <- vector(mode = "numeric", length = 100)
  set_correlations_temp <- vector(mode = "numeric", length = 100)
  # if (pp %in% c(1,2,3,4,5,6)){ #for initial pilot small test
  #   for (foo in 1:30) {
  #     set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% pilot_5_stimuli_sets[[foo]], ]$strength)
  #     set_values_temp[[foo]] <- sum(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% pilot_5_stimuli_sets[[foo]], ]$response))
  #   }
  #   
  # } else {
  #   load(file = here::here("data", paste0(network_stats[[ns]], "_", 30, "_", 6, ".RData")))
  #   
  #   for (foo in 1:30) {
  #     #set_values_temp[[foo]] <- sum(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% pilot_5_stimuli_sets[[foo]], ]$response))
  #     #proper way (when you save the images correctly)
  #     set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$strength)
  #     set_values_temp[[foo]] <-  sum(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
  #   }
  # }
    #LOAD THE generated subgraphs
    load(file = here::here("data", paste0(network_stats[[4]], ".RData")))
    
    #get individual network (the differenceNet function)
    # foo = 1
    for (foo in 1:100) {
      
      
      # remove_snacks <- net_degree[!net_degree$Name %in% dput(res[,foo]),]$Name
      # g_temp <- delete_vertices(individual_net_temp, remove_snacks)
      
      
      #set_values_temp[[foo]] <- sum(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% pilot_5_stimuli_sets[[foo]], ]$response))
      #proper way (when you save the images correctly)
      set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$degree)
      # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$strength)
      # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$weighted_transitivity)
      # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$eigen)
      # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$closeness)
      # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$betweenness)
      # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$average_path_length)
      # set_network_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$page_rank)
      
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
        set_cluster_temp[[foo]] <- 3 #3
      }else {
        #hosize
        set_cluster_temp[[foo]] <- 4 #4
      }
    }
    rat_idx <- seq(1,100) + rate_counter
    subject_ratings[rat_idx,1] <- seq(1,100)
    subject_ratings[rat_idx,2] <-  rep(pp,100)
    subject_ratings[rat_idx,3] <-  set_values_temp
    subject_ratings[rat_idx,4] <-  set_cluster_temp
    subject_ratings[rat_idx,5] <-  set_network_temp
    
    rate_counter <- rate_counter + 100
    sub_rat_counter <- sub_rat_counter + 60
    
    # print(cor.test(set_values_temp,set_network_temp))
  # value_network_corr[[pp]] <- cor(set_values_temp,set_network_temp)

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

df$cluster_condition <- df$left_cluster_condition %in% c(2,4) | df$right_cluster_condition %in% c(2,4)

# df$degree_condition <- df$left_cluster_condition %in% c(1,2) | df$right_cluster_condition %in% c(1,2)
df$degree_condition <- df$left_cluster_condition %in% c(1) | df$right_cluster_condition %in% c(1)
# df$degree_condition <- df$left_cluster_condition %in% c(2) | df$right_cluster_condition %in% c(2)
#this tests the difference in conditions for selecting from grouping 1 or 2, or
# selecting from within a given grouping 
df$eq <- factor(df$left_cluster_condition + df$right_cluster_condition)
df$eq <- relevel(df$eq, ref = "0")

df %>%
  select(left_cluster_condition,right_cluster_condition,eq) %>%
  distinct() %>% arrange(eq)

df %>%
  select(subject_id,value_network_corr, value_network_corr_p) %>%
  distinct() %>%
  filter(value_network_corr_p > .05)

for (subject_idx in 1:30){
  # if (subject_idx %in% c(1,4,8,16,24,25,26,27,29)){next}
    
  df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))
  
  temp_df <- df %>% 
    filter(subject_id == subject_idx) %>% 
    filter(!rt <= 250) %>% 
    filter(!rt >= 10000) %>% 
    mutate(Q1 = quantile(rt, .25),
           Q3 = quantile(rt, .75),
           IQR = IQR(rt)) %>% 
    filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
    mutate(
      vd = scale(abs(left_rating - right_rating)),
      nd = scale(abs(left_net - right_net)))
  print(paste0("######## subject: ", subject_idx, " #######"))
  print(summary(glm(correct ~ vd*nd, family = binomial, data = temp_df)))
  # print(broom::tidy(glm(correct ~ vd*nd, family = binomial, data = temp_df)))
  # temp_res <- broom::tidy(glm(choice ~ vd:nd , family = binomial, data = temp_df))
  # temp_res <- broom::tidy(glm(correct ~ vd*nd , family = binomial, data = temp_df))
  
  # temp_res <- broom::tidy(lm(rt ~ eq, data = temp_df))
  # temp_res$p.value <-  round(temp_res$p.value, 4)
  # print(temp_res)
}

# 1,4,8,16,24,25,27

# df$correct <- df$left_rating > df$right_rating & df$choice == 1

# df <- df %>% filter(degree_condition != 1)
# df <- df %>% filter(cluster_condition != 1)

#robustness check for correlation between net stat and value
# df <- df %>% filter(value_network_corr_p > .05)

# 1,4,8,24,25,27

df %>% 
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  # filter(subject_id != 9) %>%
  # filter(subject_id != 13) %>%
  # filter(subject_id != 1) %>%
  # filter(subject_id != 4) %>%
  # filter(subject_id != 8) %>%
  # filter(subject_id != 24) %>%
  # filter(subject_id != 25) %>%
  # filter(subject_id != 27) %>%
filter(subject_id != 1) %>%
filter(subject_id != 4) %>%
filter(subject_id != 8) %>%
filter(subject_id != 24) %>%
filter(subject_id != 25) %>%
filter(subject_id != 27) %>%
filter(subject_id != 16) %>%
filter(subject_id != 26) %>%
filter(subject_id != 29) %>%
filter(subject_id != 30) %>%
  mutate(vd = left_rating - right_rating,
         nd = left_net - right_net) %>% 
  mutate(
    binned_value_diff = as.numeric(cut_number(vd,7)) - 4,
  ) %>% 
  group_by(subject_id,binned_value_diff) %>%
  mutate(
    binned_net_diff = as.numeric(cut_number(nd,5)) - 3
  ) %>% 
  group_by(binned_net_diff,binned_value_diff) %>%
  mutate(n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = binned_value_diff, y = m_left, color = factor(binned_net_diff))) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  theme_classic() +
  geom_line(size = 1) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    y = "Probability of Choosing Left",
    x = "Value Difference (L-R)",
    color = "Network Difference (L-R)"
  )

df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 1) %>%
  filter(subject_id != 4) %>%
  filter(subject_id != 8) %>%
  filter(subject_id != 24) %>%
  filter(subject_id != 25) %>%
  filter(subject_id != 27) %>%
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(vd = left_rating - right_rating,
         nd = left_net - right_net) %>% 
  mutate(
    binned_value_diff = as.numeric(cut_number(vd,7)) - 4,
  ) %>% 
  group_by(subject_id,binned_value_diff) %>%
  mutate(
    binned_net_diff = as.numeric(cut_number(nd,5)) - 3
  ) %>% 
  group_by(subject_id,binned_net_diff,binned_value_diff) %>%
  mutate(
    q1 = quantile(rt, .1),
    q3 = quantile(rt, .3),
    q5 = quantile(rt, .5),
    q7 = quantile(rt, .7),
    q9 = quantile(rt, .9),
  ) %>%
  pivot_longer(cols = q1:q9,
               names_to = "quantiles",
               values_to = "rts"
  ) %>% ungroup() %>% 
  group_by(quantiles,binned_net_diff,binned_value_diff) %>%
  mutate(n = n(),
    m_rt = mean(rts),
    se = sqrt(var(rts) / length(rts))
  ) %>%
  ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(quantiles), shape =factor(binned_net_diff))) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  theme_classic() +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "RT(s)",
    x = "Value Difference (L-R)"
  ) + theme(legend.position="top") + facet_wrap(~quantiles)


df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 1) %>%
  filter(subject_id != 4) %>%
  filter(subject_id != 8) %>%
  filter(subject_id != 24) %>%
  filter(subject_id != 25) %>%
  filter(subject_id != 27) %>%
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(vd = left_rating + right_rating,
         nd = left_net - right_net) %>% 
  mutate(
    binned_value_diff = as.numeric(cut_number(vd,7))
  ) %>%
  group_by(subject_id,binned_value_diff) %>%
  mutate(
    q1 = quantile(rt, .1),
    q3 = quantile(rt, .3),
    q5 = quantile(rt, .5),
    q7 = quantile(rt, .7),
    q9 = quantile(rt, .9),
  ) %>%
  pivot_longer(cols = q1:q9,
               names_to = "quantiles",
               values_to = "rts"
  ) %>% ungroup() %>% 
  group_by(quantiles,binned_value_diff) %>%
  mutate(n = n(),
         m_rt = mean(rts),
         se = sqrt(var(rts) / length(rts))
  ) %>%
  ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(quantiles))) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  theme_classic() +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "RT(s)",
    x = "Value Magnitude (L + R)"
  ) + theme(legend.position="bottom")


tune <- 1
df %>% 
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 1) %>%
  filter(subject_id != 4) %>%
  filter(subject_id != 8) %>%
  filter(subject_id != 24) %>%
  filter(subject_id != 25) %>%
  filter(subject_id != 27) %>%
  mutate(vd = left_rating - right_rating,
         nd = left_net - right_net) %>%
  mutate(
    binned_net_diff = as.numeric(cut_number(nd,3)) - 2
    
  ) %>% 
  group_by(subject_id,binned_net_diff) %>%
  mutate(
    binned_value_diff = as.numeric(cut_number(vd,3)) - 2
  ) %>% 
  ungroup() %>%
  group_by(subject_id,binned_value_diff, binned_net_diff) %>%
  mutate(n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = binned_value_diff, y = m_left, color = factor(binned_net_diff))) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  theme_classic() +
  geom_line(size = 1) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    y = "Probability of Choosing Left",
    x = "Value Difference (L-R)",
    color = "Network Difference (L-R)"
  ) + facet_wrap(~subject_id)

df %>% 
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 1) %>%
  filter(subject_id != 4) %>%
  filter(subject_id != 8) %>%
  filter(subject_id != 24) %>%
  filter(subject_id != 25) %>%
  filter(subject_id != 27) %>%
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(eq = left_cluster_condition == right_cluster_condition) %>% 
  group_by(eq, subject_id) %>% 
  summarise(p_correct = mean(correct),
            q1 = quantile(rt, .1),
            q3 = quantile(rt, .3),
            q5 = quantile(rt, .5),
            q7 = quantile(rt, .7),
            q9 = quantile(rt, .9)) %>% 
  pivot_longer(cols = q1:q9,
               names_to = "quantiles",
               values_to = "rts"
  ) %>% 
  group_by(eq,quantiles) %>% 
  summarize(n = n(),
            mean = mean(rts),
            sd = sd(rts),
            se = sd/sqrt(n)) %>% 
  ggplot(aes(x = eq,
             y = mean, 
             group=quantiles, 
             color=quantiles)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin  =mean - se, 
                    ymax = mean+se), 
                width = .1)+
  theme_classic()+
  scale_color_brewer(palette="Set1") +
  theme_minimal() +
  labs(x = "cluster", 
       y = "rt",
       color = "quantiles")

df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 1) %>%
  filter(subject_id != 4) %>%
  filter(subject_id != 8) %>%
  filter(subject_id != 24) %>%
  filter(subject_id != 25) %>%
  filter(subject_id != 27) %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(rt = rt/1000) %>%
  ggplot(aes(subject_id, rt, fill = factor(subject_id), group = factor(subject_id))) +
  ggdist::stat_halfeye(justification = -.3, point_colour = NA) +
  geom_boxplot(width = .1, outlier.shape = NA) +
  gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .5) +
  theme_classic() +
  labs(x = "subjects", y = "RT(s)") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_wrap(~subject_id, scales = "free")

p3 <- df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  # filter(subject_id != 1) %>%
  # filter(subject_id != 4) %>%
  # filter(subject_id != 8) %>%
  # filter(subject_id != 24) %>%
  # filter(subject_id != 25) %>%
  # filter(subject_id != 27) %>%
filter(subject_id != 1) %>%
filter(subject_id != 4) %>%
filter(subject_id != 8) %>%
filter(subject_id != 24) %>%
filter(subject_id != 25) %>%
filter(subject_id != 27) %>%
filter(subject_id != 16) %>%
filter(subject_id != 26) %>%
filter(subject_id != 29) %>%
filter(subject_id != 30) %>%
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(vd = left_rating - right_rating,
         nd = left_net - right_net) %>%
  group_by(subject_id) %>%
  mutate(
    binned_value_diff = as.numeric(cut_number(vd,5)),
  ) %>% 
  group_by(subject_id, binned_value_diff) %>%
  mutate(
    q1 = quantile(rt, .1),
    q3 = quantile(rt, .3),
    q5 = quantile(rt, .5),
    q7 = quantile(rt, .7),
    q9 = quantile(rt, .9),
    # m_rt = mean(rt),
    # se = sqrt(var(rt) / length(rt))
  ) %>%
  pivot_longer(cols = q1:q9,
               names_to = "quantiles",
               values_to = "rts"
  ) %>% ungroup() %>% 
  ggplot(aes(x = binned_value_diff, y = rts, group = quantiles, color = quantiles)) +
  geom_point()+
  # geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  theme_classic() +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "RT(s)",
    x = "Value Difference (L-R)"
  ) +
  facet_wrap(~subject_id, scales = "free")+
  theme(legend.position="none")

  
p4 <- df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  # filter(subject_id != 1) %>%
  # filter(subject_id != 4) %>%
  # filter(subject_id != 8) %>%
  # filter(subject_id != 24) %>%
  # filter(subject_id != 25) %>%
  # filter(subject_id != 27) %>%
filter(subject_id != 1) %>%
filter(subject_id != 4) %>%
filter(subject_id != 8) %>%
filter(subject_id != 24) %>%
filter(subject_id != 25) %>%
filter(subject_id != 27) %>%
filter(subject_id != 16) %>%
filter(subject_id != 26) %>%
filter(subject_id != 29) %>%
filter(subject_id != 30) %>%
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(nd = left_net - right_net) %>%
  group_by(subject_id) %>%
  mutate(
    binned_network_diff = as.numeric(cut_number(nd,5)),
  ) %>% 
  group_by(subject_id, binned_network_diff) %>%
  mutate(
    q1 = quantile(rt, .1),
    q3 = quantile(rt, .3),
    q5 = quantile(rt, .5),
    q7 = quantile(rt, .7),
    q9 = quantile(rt, .9),
    # m_rt = mean(rt),
    # se = sqrt(var(rt) / length(rt))
  ) %>%
  pivot_longer(cols = q1:q9,
               names_to = "quantiles",
               values_to = "rts"
  ) %>% ungroup() %>% 
  ggplot(aes(x = binned_network_diff, y = rts, group = quantiles, color = quantiles)) +
  geom_point()+
  # geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  theme_classic() +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "RT(s)",
    x = "Network Difference (L-R)"
  ) +
  facet_wrap(~subject_id, scales = "free")+
  theme(legend.position="none")



p1 <- df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  # filter(subject_id != 1) %>%
  # filter(subject_id != 4) %>%
  # filter(subject_id != 8) %>%
  # filter(subject_id != 24) %>%
  # filter(subject_id != 25) %>%
  # filter(subject_id != 27) %>%
  filter(subject_id != 1) %>%
  filter(subject_id != 4) %>%
  filter(subject_id != 8) %>%
  filter(subject_id != 24) %>%
  filter(subject_id != 25) %>%
  filter(subject_id != 27) %>%
  filter(subject_id != 16) %>%
  filter(subject_id != 26) %>%
  filter(subject_id != 29) %>%
  filter(subject_id != 30) %>%
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(vd = left_rating - right_rating) %>%
  group_by(subject_id) %>%
  mutate(
    binned_value_diff = as.numeric(cut_number(vd,7)),
  ) %>% 
  group_by(subject_id, binned_value_diff) %>%
  mutate(
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_value_diff, y = m_left, group = subject_id)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  theme_classic() +
  geom_line(size = 1) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    y = "Probability of Choosing Left",
    x = "Value Difference (L-R)"
  ) +
  facet_wrap(~subject_id)

p2 <- df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  # filter(subject_id != 1) %>%
  # filter(subject_id != 4) %>%
  # filter(subject_id != 8) %>%
  # filter(subject_id != 24) %>%
  # filter(subject_id != 25) %>%
  # filter(subject_id != 27) %>%
  filter(subject_id != 1) %>%
  filter(subject_id != 4) %>%
  filter(subject_id != 8) %>%
  filter(subject_id != 24) %>%
  filter(subject_id != 25) %>%
  filter(subject_id != 27) %>%
  filter(subject_id != 16) %>%
  filter(subject_id != 26) %>%
  filter(subject_id != 29) %>%
  filter(subject_id != 30) %>%
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(nd = left_net - right_net) %>%
  group_by(subject_id) %>%
  mutate(
    binned_network_diff = as.numeric(cut_number(nd,7)),
  ) %>% 
  group_by(subject_id, binned_network_diff) %>%
  mutate(
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_network_diff, y = m_left, group = subject_id)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  theme_classic() +
  geom_line(size = 1) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    y = "Probability of Choosing Left",
    x = "Network Difference (L-R)"
  ) +
  facet_wrap(~subject_id)

#plot individual subject choice and rt curves
p1 + p2
p3 + p4

####data analysis
df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))

model_dat <- df %>% 
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  # filter(subject_id != 1) %>%
  # filter(subject_id != 4) %>%
  # filter(subject_id != 8) %>%
  # filter(subject_id != 24) %>%
  # filter(subject_id != 25) %>%
  # filter(subject_id != 27) %>%
  # filter(subject_id != 16) %>%
  # filter(subject_id != 26) %>%
  # filter(subject_id != 29) %>%
  # filter(subject_id != 30) %>%
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  group_by(subject_id) %>%
  mutate(
    nd = scale(left_net - right_net),
    vd = scale(left_rating - right_rating),
    cd = scale(left_correlation - right_correlation)
  )

model_dat$cluster_condition = factor(model_dat$cluster_condition,labels = c("No Comparison", "Cluster Comparison"))
# model_dat$eq = factor(model_dat$eq,labels = c("Distinct", "Same"))

# mlm1 <- lmer(log(rt) ~ abs(vd)*abs(nd) + (vd*nd| subject_id), data = model_dat)

# mlm1 <- lmer(log(rt) ~  poly(vd,degree = 2, raw = TRUE)*poly(nd,degree = 2, raw = TRUE) + (vd*nd| subject_id), data = model_dat)
# # mlm1 <- lmer(log(rt) ~ poly(vd,degree = 2, raw = TRUE)*poly(nd,degree = 2, raw = TRUE)*cluster_condition*eq + (1| subject_id), data = model_dat)
# # mlm1 <- lmer(log(rt) ~ vd*nd*cluster_condition*eq  + (1| subject_id), data = model_dat)
# summary(mlm1)

# mlm2 <- glmer(choice ~ vd*nd +  (1 | subject_id), data = model_dat, family = "binomial")
# summary(mlm2)

mlm2 <- glmer(choice ~ vd*nd +  (vd*nd | subject_id), data = model_dat, 
              family=binomial(link="logit"),
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(mlm2)
mlm2_1 <- glmer(choice ~ vd*cd +  (vd*cd | subject_id), data = model_dat, 
              family=binomial(link="logit"),
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(mlm2_1)

mlm2_2 <- glmer(choice ~ vd*nd + cd +  (vd*nd + cd | subject_id), data = model_dat, 
              family=binomial(link="logit"),
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(mlm2_2)

mlm2_3 <- glmer(choice ~ vd*nd*cd +  (vd*nd*cd | subject_id), data = model_dat, 
                family=binomial(link="logit"),
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
summary(mlm2_3)

df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))

model_dat <- df %>% 
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  # filter(subject_id != 1) %>%
  # filter(subject_id != 4) %>%
  # filter(subject_id != 8) %>%
  # filter(subject_id != 24) %>%
  # filter(subject_id != 25) %>%
  filter(subject_id != 27) %>%
  # filter(subject_id != 16) %>%
  filter(subject_id != 26) %>%
  filter(subject_id != 29) %>%
  # filter(subject_id != 30) %>%
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  # group_by(subject_id) %>%
  mutate(
    nd = scale(abs(left_net - right_net)),
    vd = scale(abs(left_rating - right_rating)),
    cd = scale(abs(left_correlation - right_correlation))
  )
# mlm1 <- lmer(log(rt) ~ vd*nd+ (vd*nd| subject_id), data = model_dat)
# summary(mlm1)

mlm2_3 <- glmer(correct ~ vd*nd + (vd*nd | subject_id), data = model_dat, 
                family=binomial(link="logit"),
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
summary(mlm2_3)
# library(brms)
# fit<- brm(correct ~ vd*nd +  (vd*nd | subject_id), data = model_dat, family = "bernoulli", cores = 10)
# summary(fit)

# library(ggeffects)
# plot(ggeffects::ggpredict(fit, terms = c("nd [all]", "vd[-1 ,0, 1]")))+
#   labs(title = "interaction between network strength difference",
#        x = "absolute value difference",
#        color = "absolute network difference",
#        y = "Acc")+
#   theme_classic()+
#   geom_hline(yintercept = .5, linetype = "dashed")

# plot(ggeffects::ggpredict(mlm2, terms = c("nd [all]", "vd[-1 ,0, 1]")))+
#   labs(title = "main effect of average clustering coefficient",
#        x = "network difference",
#        color = "value difference",
#        y = "Pr(left)")+
#   theme_classic()+
#   geom_hline(yintercept = .5, linetype = "dashed")
# 
# plot(ggeffects::ggpredict(mlm2, terms = c("nd [all]", "vd[-1 ,0, 1]")))+
#   labs(title = "main effect of clossness",
#        x = "network difference",
#        color = "value difference",
#        y = "Pr(left)")+
#   theme_classic()+
#   geom_hline(yintercept = .5, linetype = "dashed")
# plot(ggpredict(mlm1, terms = c("vd [all]", "nd[-2, -1, 0 ,1, 2]"), ci.lvl = NA))+
#   labs(title = "",
#        x = "value difference",
#        color = "network difference",
#        y = "rt")
# plot(ggpredict(mlm1, terms = c("nd [all]", "vd[-2, -1, 0 ,1, 2]"), ci.lvl = NA))+
#   labs(title = "",
#        x = "value difference",
#        color = "network difference",
#        y = "rt")
# 
# plot(ggpredict(mlm1, terms = c("vd [all]", "nd[-1, 0 , 1]", "eq", "cluster_condition"), ci.lvl = NA))+
#   labs(title = "",
#        x = "value difference",
#        color = "network difference",
#        y = "rt")
# 
# plot(ggeffects::ggpredict(mlm1, terms = c("vd[all]","eq","cluster_condition", "nd[0]"), ci.lvl = NA))+
#   labs(title = "sets with different degree distributions take longer to choose between",
#        y = "RT(ms)",
#        color = "cluster compatibility",
#        x = "value difference (left - right)")+
#   theme_classic()
# 
# plot(ggeffects::ggpredict(mlm1, terms = c("nd[all]","eq","cluster_condition", "vd[0]"), ci.lvl = NA))+
#   labs(title = "",
#        y = "RT(ms)",
#        color = "cluster compatibility",
#        x = "network difference (left - right)")+
#   theme_classic()

#check correlations
df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 8) %>% 
  filter(subject_id != 9) %>% 
  filter(subject_id != 13) %>% 
  filter(subject_id != 4) %>% 
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(vd = left_rating - right_rating,
         nd = left_net - right_net, 
         cd = left_correlation - right_correlation
         ) %>% 
  select(vd, nd, cd) %>% 
  correlation::correlation()

##degree (strength)
  #we find an interaction with value difference
  
#Eigenvector Centrality 
#measures the transitive influence of nodes. 
#Relationships originating from high-scoring nodes contribute more to the score of a node than connections from low-scoring nodes.
#A high eigenvector score means that a node is connected to many nodes who themselves have high scores.
#we find an inverse shape in the RT distribution so for the most extreme differences
#in groups of options people take longer

#weighted_transitivity (clustering)
#we find a main effect of network difference

#closeness as well


#####rating distributions

subject_ratings %>% as.data.frame() %>% 
  filter(subject_idx != 1) %>%
  filter(subject_idx != 4) %>%
  filter(subject_idx != 8) %>%
  filter(subject_idx != 24) %>%
  filter(subject_idx != 25) %>%
  filter(subject_idx != 27) %>%
  mutate(condition = factor(condition)) %>% 
  ggplot(aes(sum_rating, fill = condition)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1) +
  geom_density(lwd = 1, alpha = 0.25)+
  facet_grid(~condition)+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()

subject_ratings %>% as.data.frame() %>% 
  filter(subject_idx != 1) %>%
  filter(subject_idx != 4) %>%
  filter(subject_idx != 8) %>%
  filter(subject_idx != 24) %>%
  filter(subject_idx != 25) %>%
  filter(subject_idx != 27) %>%
  mutate(condition = factor(condition)) %>% 
  ggplot(aes(net_stat, fill = condition)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, binwidth = .08) +
  geom_density(lwd = 1, alpha = 0.25)+
  facet_grid(~condition)+
  scale_fill_brewer(palette = "Set1")


rp1 <- subject_ratings %>% as.data.frame() %>% 
  filter(subject_idx != 1) %>%
  filter(subject_idx != 4) %>%
  filter(subject_idx != 8) %>%
  filter(subject_idx != 24) %>%
  filter(subject_idx != 25) %>%
  filter(subject_idx != 27) %>%
  mutate(condition = factor(condition)) %>% 
  ggplot(aes(x = condition, y = sum_rating, fill = condition)) +
  geom_violin(trim = FALSE,
              alpha = 0.8) +
  geom_boxplot(width = 0.07)+
  theme_classic()+
  labs(title = "distribution of sum ratings for stimuli per condition",
       y = "Sum Value")+
  scale_fill_brewer(palette = "Set1")+
  scale_x_discrete("Stimuli Groups",
                   labels = c(
                     "1" = "Clusters 1,5,6,7",
                     "2" = "Clusters 2,3",
                     "3" = "Without high degree items",
                     "4" = "Without low degree items"
                   ))+
  theme(legend.position="none")

rp2 <- subject_ratings %>% as.data.frame() %>% 
  filter(subject_idx != 1) %>%
  filter(subject_idx != 4) %>%
  filter(subject_idx != 8) %>%
  filter(subject_idx != 24) %>%
  filter(subject_idx != 25) %>%
  filter(subject_idx != 27) %>%
  mutate(condition = factor(condition)) %>% 
  ggplot(aes(x = condition, y = net_stat, fill = condition)) +
  geom_violin(trim = FALSE,
              alpha = 0.8) +
  geom_boxplot(width = 0.07)+
  theme_classic()+
  labs(title = "distribution of centrality measure for stimuli per condition",
       y = "Sum Centrality")+
  scale_fill_brewer(palette = "Set1")+
  scale_x_discrete("Stimuli Groups",
                   labels = c(
                     "1" = "Clusters 1,5,6,7",
                     "2" = "Clusters 2,3",
                     "3" = "Without high degree items",
                     "4" = "Without low degree items"
                   ))+
  theme(legend.position="none")

rp1/rp2

mean(as.data.frame(subject_ratings)$sum_rating)
subject_ratings %>% as.data.frame() %>% 
  filter(subject_idx != 1) %>%
  filter(subject_idx != 4) %>%
  filter(subject_idx != 8) %>%
  filter(subject_idx != 24) %>%
  filter(subject_idx != 25) %>%
  filter(subject_idx != 27) %>%
  mutate(stimuli = factor(stimuli)) %>% 
  ggplot(aes(x = stimuli, y = sum_rating)) +
  geom_boxplot()+
  theme_classic()+
  theme(legend.position="none")+
  geom_hline(yintercept = 316.3543, size = 1.5, linetype = "dashed")+
  labs(title = "distribution of ratings for stimuli per item",
       y = "Rating")

subject_ratings %>% as.data.frame() %>% 
  filter(subject_idx != 1) %>%
  filter(subject_idx != 4) %>%
  filter(subject_idx != 8) %>%
  filter(subject_idx != 24) %>%
  filter(subject_idx != 25) %>%
  filter(subject_idx != 27) %>%
  ggplot(aes(x = stimuli, y = net_stat, color = factor(condition))) +
  geom_point()+
  theme_classic()+
  theme(legend.position="none")



##looking at the rating data we have so far, how does the network compare?
subject_individual_ratings <- as.data.frame(subject_individual_ratings)
subject_individual_ratings$subject_idx <- as.numeric(subject_individual_ratings$subject_idx)
subject_individual_ratings$response <- as.numeric(subject_individual_ratings$response)
subject_individual_ratings$Image <- as.numeric(subject_individual_ratings$Image)
subject_individual_ratings$Item <- as.numeric(subject_individual_ratings$Item)
subject_individual_ratings$Name <- as.factor(subject_individual_ratings$Name)

TEST <- subject_individual_ratings %>% 
  select(response,Name) %>% 
  pivot_wider(names_from = Name, values_from = response) %>% 
  unnest(everything())
#make the order of the names the same
TEST<-TEST[names(lee_2021_rating1)]
# TEST <- TEST[1,]
#make sure the matrix is near positive definite
# cor_x1 <- SemNeT::similarity(TEST, method = "cor")
cor_x1 <- cor(TEST)

cor_x1 <- matrix(nearPD(cor_x1, corr=TRUE, maxit = 500 )$mat, ncol = 60)
cor_x1 <- (cor_x1 + t(cor_x1)) / 2 # make symmetric


test_net <-EGAnet::EGA(cor_x1, n = 30, model = "glasso", algorithm = "walktrap",corr = "pearson")


cor(new_TEST)
myCov <- cov(TEST)
round(myCov, 2)
## check whether any correlations are perfect (i.e., collinearity)
myCor <- cov2cor(myCov)
noDiag <- myCor
diag(noDiag) <- 0
any(noDiag == 1)
## if not, check for multicollinearity (i.e., is one variable a linear combination of 2+ variables?)
det(myCov) < 0
## or
any(eigen(myCov)$values < 0)

dput(which(eigen(myCov)$values < 0))
#remove the items with the negative eigen values (the ones that contain redundancy)
new_TEST <- TEST[, -c(45:60)]
test_net$Methods
test_net <-EGAnet::EGA(TEST, n = 30, model = "glasso", algorithm = "walktrap",
                       corr = "pearson", 
                       model.args = list(lambda.min.ratio = 0.1,
                                         nlambda = 300,
                                         gamma = 0.05))

boot_test <- EGAnet::bootEGA(TEST,
                             plot.type = "qgraph",
                             iter = 1000,
                             type = "resampling",
                             corr = "pearson",
                             n = 30,
                             model = "glasso",
                             algorithm = "walktrap",
                             ncores = 10, typicalStructure = T,
                             model.args = list(lambda.min.ratio = 0.1,
                                               nlambda = 300)) # gamma = 0.05

boot_test[["plot.typical.ega"]][["layers"]][[6]] <- NULL
boot_test$plot.typical.ega
A <- boot_test[["typicalGraph"]][["graph"]]
# get clusters
dimattributes <- boot_test[["typicalGraph"]][["wc"]]
# create igraph object
g_2 <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
# add decorate attributes
V(g_2)$snack_type <- dimattributes

#look at the relationship between the two (doug and my sample graphs)
library(NetworkComparisonTest)

g_1 <- g #name data from netork one above

#note that this might be the wrong data to feed the models,
#you might need to feed the function the raw data for each
nw1 <- igraph::as_adjacency_matrix(g_1, sparse = F, attr = "weight")
nw2 <- igraph::as_adjacency_matrix(g_2, sparse = F, attr = "weight")
#note we can create custom estimators! look into this

# Res_1 <- NCT(nw1, nw2, it=1000, gamma = 0.05)

# Res_1 <- NCT(lee_2021_rating1, TEST, it=100, gamma = 0.05,
#              test.centrality=T,
#              centrality = "strength")
# 
# plot(Res_1, what="network")
# plot(Res_1, what="strength")

#> Comparison between the two groups were performed using the NetworkComparisonTest package (NCT; van Borkulo et al., 2017; versão 2.2.1) 
#> with a permutation seed value of ‘123’. Based on 1000 permutations, we investigated 
#> network invariance (possible edge weight differences) and 
#> global strength invariance (possible difference on the absolute sum of network edge weights).
#>
#>From NCT analyses, we observed that networks seem to be the same for the two groups M:  0.58037 , p-value 0.08, and S:  1.097621 with p = 0.72 

graph_sample <- graph_from_adjacency_matrix(boot_test$typicalGraph$graph,
                                             "undirected",
                                             weighted = TRUE
)
graph_lee <- graph_from_adjacency_matrix(ega_res$typicalGraph$graph,
                                               "undirected",
                                               weighted = TRUE
)

value_sample_data <- tibble(items = V(graph_sample)$name) %>%
  left_join(boot_test$typicalGraph$typical.dim.variables,
            by = "items"
  )
value_lee_data <- tibble(items = V(graph_lee)$name) %>%
  left_join(ega_res$typicalGraph$typical.dim.variables,
            by = "items"
  )
L <-   qgraph::averageLayout(graph_sample, graph_lee)

#how to get correspondence between the two ?
clusters <- left_join(value_lee_data, value_sample_data, by = "items")

# value_sample_data$dimension[value_sample_data$dimension == 1] <- 3
# value_sample_data$dimension[value_sample_data$dimension == 2] <- 4
# value_sample_data$dimension[value_sample_data$dimension == 3] <- 2
# value_sample_data$dimension[value_sample_data$dimension == 4] <- 3
# value_sample_data$dimension[value_sample_data$dimension == 5] <- 6
# value_sample_data$dimension[value_sample_data$dimension == 6] <- 1

V(graph_sample)$color <- value_sample_data$dimension
V(graph_lee)$color <- value_lee_data$dimension

graph_lee$palette <- RColorBrewer::brewer.pal(n = length(unique(value_lee_data$dimension)), name = "Set2")
graph_sample$palette <- RColorBrewer::brewer.pal(n = length(unique(value_lee_data$dimension)), name = "Set1")


E(graph_sample)$color[E(graph_sample)$weight > 0] <- "forestgreen"
E(graph_sample)$color[E(graph_sample)$weight < 0] <- "red2"

E(graph_lee)$color[E(graph_lee)$weight > 0] <- "forestgreen"
E(graph_lee)$color[E(graph_lee)$weight < 0] <- "red2"

par(mfrow = c(1, 2)) # set the plotting area into a 1*3 array
plot(graph_lee,
     layout = L,
     margin = .0,
     vertex.label = V(graph_lee)$name,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.label.dist = .5,
     vertex.size = 6,
     vertex.label.family = "Times",
     main = "Lee & Coricelli: N = 267",
     edge.width = abs(E(graph_lee)$weight) * 5,
     # mark.groups = value_lee_data$dimension
)
plot(graph_sample,
     layout = L,
     margin = .0,
     vertex.label = V(graph_sample)$name,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.label.dist = .5,
     vertex.size = 6,
     vertex.label.family = "Times",
     main = "sample: N = 30",
     edge.width = abs(E(graph_sample)$weight) * 5,
     # mark.groups = value_sample_data$dimension
)


#individual network (johns idea)

par(mfrow = c(2,2), mar = c(0,0,1,0)) # set the plotting area into a 1*3 array
# par(mfrow=c(5,5), mar=rep(0,0,0,0))   # plot four figures - 2 rows, 2 columns
library(jpeg)

# par(mfrow=c(4,4), mar=c(1,1,1,1))

par(mfrow=c(1,2))

nodes_temp <- temp[foods_in_image$rowid]


for (pp in 1:30){
  
graph_individual <- differenceNet(TEST, subject = pp, cut.off = T)

V(graph_individual)$color <- value_lee_data$dimension

degree <- degree(graph_individual, mode="all")
# 
# deg.dist <- degree_distribution(graph_individual, cumulative=T, mode="all")
# print(plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",
#       xlab="Degree", ylab="Cumulative Frequency",main =  paste0("individual: ", pp)))
# 

# E(graph_individual)$color[E(graph_individual)$weight > 0] <- "forestgreen"
# E(graph_individual)$color[E(graph_individual)$weight < 0] <- "red2"

# E(graph_individual)$weight <- 2**((E(graph_individual)$weight - min(E(graph_individual)$weight)) / diff(range(E(graph_individual)$weight)))

#how to plot the images if you wanted to try that
# adj_temp <- igraph::as_adjacency_matrix(graph_individual, sparse = F, attr = "weight")
# qgraph::qgraph(adj_temp, 
#                layout = layout_nicely(graph_individual),
#                palette = "ggplot2",
#                labels = F,
#                images = nodes_temp,
#                edge.width = abs(E(graph_individual)$weight) * .3
#                )
               
print(plot(graph_individual,
     layout = layout_nicely(graph_individual),
     # layout = layout_components(graph_individual),
     # layout = layout_with_fr(graph_individual),
     # layout = layout_with_mds(graph_individual),
     # layout = layout.circle(graph_individual),
     margin = .0,
     vertex.label = V(graph_individual)$name,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.label.dist = .5,
     # vertex.shapes = nodes_temp,
     # vertex.size = deg*.2,
     vertex.size = as.numeric(TEST[pp,]/10),
     vertex.label.family = "Times",
     main = paste0("individual: ", pp),
     edge.width = abs(E(graph_individual)$weight) * .4,
     mark.groups = communities(cluster_walktrap(graph_individual, steps = 4))
     # mark.groups =communities(cluster_spinglass(graph_individual, implementation = "neg"))
     # mark.groups =communities(cluster_louvain(graph_individual))
     
     ))
}


#make the example snack networks
#use the code below to build the examples:

remove_snacks <- net_degree[!net_degree$Name %in% dput(res[,67]),]$Name
V(graph_individual)$color <- value_lee_data$dimension
g3 <- delete_vertices(graph_individual, remove_snacks)
degree(g3)
l4 <- layout_in_circle(g3)
# l4 <- layout_nicely(g3)

plot(g3,
     layout = l4,
     margin = .0,
     vertex.label.color = "black",
     vertex.label = V(g3)$name,
     vertex.label.cex = 1.3,
     vertex.size = 10,
     vertex.label.family = "Times",
     edge.width = abs(E(graph_individual)$weight) * .8
)

#example two 
remove_snacks <- net_degree[!net_degree$Name %in% dput(res[,35]),]$Name
V(graph_individual)$color <- value_lee_data$dimension
g3 <- delete_vertices(graph_individual, remove_snacks)
l4 <- layout_in_circle(g3)
# l4 <- layout_nicely(g3)

degree(g3)
plot(g3,
     layout = l4,
     vertex.label = V(g3)$name,
     vertex.label.color = "black",
     vertex.label.cex = 1.3,
     vertex.size = 10,
     vertex.label.family = "Times",
     edge.width = abs(E(graph_individual)$weight) * .8
)

