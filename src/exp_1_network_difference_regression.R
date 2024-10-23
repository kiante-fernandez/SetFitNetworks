# network_difference_regression_analysis.R - algorithm for selecting sub graphs from preference network

# Copyright (C) 2023 Kianté Fernandez, <kiantefernan@gmail.com>
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
# 2022/10/28      Kianté  Fernandez                     coded up version one
# 2023/01/09      Kianté  Fernandez                     refactored

# Libraries
library(purrr) # Functional Programming Tools
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(jsonlite) # A Simple and Robust JSON Parser and Generator for R

library(gghalves) # Compose Half-Half Plots Using Your Favorite Geoms
library(ggforce) # Accelerating 'ggplot2'
library(ggdist) # Visualizations of Distributions and Uncertainty
library(patchwork) # The Composer of Plots

library(brms)

library(modelsummary)
library(kableExtra)
library(gt)

library(cmdstanr)

#load helper functions
source(here::here("src", "utils.R"))

estimate_brms <- function(df, outcome = "choice") {
  #
  # TODO just create a folder for each network statistic, then add an argument that places each model in what ever name you write
  #     create a error too. if the folder name does not exist in the directory then throw an error and don't run the models 'could not find folder to save models'
  if (outcome == "choice") {
    df_temp = create_dataset(df, type = "choice")
    
    # base model
    # model1 <- brm(choice ~ zleft_rating + zright_rating + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_choice01"))
    # add network difference
    # model2 <- brm(choice ~ zleft_rating + zright_rating + zleft_net + zright_net + (1 + zleft_rating + zright_rating + zleft_net + zright_net | subject_id), data = df_temp, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_choice02"))
    #interactions
    model3 <- brm(choice ~ (zleft_rating*zleft_net) + (zright_rating*zright_net) + (1 + zleft_rating + zright_rating + zleft_net + zright_net | subject_id), data = df_temp, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_choice03"))
    
    # return(list(model1, model2, model3)) 
    return(list(model3)) 
    
    
  } else if (outcome == "correct") {
    # the coded as correct models (which take the absolute value for the regressors)
    model1 <- brm(correct ~ vd + ov + (vd + ov | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_correct01"))
    # add network difference
    model2 <- brm(correct ~ vd + ov + nd + (vd + ov + nd| subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_correct02A"))

    return(list(model1, model2)) 
    
  } else if (outcome == "rt") {
    df_temp = create_dataset(df, type = "correct/rt")
    
    # the coded as response time models (which take the absolute value for the regressors)
    # model1 <- brm(log(rt) ~ vd + ov + (vd + ov| subject_id), data = df_temp, cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_rt01"))
    # add network difference
    model2 <- brm(log(rt) ~ vd + ov + nd + (vd + ov + nd | subject_id), data = df_temp, cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_rt02"))

    # return(list(model1, model2)) 
    return(list(model1))
    
  }

}

######
# calculate a bunch of network measures to look at relationship to stuff
source("exploratory_graph_analysis.R")
# source('fernandez_rating_network.R') #load the EGA from the new rating data

# # #look at the emprical network res
# A <- ega_res[["EGA"]][["network"]]
# # dimattributes <- ega_res[["EGA"]][["wc"]]
# dimattributes <- ega_res[["typicalGraph"]][["wc"]]
# g <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
# V(g)$snack_type <- dimattributes # let the communities belong to the bootEGA, but the weights to the empirical

# dimattributes <- V(g)$snack_type
# g <- graph_from_adjacency_matrix(SemNeT::similarity(lee_2021_rating1, method = "cor"), "undirected", weighted = TRUE,diag = F)
# # g <- graph_from_adjacency_matrix(SemNeT::TMFG(SemNeT::similarity(lee_2021_rating1, method = "cor")), "undirected", weighted = TRUE,diag = F)
# V(g)$snack_type <- dimattributes

# g <-graph_from_adjacency_matrix(net_sim_bert,"undirected",weighted = TRUE,diag = F)
# clp <- cluster_fast_greedy(g)
# V(g)$snack_type <- clp$membership

net_degree <- calculate_net_stats(g)


##### loading the data#####
# source(here::here("src", "organize_group_data_v2.R"))

# df <- organize_group_data(experiment = 1, net_stat = "modularity")
df <- organize_group_data(experiment = 1)

rt_exclude_pct <- vector(mode = "numeric", length = 30)

for (subject_idx in 1:length(unique(df$subject_id))) {
  temp_df <- df %>%
    filter(subject_id == subject_idx) %>%
    mutate(trial = 1:99) %>%
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    filter(!rt <= 250) %>% # response times cutoffs
    filter(!rt >= 9000) %>%
    summarise(pct_excluded = (99 - n()) / 99)
  
  rt_exclude_pct[[subject_idx]] <- temp_df$pct_excluded
  
  if (temp_df$pct_excluded > 0.50) {
    print(paste0("######## subject: ", subject_idx, " #######"))
    print(paste0("######## percent trials excluded: ", temp_df$pct_excluded, " #######"))
  }
}
mean(rt_exclude_pct)

## value difference exclusion
p_values <- vector(mode = "numeric", length = 30)
set_strategy_winner <- vector(mode = "numeric", length = 30)

# subject_idx = 1
for (subject_idx in 1:30) {
  
  temp_df <- df %>%
    filter(subject_id == subject_idx) %>%
    mutate(trial = 1:99) %>%
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    filter(!rt <= 250) %>% # response times cutoffs
    filter(!rt >= 9000) %>%
    mutate(vd = left_rating - right_rating) %>%
    mutate(sd = left_sim - right_sim) %>% 
    mutate(left_range =  left_MAX - left_MIN,
           right_range =  right_MAX - right_MIN)
  
  if (subject_idx %in% c(9, 13, 30)) { # remove that break on the choose min /max bc they always follow (each still have sig vd)
    p_values[[subject_idx]] <- NA
    next
  }
  
  # temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating + left_net + right_net + choose_max + choose_min, family = binomial, data = temp_df))
  temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating  + choose_max + choose_min, family = binomial, data = temp_df))
  # 0. selecting using the average value
  temp_res0 <- glm(choice ~ left_rating + right_rating, family = binomial, data = temp_df)
  # 1.	Selecting the set with maximum
  temp_res1 <- glm(choice ~ choose_max, family = binomial, data = temp_df)
  # 2.	Selecting the set without the minimum
  temp_res2 <- glm(choice ~ choose_min, family = binomial, data = temp_df)
  # 3.	Selecting the set with range
  temp_res3 <- glm(choice ~ left_range + right_range, family = binomial, data = temp_df)

  xx <-performance::compare_performance(temp_res0,temp_res1,temp_res2,temp_res3, rank = TRUE, metrics = c("AIC","AICc","BIC","RMSE","R2"))
  print(paste0("############### Subject data:",subject_idx,"  ###############"))
  print(xx[,c(1,8)])
  set_strategy_winner[[subject_idx]] <- xx[1,1]
  
  temp_res$p.value <- round(temp_res$p.value, 2)
  
  if ((temp_res[2, 5][[1]] > 0.05) & (temp_res[3, 5][[1]] > 0.05) & (temp_res[4, 5][[1]] > 0.05) & (temp_res[5, 5][[1]] > 0.05)) { # check p-value (prereg -- 0.05. check robustness across values)
    # if ((temp_res[2, 5][[1]] > 0.05) & (temp_res[3, 5][[1]] > 0.05)) { # check p-value (prereg -- 0.05. check robustness across values)
      
    p_values[[subject_idx]] <- unique(temp_df$subject_id)
    # print(temp_res)
  } else {
    (p_values[[subject_idx]] <- NA)
  }
  # print(temp_res)
}

# print(paste0("############### Subject data exlclusions:", ((length(as.numeric(na.omit(p_values)))) / 30)*100,"%  ###############"))

as.numeric(na.omit(p_values))

length(set_strategy_winner[is.na(as.numeric(p_values))])
length(set_strategy_winner)
set_strategy_winner <- set_strategy_winner[is.na(as.numeric(p_values))]


exlusions <- function(df) {
  # function for data exclusions
  temp <- df %>%
    # filter(!subject_id %in% c(8,9,16)) %>% #comment out for no exclusions (subejct 13?)
    filter(!subject_id %in% as.numeric(na.omit(p_values))) %>% #comment out for no exclusions
    group_by(subject_id) %>% # response times
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    ungroup() %>%
    filter(!rt <= 250) %>% # response times cutoffs
    filter(!rt >= 9000)
  return(temp)
}

for_save <- df %>% exlusions()
# write_csv(for_save, "data/ISDN_poster_exp1.csv")

# net_stats <- c("strength", "eigen", "edge_density", "modularity")
net_stats <- c("strength","betweenness","closeness","weighted_transitivity","eigen", "edge_density", "modularity","pca1", "pca2", "set_pca1", "set_pca2")

res_netstats <- vector(mode = "list", length = length(net_stats))
# net_idx = 8
for (net_idx in 1:length(net_stats)) {
  # if (net_idx %in% c(6)) {
  #   next
  # }
  print(paste0("############### ", net_stats[[net_idx]], " ###############"))
  # df <- organize_group_data(experiment = 1, net_stat = net_stats[[net_idx]])
  # test <- df[(df$left_net != 0) | (df$right_net != 0),] #eleminate trials where both are zero
  #now the loop just finds the relevant stat and renames it rather than recalculating
  
  df$left_net1 <-   select(df,contains(net_stats[[net_idx]]))[[1]]
  df$right_net1 <-   select(df,contains(net_stats[[net_idx]]))[[2]]
  
  df$left_net2 <-   select(df,contains(net_stats[[9]]))[[1]] #pc2
  df$right_net2 <-   select(df,contains(net_stats[[9]]))[[2]] #pc2
  
  # #### data analysis
  print(paste0("############### CHOICE ###############"))
  
  df_temp = create_dataset(df, type = "choice")
  # models_choice <- brm(choice ~ zleft_rating*(zleft_net1 + zleft_net2) + zright_rating*(zright_net1 + zright_net2) +
  #                        (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 +  zleft_net2 + zright_net2 | subject_id), 
  #                      data = df_temp, family = "bernoulli", iter = 10000, 
  #                      chains = 4, cores = 4,
  #                      file = here::here("fits", paste0(net_stats[[net_idx]], "_exp_1_fit_choice03")))
  models_choice <- brm(choice ~ zleft_rating*(zleft_net1) + zright_rating*(zright_net1) +
                         (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1| subject_id),
                       data = df_temp, family = "bernoulli", iter = 10000,
                       chains = 4, cores = 4,
                       file = here::here("fits", paste0(net_stats[[net_idx]], "_exp_1_fit_choice03")))
                       # file_refit =   getOption("brms.file_refit", "always"))
  
  # models_choice1 <- brm(choice ~ zleft_rating + zright_rating +
  #                 (1 + zleft_rating + zright_rating| subject_id),
  #               data = df_temp, family = "bernoulli", iter = 10000,
  #               chains = 4, cores = 4, backend = "cmdstanr")
  # models_choice2 <- brm(choice ~ zleft_rating*zleft_net1 + zright_rating*zright_net1 +
  #                         (1 + zleft_rating + zleft_net1 + zright_rating + zright_net1| subject_id),
  #                       data = df_temp, family = "bernoulli", iter = 10000,
  #                       chains = 4, cores = 4)
  # models_choice3 <- brm(choice ~ zleft_rating*zleft_net2 + zright_rating*zright_net2 +
  #                         (1 + zleft_rating + zleft_net2 + zright_rating + zright_net2| subject_id),
  #                       data = df_temp, family = "bernoulli", iter = 10000,
  #                       chains = 4, cores = 4)
  # models_choice3 <- brm(choice ~ zleft_rating + zleft_net1+ zleft_net2 + zright_rating + zright_net1 + zright_net2+
  #                         (1 + zleft_rating + zleft_net1+ zleft_net2 + zright_rating + zright_net1 + zright_net2| subject_id),
  #                      data = df_temp, family = "bernoulli", iter = 10000,
  #                      chains = 4, cores = 4, backend = "cmdstanr")
  # looR21<-loo_R2(models_choice1)
  # round(median(looR21), 3)
  # looR22<-loo_R2(models_choice2)
  # round(median(looR22), 3)
  # looR23<-loo_R2(models_choice3)
  # round(median(looR23), 3)
  # brms::bayes_R2(models_choice1)
  # brms::bayes_R2(models_choice2)
  # brms::bayes_R2(models_choice3)
  
  # looR2<-loo_R2(models_choice3)
  # round(median(looR2), 2)
  # report::report(models_choice3)
  
  #an additional four percent variance explained from PCA 2 alone. 
  # loo1 <- loo(models_choice1)
  # loo2 <- loo(models_choice2)
  # loo3 <- loo(models_choice3)
  # loo1$estimates
  # loo2$estimates
  
  # loo_compare(loo1, loo2, loo3)
  # models_choice <- estimate_mlms(df_temp, outcome = "choice")
  # models_choice <- estimate_brms(df, outcome = "choice") #bayes
  # library(tidybayes)
  # plot(ggeffects::ggpredict(models_choice3, terms = c("zleft_net2[all]"))) 

  # print(performance::compare_performance(models_choice, rank = TRUE, metrics = c("AIC", "BIC")))
  # # print(performance::compare_performance(models_choice, rank = TRUE, metrics = c("WAIC","LOOIC"))) #bayes
  #
  # print(parameters::compare_models(models_choice))
  # print(parameters::compare_models(models_choice, style = "ci_p"))
  # #
  # mp <- modelplot(models_choice, coef_omit = "Interc") +
  #   geom_vline(xintercept = 0, linetype = "dashed") +
  #   labs(x = "Coefficients",y = "Terms",
  #        title = paste0("Choice: ",net_stats[[net_idx]])
  #   ) +
  #   theme_classic() +
  #   scale_color_brewer(palette = "Set1")
  # print(mp)
  
  # print(paste0("############### CORRECT ###############"))
  # models_correct<- estimate_mlms(df, outcome = "correct")
  # print(performance::compare_performance(models_correct, rank = TRUE, metrics = c("AIC", "BIC", "R2", "RMSE", "LOGLOSS")))
  # print(parameters::compare_models(models_correct,  style = "ci_p"))
  # mp <- modelplot(models_correct, coef_omit = "Interc") +
  #   geom_vline(xintercept = 0, linetype = "dashed") +
  #   labs(x = "Coefficients",y = "Terms",
  #        title = net_stats[[net_idx]]
  #   ) +
  #   theme_classic() +
  #   scale_color_brewer(palette = "Set1")
  # print(mp)
  
  print(paste0("############### RT ###############"))
  # models_rt <- estimate_mlms(df, outcome = "rt")
  # models_rt <- estimate_mlms(df[df$correct == 1,], outcome = "rt") #only ocrrect
  # models_rt <- estimate_brms(df, outcome = "rt") #bayes versions
  
  df_temp = create_dataset(df[df$correct == 1,], type = "correct/rt")
  
  models_rt <- brm(log(rt) ~ vd + ov + nd2 +
                     (vd + ov + nd2  | subject_id),
                   data = df_temp, iter = 10000,
                   chains = 4, cores = 4,
                   file = here::here("fits", paste0(net_stats[[net_idx]], "_exp_1_fit_rt02_correct_only")))
  
  # models_rt <- brm(log(rt) ~ vd + ov + nd1 + nd2 +
  #                    (vd + ov + nd1 + nd2  | subject_id),
  #                  data = df_temp, iter = 10000,
  #                  chains = 4, cores = 4, backend = "cmdstanr", threads = threading(2),
  #                  file = here::here("fits", paste0(net_stats[[net_idx]], "_exp_1_fit_rt02")))
  
  models_rt <- brm(log(rt) ~ vd + ov + nd1 +
                   (vd + ov + nd1  | subject_id), 
                   data = df_temp, iter = 10000, 
                   chains = 4, cores = 4, backend = "cmdstanr",
                   file = here::here("fits", paste0(net_stats[[net_idx]], "_exp_1_fit_rt02")))
                   # file_refit =   getOption("brms.file_refit", "always"))
  
  # print(paste0("############### ", net_stats[[net_idx]], " ###############"))
  # bayestestR::sexit(models_choice)
  # bayestestR::sexit(models_rt)
  
  res_netstats[[net_idx]] <- list(models_choice,models_rt)
  
}
# 
first_elements <- sapply(res_netstats, function(x) x[1])
sec_elements <- sapply(res_netstats, function(x) x[2])

first_elements <- sapply(res_netstats[1:7], function(x) x[1])

sec_elements <- sapply(res_netstats[1:7], function(x) x[2])
# 
map(sec_elements, bayestestR::sexit)
# 
# lapply(first_elements, function(x)  bayestestR::convert_pd_to_p(bayestestR::p_direction(x)$pd))

