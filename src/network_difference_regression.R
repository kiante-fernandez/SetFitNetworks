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

library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4
library(lmerTest) # Tests in Linear Mixed Effects Models

library(gghalves) # Compose Half-Half Plots Using Your Favorite Geoms
library(ggforce) # Accelerating 'ggplot2'
library(ggdist) # Visualizations of Distributions and Uncertainty
library(patchwork) # The Composer of Plots

library(brms)

library(modelsummary)
library(kableExtra)
library(gt)

#load helper functions
source(here::here("src", "utils.R"))
# source(here::here("src", "utils_plotting.R"))

estimate_mlms <- function(df, outcome = "choice") {
  # estimate the mixed effect regressions using lme4 package
  if (outcome == "choice") {
    df_temp = create_dataset(df, type = "choice")
    # base model
    model1  = glmer(choice ~ zleft_rating + zright_rating + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    # add network difference
    model2 <- glmer(choice ~ zleft_rating + zright_rating + zleft_net + zright_net + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    
    model3 <- glmer(choice ~ (zleft_rating*zleft_net) + (zright_rating*zright_net) + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    
    return(list(model1, model2, model3)) 
    
  } else if (outcome == "correct") {
    # base model
    df_temp = create_dataset(df, type = "correct/rt")
    model1 <- glmer(correct ~ vd + ov + (1| subject_id), data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add network difference
    model2 <- glmer(correct ~ vd + ov + nd + (1 | subject_id), data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    return(list(model1, model2)) 
    
  } else if (outcome == "rt") {
    df_temp = create_dataset(df, type = "correct/rt")
    # df = df[df$correct == 1,] #check only correct
    
    # base model
    model1 <- lmer(log(rt) ~ vd + ov + (1 + vd + ov| subject_id), data = df_temp, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add network difference
    model2 <- lmer(log(rt) ~ vd + ov + nd + (1 + vd + ov | subject_id), data = df_temp, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))    # add overall network
    
    return(list(model1, model2)) 
  }
}

estimate_brms <- function(df, outcome = "choice") {
  #
  # TODO just create a folder for each network statistic, then add an argument that places each model in what ever name you write
  #     create a error too. if the folder name does not exist in the directory then throw an error and don't run the models 'could not find folder to save models'
  if (outcome == "choice") {
    df_temp = create_dataset(df, type = "choice")
    
    # base model
    model1 <- brm(choice ~ zleft_rating + zright_rating + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_choice01"))
    # add network difference
    model2 <- brm(choice ~ zleft_rating + zright_rating + zleft_net + zright_net + (1 + zleft_rating + zright_rating + zleft_net + zright_net | subject_id), data = df_temp, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_choice02"))
    #interactions
    model3 <- brm(choice ~ (zleft_rating*zleft_net) + (zright_rating*zright_net) + (1 + zleft_rating + zright_rating + zleft_net + zright_net | subject_id), data = df_temp, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_choice03"))
    
    return(list(model1, model2, model3)) 
    
  } else if (outcome == "correct") {
    # the coded as correct models (which take the absolute value for the regressors)
    model1 <- brm(correct ~ vd + ov + (vd + ov | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_correct01"))
    # add network difference
    model2 <- brm(correct ~ vd + ov + nd + (vd + ov + nd| subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_correct02A"))

    return(list(model1, model2)) 
    
  } else if (outcome == "rt") {
    df_temp = create_dataset(df, type = "correct/rt")
    
    # the coded as response time models (which take the absolute value for the regressors)
    model1 <- brm(log(rt) ~ vd + ov + (vd + ov| subject_id), data = df_temp, cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_rt01"))
    # add network difference
    model2 <- brm(log(rt) ~ vd + ov + nd + (vd + ov + nd | subject_id), data = df_temp, cores = 4, iter = 10000, file = here::here("fits", "exp_1_fit_rt02"))

    return(list(model1, model2)) 
    
  }

}

######
# calculate a bunch of network measures to look at relationship to stuff
source("exploratory_graph_analysis.R")
# source('fernandez_rating_network.R') #load the EGA from the new rating data

# #look at the emprical network res
A <- ega_res[["EGA"]][["network"]]
# dimattributes <- ega_res[["EGA"]][["wc"]]
dimattributes <- ega_res[["typicalGraph"]][["wc"]]
g <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
V(g)$snack_type <- dimattributes # let the communities belong to the bootEGA, but the weights to the empirical


net_degree <- calculate_net_stats(g)

##### loading the data#####
df <- organize_group_data(experiment = 1, net_stat = "modularity")

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
    filter(!rt <= 300) %>% # response times cutoffs
    filter(!rt >= 9000) %>%
    mutate(vd = left_rating - right_rating) %>%
    mutate(nd = left_net - right_net) %>%
    mutate(sd = left_sim - right_sim)
  
  if (subject_idx %in% c(9, 13, 30)) { # remove that break on the choose min /max bc they always follow (each still have sig vd)
    p_values[[subject_idx]] <- NA
    next
  }
  
  # temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating + left_net + right_net + choose_max + choose_min, family = binomial, data = temp_df))
  temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating  + choose_max + choose_min, family = binomial, data = temp_df))
  
  # temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating + left_net + right_net, family = binomial, data = temp_df))
  
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

print(paste0("############### Subject data exlclusions:", ((length(as.numeric(na.omit(p_values)))) / 30)*100,"%  ###############"))

as.numeric(na.omit(p_values))

exlusions <- function(df) {
  # function for data exclusions
  temp <- df %>%
    filter(!subject_id %in% c(8,9,16)) %>% #comment out for no exclusions (subejct 13?)
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

# net_stats <- c("strength", "eigen", "edge_density", "modularity")
# net_stats <- c("strength","betweenness","closeness","weighted_transitivity","eigen","efficiency", "edge_density", "modularity")
# net_stats <- c("strength","eigen","efficiency", "edge_density", "modularity")

# net_stats <- c("weighted_transitivity","edge_density", "modularity", "conductance")
net_stats <- c( "modularity")

# net_idx <- 1
for (net_idx in 1:length(net_stats)) {
  print(paste0("############### ", net_stats[[net_idx]], " ###############"))
  df <- organize_group_data(experiment = 1, net_stat = net_stats[[net_idx]])
  # test <- df[(df$left_net != 0) | (df$right_net != 0),] #eleminate trials where both are zero
  
  # #### data analysis
  print(paste0("############### CHOICE ###############"))
  models_choice <- estimate_mlms(df, outcome = "choice")
  # models_choice <- estimate_brms(df, outcome = "choice") #bayes
  
  # print(performance::compare_performance(models_choice, rank = TRUE, metrics = c("AIC", "BIC")))
  # print(performance::compare_performance(models_choice, rank = TRUE, metrics = c("WAIC","LOOIC"))) #bayes
  
  # print(parameters::compare_models(models_choice))
  print(parameters::compare_models(models_choice, style = "ci_p"))
  # 
  mp <- modelplot(models_choice, coef_omit = "Interc") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Coefficients",y = "Terms",
         title = paste0("Choice: ",net_stats[[net_idx]])
    ) +
    theme_classic() +
    scale_color_brewer(palette = "Set1")
  print(mp)
  
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
  models_rt <- estimate_brms(df, outcome = "rt") #bayes versions
  
  # print(performance::compare_performance(models_rt, rank = TRUE))
  # print(parameters::compare_models(models_rt,  style = "ci_p"))
  # # print(parameters::compare_models(models_choice))
  # 
  # mp <- modelplot(models_rt, coef_omit = "Interc") +
  #   geom_vline(xintercept = 0, linetype = "dashed") +
  #   labs(x = "Coefficients",y = "Terms",
  #        title = paste0("RT: ",net_stats[[net_idx]])
  #   ) +
  #   theme_classic() +
  #   scale_color_brewer(palette = "Set1")
  # print(mp)
  
  # generate tables
  # generate_table(models_choice, type = "exp_1_choice", net_stat = net_stats[[net_idx]], save = T)
  # generate_table(models_correct, type = "exp_1_correct", net_stat = net_stats[[net_idx]], save = T)
  # generate_table(models_rt, type = "exp_1_rt", net_stat = net_stats[[net_idx]], save = T)
  
}
# (plts[[1]] + plts[[2]])/(plts[[3]] + plts[[4]])

# knitr::kable(bayestestR::sexit(exp_1_fit_choice03), digits = 2)
# bayestestR::sexit(exp_1_fit_rt02, significant = 0.01)
# bayestestR::sexit(exp_1_fit_choice03, significant = 0.01)

