# exp2_network_difference_regression_analysis.R - analysis of experiment two
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
# 2022/12/22      Kianté  Fernandez                   coded up version one
# 2022/12/25      Kianté  Fernandez                   moved many functions to utils
# 2022/12/27      Kianté  Fernandez                   created table functions
# 2023/01/20      Kianté  Fernandez                   change the regression output

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

library(brms) # Bayesian Regression Models using 'Stan'

library(modelsummary)
library(kableExtra)
library(gt)

source(here::here("src", "utils.R"))

######
# calculate a bunch of network measures to look at relationship to stuff

source("exploratory_graph_analysis.R")
# source('fernandez_rating_network.R') #load the EGA from the new rating data

#use the empirical network instead of the bootnet one
# A <- ega_res[["EGA"]][["network"]]
# dimattributes <- ega_res[["EGA"]][["wc"]]
# dimattributes <- ega_res[["typicalGraph"]][["wc"]]
# g <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
# V(g)$snack_type <- dimattributes # let the communities belong to the bootEGA, but the weights to the empirical

# dimattributes <- V(g)$snack_type
# g <- graph_from_adjacency_matrix(SemNeT::similarity(lee_2021_rating1, method = "cor"), "undirected", weighted = TRUE,diag = F)
# # g <- graph_from_adjacency_matrix(SemNeT::TMFG(SemNeT::similarity(lee_2021_rating1, method = "cor")), "undirected", weighted = TRUE,diag = F)
# V(g)$snack_type <- dimattributes
g <-graph_from_adjacency_matrix(net_sim_GPT3,"undirected",weighted = TRUE,diag = F)
clp <- cluster_fast_greedy(g)
V(g)$snack_type <- clp$membership

net_degree <- calculate_net_stats(g)

##### loading the data#####
source(here::here("src", "organize_group_data_v2.R"))

# df <- organize_group_data(experiment = 2, net_stat = "modularity")
df <- organize_group_data(experiment = 2)

#trying out the zero out method from the permutation tests?

#View(df[df$correct == 1 & df$correctwt == 0,])
# describe_exclusions <- function(){
#   ###this function will take the RT exclusion below and print the subject were
### 70% of the trails were removed. Thus we will not run the exclusion
### regressions because we don't have enough trials to preform them.

## then it will take the remaining data and run individual level logistic
## regressions with value difference regressed onto choice. It will print the
## not significant subjects and add there names to a list for the exclusion function
# }

### check for response time exclusions (before or after choice?)
rt_exclude_pct <- vector(mode = "numeric", length = 75)

for (subject_idx in 1:length(unique(df$subject_id))) {
  temp_df <- df %>%
    filter(subject_id == subject_idx) %>%
    mutate(trial = 1:100) %>%
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    filter(!rt <= 250) %>% # response times cutoffs
    filter(!rt >= 9000) %>%
    summarise(pct_excluded = (100 - n()) / 100)

  rt_exclude_pct[[subject_idx]] <- temp_df$pct_excluded

  if (temp_df$pct_excluded > 0.40) {
    print(paste0("######## subject: ", subject_idx, " #######"))
    print(paste0("######## percent trials excluded: ", temp_df$pct_excluded, " #######"))
  }
}
mean(rt_exclude_pct)

## value difference exclusion
p_values <- vector(mode = "numeric", length = 75)
#subject_idx = 1
for (subject_idx in 1:75) {
  
  temp_df <- df %>%
    filter(subject_id == subject_idx) %>%
    mutate(trial = 1:100) %>%
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    filter(!rt <= 300) %>% # response times cutoffs
    filter(!rt >= 9000) %>%
    mutate(vd = left_rating - right_rating) %>%
    mutate(sd = left_sim - right_sim)
  
  # df %>% select(choice,left_MAX,right_MAX,left_MIN,right_MIN,choose_max,choose_min) %>% View
  if (subject_idx %in% c(12, 35, 37)) { # remove the one subject that removes all the trials
    p_values[[subject_idx]] <- NA
    next
  }

  # print(ggplot(temp_df, aes(trial, rt)) + geom_point()+
  #         geom_smooth(method = "lm") +
  #         geom_hline(yintercept = 300, linetype = "dashed")+
  #         geom_hline(yintercept = 9000, linetype = "dashed")+
  #         labs(title = paste0("subject: ",subject_idx)))
  
  # temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating + choose_max + choose_min, family = binomial, data = temp_df))
  # temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating + left_net + right_net + left_sim + right_sim + choose_max + choose_min, family = binomial, data = temp_df))
  # temp_res <- broom::tidy(glm(choice ~ left_wtrating + right_wtrating + left_net + right_net + left_sim + right_sim + choose_max + choose_min, family = binomial, data = temp_df))
  
  # temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating + left_net + right_net + left_sim + right_sim, family = binomial, data = temp_df))
  # temp_res <- broom::tidy(glm(choice ~ left_wtrating + right_wtrating + left_sim + right_sim, family = binomial, data = temp_df))
  
  temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating, family = binomial, data = temp_df))
  
  temp_res$p.value <- round(temp_res$p.value, 2)
  
  # print(temp_res)

  # if ((temp_res[2, 5][[1]] > 0.05) & (temp_res[3, 5][[1]] > 0.05) & (temp_res[8, 5][[1]] > 0.05) & (temp_res[9, 5][[1]] > 0.05)) { # check p-value (prereg -- 0.05. check robustness across values)
    if ((temp_res[2, 5][[1]] > 0.05) & (temp_res[3, 5][[1]] > 0.05)) { # check p-value (prereg -- 0.05. check robustness across values)
      # if ((temp_res[2, 5][[1]] > 0.1) & (temp_res[3, 5][[1]] > 0.1)) { # check p-value (prereg -- 0.05. check robustness across values)
        
      #'two-stage residual inclusion to test if people are using the choose min stratedgy
      test <- broom::augment(glm(choice ~ left_rating + right_rating, family = binomial, data = temp_df)) %>% left_join(temp_df)

      # print(broom::tidy(lm(`.resid` ~ choose_max + choose_min, data = test)))
      
      p_values[[subject_idx]] <- unique(temp_df$subject_id)

    plt <- df %>%
      filter(subject_id == subject_idx) %>%
      mutate(trial = 1:100) %>%
      mutate(
        Q1 = quantile(rt, .25),
        Q3 = quantile(rt, .75),
        IQR = IQR(rt)
      ) %>%
      filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
      filter(!rt <= 300) %>% # response times cutoffs
      filter(!rt >= 9000) %>%
      mutate(vd = left_rating - right_rating) %>%
      mutate(binned_value_diff = as.numeric(cut_number(vd, 9)) - 5) %>%
      group_by(binned_value_diff) %>%
      mutate(
        n = n(),
        m_left = mean(choice),
        se = sqrt(var(choice) / length(choice))
      ) %>%
      ungroup() %>%
      ggplot(aes(x = binned_value_diff, y = m_left)) +
      geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
      theme_classic() +
      geom_line(size = 1) +
      geom_hline(yintercept = .5, linetype = "dashed") +
      scale_color_brewer(palette = "Set1") +
      scale_y_continuous(limits = c(0, 1.01)) +
      labs(
        title = paste0("subject: ", subject_idx),
        y = "Probability of Choosing Left",
        x = "Value Difference (L-R)"
      )
    print(paste0("############### ", "subject: ", subject_idx, " ###############"))
    
    # print(plt)
    # print(temp_res)
  } else {
    (p_values[[subject_idx]] <- NA)
  }
  # print(temp_res)
}

# print(p_values)
# so far we have a 25% exclusion rate
# so if we relax the exclusion criterion to p = 0.0 we get 19%
# print(paste0("############### Subject data exlclusions:", ((length(as.numeric(na.omit(p_values))) + 2) / 75)*100,"%  ###############"))
length(as.numeric(na.omit(p_values)))

dput(as.numeric(na.omit(p_values)))


exlusions <- function(df) {
  # function for data exclusions following the preregistration specs
  temp <- df %>%
    # filter(subject_id != 35) %>%
    # filter(!subject_id %in% c(17, 29, 35, 37)) %>% #comment out for no exclusions (subejct 13?)
    # filter(!subject_id %in% c(4, 5, 10, 12, 17, 22, 29, 35, 37,41, 42, 48, 55, 57, 58, 63, 64, 65, 66, 67, 70, 75)) %>% #comment out for no exclusions (subejct 13?)
    filter(!subject_id %in% as.numeric(na.omit(p_values))) %>%
    group_by(subject_id) %>% # response times (IQR exclusion)
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
# net_stats <- c("strength","eigen","efficiency", "edge_density", "modularity")
# net_stats <- c("strength","eigen","efficiency", "edge_density", "modularity", "conductance","weighted_clustering_coefficient")

net_stats <- c("edge_density", "modularity", "pca1", "pca2")

# net_stats <- c("weighted_transitivity", "edge_density", "modularity", "conductance", "pca1", "pca2")
# net_stats <- c("weighted_transitivity", "modularity", "conductance", "pca1", "pca2")

# net_stats <- c("modularity")

res_netstats <- vector(mode = "list", length = length(net_stats))
res_netstats2 <- vector(mode = "list", length = length(net_stats))

# res_model_comparisons <- vector(mode = "list", length = length(net_stats))
# net_idx  = 6
for (net_idx in 1:length(net_stats)) {
  # for each network statistic...
  print(paste0("############### ", net_stats[[net_idx]], " ###############"))

  # generate the dataset with the network statistic of interest
  # (no longer needed each time. Will save alot of computation time)
  # df <- organize_group_data(experiment = 2, net_stat = net_stats[[net_idx]])
  # test <- df[(df$left_net != 0) | (df$right_net != 0),] #eleminate trials where both are zero

  df$left_net <-   select(df,contains(net_stats[[net_idx]]))[[1]]
  df$right_net <-   select(df,contains(net_stats[[net_idx]]))[[2]]
  
  #### data analysis (regressions)
  print(paste0("############### CHOICE ###############"))
  models_choice <- estimate_mlms(df, outcome = "choice")
  # models_choice <- estimate_brms(df, outcome = "choice") #bayes versions
  
  # print(performance::compare_performance(models_choice, rank = TRUE, metrics = c("AIC", "BIC")))
  # print(performance::compare_performance(models_choice, rank = TRUE, metrics = c("WAIC","LOOIC")))
  
  print(parameters::compare_models(models_choice,  style = "ci_p"))
  # print(parameters::compare_models(models_choice))
  
  mp <- modelplot(models_choice, coef_omit = "Interc") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Coefficients",y = "Terms",
         title = paste0("Choice: ",net_stats[[net_idx]])
    ) +
    theme_classic() +
    scale_color_brewer(palette = "Set1")
  print(mp)
  res_netstats[[net_idx]] <- models_choice[[5]]
  
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
  models_rt <- estimate_mlms(df, outcome = "rt")
  # models_rt <- estimate_mlms(df[df$correct == 1,], outcome = "rt") #only ocrrect
  
  # models_rt <- estimate_brms(df, outcome = "rt") #bayes versions
  
  # print(performance::compare_performance(models_rt, rank = TRUE))
  print(parameters::compare_models(models_rt,  style = "ci_p"))
  # # print(parameters::compare_models(models_choice))
  # 
  mp <- modelplot(models_rt, coef_omit = "Interc") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Coefficients",y = "Terms",
         title = paste0("RT: ",net_stats[[net_idx]])
    ) +
    theme_classic() +
    scale_color_brewer(palette = "Set1")
  print(mp)
  # res_netstats[[net_idx]] <- models_rt[[5]]
  
  # generate tables
  generate_table(models_choice, type = "exp_2_choice", net_stat = net_stats[[net_idx]], save = T)
  # generate_table(models_correct, type = "exp_2_correct", net_stat = net_stats[[net_idx]], save = T)
  generate_table(models_rt, type = "exp_2_rt", net_stat = net_stats[[net_idx]], save = T)
  
  #nice way to make those regression coef tables you like
  #TODO make one with factor for each
  # mp <- modelplot(models_choice, coef_omit = "Interc") +
  #   geom_vline(xintercept = 0, linetype = "dashed") +
  #   labs(
  #     x = "Coefficients",
  #     y = "Terms",
  #     title = net_stats[[net_idx]]
  #   ) +
  #   theme_classic() +
  #   scale_color_brewer(palette = "Set1")
  # print(mp)
  # res_netstats[[net_idx]] <- mp
  
  # Bayes analysis
  # choice_res <- estimate_brms(create_dataset(df, type = "choice"), outcome = "choice")
  # correct_res <- estimate_brms(create_dataset(df, type = "correct/rt"), outcome = "correct")
  # rt_res <- estimate_brms(create_dataset(df, type = "correct/rt"), outcome = "rt")
  #
  # res_netstats[[net_idx]] <- list(choice_res, correct_res, rt_res)

  # # model metrics
  # #does the correct and choice model map to one another

  # model_compare_choice_res <- map(choice_res, loo)
  # model_compare_correct_res <- map(correct_res, loo)
  # model_compare_rt_res <- map(rt_res, loo)
  #
  # names(model_compare_choice_res) <- c(1,2,3,4,5,6,7,8,9,10)
  # names(model_compare_correct_res) <- c(1,2,3,4,5,6,7,8,9,10)
  # names(model_compare_rt_res) <- c(1,2,3,4,5,6,7,8,9,10)
  #
  # res_model_comparisons[[net_idx]] <- list(model_compare_choice_res, model_compare_correct_res, model_compare_rt_res)
  #
  # loo_compare(model_compare_choice_res)
  # loo_compare(model_compare_correct_res)
  # loo_compare(model_compare_rt_res)

  #
  # map(correct_res, bayestestR::sexit)
  # map(choice_res, bayestestR::sexit)

  #
  # knitr::kable(bayestestR::sexit(correct_res[[4]]), digits = 2)
  # knitr::kable(bayestestR::sexit(choice_res[[4]]), digits = 2)
  # knitr::kable(bayestestR::sexit(rt_res[[4]]), digits = 2)
}
# (res_netstats[[1]] + res_netstats[[2]])/(res_netstats[[3]] + res_netstats[[4]])
# save(res_netstats, file = here("data", "res_mixed_model.RData"))


# Compute the Probability of Direction (pd, also known as the Maximum Probability of Effect - MPE).
# It varies between ⁠50%⁠ and ⁠100%⁠ (i.e., 0.5 and 1) and can be interpreted as the probability
# (expressed in percentage) that a parameter (described by its posterior distribution) is
# strictly positive or negative (whichever is the most probable).
# It is mathematically defined as the proportion of the posterior distribution that is of the median's sign.
# Although differently expressed, this index is fairly similar (i.e., is strongly correlated) to the frequentist p-value.

# pd <= 95% ~ p > .1: uncertain
# pd > 95% ~ p < .1: possibly existing
# pd > 97%: likely existing
# pd > 99%: probably existing
# pd > 99.9%: certainly existing
# 
# library(bayestestR) # Understand and Describe Bayesian Models and Posterior
# # Distributions
# library(insight) # Easy Access to Model Information for Various Model Objects
# 
# posteriors <- insight::get_parameters(fit_choice02A)
# ggplot(posteriors, aes(x = b_nd)) +
#   geom_density(fill = "orange") +
#   geom_vline(xintercept = 0) +
#   theme_classic()

# Run many brms models in parallel using futures
# https://rpubs.com/mvuorre/brms-parallel
# this will let you fun the choice and correct model at the same time

# library(future) # Unified Parallel and Distributed Processing in R for Everyone
# 
# df <- organize_group_data(experiment = 2, net_stat = net_stats[[3]])
# #
# plan(
#   list(
#     tweak(multisession, workers = 4),
#     tweak(multisession, workers = 4)
#   )
# )
# # #you need to make sure you feed the correct data. But this should work otherwise?
# # #need to change the number of cores used in the function as well I think.
# fits1 %<-% estimate_brms(df = create_dataset(df, type = "correct/rt"), outcome = "correct")
# fits2 %<-% estimate_brms(df = create_dataset(df, type = "correct/rt"), outcome = "rt")
