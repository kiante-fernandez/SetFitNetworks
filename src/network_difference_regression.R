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
    # base model
    model1  = glmer(choice ~ zleft_rating + zright_rating + (1 | subject_id), data = create_dataset(df, type = "choice"), family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    # add network difference
    model2 <- glmer(choice ~ zleft_rating + zright_rating + zleft_net + zright_net + (1 | subject_id), data = create_dataset(df, type = "choice"), family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    return(list(model1, model2)) 
    
  } else if (outcome == "correct") {
    # base model
    df = create_dataset(df, type = "correct/rt")
    model1 <- glmer(correct ~ vd + ov + (1| subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add network difference
    model2 <- glmer(correct ~ vd + ov + nd + (1 | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    model3 <- glmer(correct ~ vd + ov + nd + on + (1 | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    return(list(model1, model2, model3)) 
    
  } else if (outcome == "rt") {
    df = create_dataset(df, type = "correct/rt")
    
    # base model
    model1 <- lmer(log(rt) ~ vd + ov + (1 | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add network difference
    model2 <- lmer(log(rt) ~ vd + ov + nd + (1 | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))    # add overall network
    model3 <- lmer(log(rt) ~ vd + ov + nd + on  + (1 | subject_id), data = df, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    return(list(model1, model2, model3)) 
  }
}
  
######
# calculate a bunch of network measures to look at relationship to stuff
source("exploratory_graph_analysis.R")
net_degree <- calculate_net_stats(g)

##### loading the data#####
df <- organize_group_data(experiment = 1, net_stat = "modularity")

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
  
  temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating + left_net + right_net + choose_max + choose_min, family = binomial, data = temp_df))
  # temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating + left_net + right_net, family = binomial, data = temp_df))
  
  temp_res$p.value <- round(temp_res$p.value, 2)
  
  if ((temp_res[2, 5][[1]] > 0.05) & (temp_res[3, 5][[1]] > 0.05) & (temp_res[6, 5][[1]] > 0.05) & (temp_res[7, 5][[1]] > 0.05)) { # check p-value (prereg -- 0.05. check robustness across values)
    # if ((temp_res[2, 5][[1]] > 0.05) & (temp_res[3, 5][[1]] > 0.05)) { # check p-value (prereg -- 0.05. check robustness across values)
      
    p_values[[subject_idx]] <- unique(temp_df$subject_id)
    print(temp_res)
  } else {
    (p_values[[subject_idx]] <- NA)
  }
  # print(temp_res)
}

(length(as.numeric(na.omit(p_values)))) / 30

as.numeric(na.omit(p_values))

exlusions <- function(df) {
  # function for data exclusions
  temp <- df %>%
    group_by(subject_id) %>% # response times
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    filter(!subject_id %in% as.numeric(na.omit(p_values))) %>%
    ungroup() %>%
    filter(!rt <= 300) %>% # response times cutoffs
    filter(!rt >= 9000)
  return(temp)
}

net_stats <- c("strength", "eigen", "edge_density", "modularity")

plts <- vector("list", length = length(net_stats))

for (net_idx in 1:length(net_stats)) {
  print(paste0("############### ", net_stats[[net_idx]], " ###############"))
  df <- organize_group_data(experiment = 1, net_stat = net_stats[[net_idx]])

  # #### data analysis
  
  #### data analysis (regressions)
  models_choice <- estimate_mlms(df, outcome = "choice")
  models_correct<- estimate_mlms(df, outcome = "correct")
  models_rt <- estimate_mlms(df, outcome = "rt")
  
  print(performance::compare_performance(models_choice, rank = TRUE, metrics = c("AIC", "BIC", "R2", "RMSE", "LOGLOSS")))
  print(performance::compare_performance(models_correct, rank = TRUE, metrics = c("AIC", "BIC", "R2", "RMSE", "LOGLOSS")))
  print(performance::compare_performance(models_rt, rank = TRUE))
  
  # generate tables
  generate_table(models_choice, type = "exp_1_choice", net_stat = net_stats[[net_idx]], save = F)
  generate_table(models_correct, type = "exp_1_correct", net_stat = net_stats[[net_idx]], save = F)
  generate_table(models_rt, type = "exp_1_rt", net_stat = net_stats[[net_idx]], save = F)
  
  mp <- modelplot(models_choice, coef_omit = "Interc") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      x = "Coefficients",
      y = "Terms",
      title = net_stats[[net_idx]]
    ) +
    theme_classic() +
    scale_color_brewer(palette = "Set1")
  
  plts[[net_idx]] <- mp
  
}
(plts[[1]] + plts[[2]])/(plts[[3]] + plts[[4]])


