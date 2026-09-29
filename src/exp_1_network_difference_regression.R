# network_difference_regression_analysis.R - regression analysis for exp 1

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
# 2024/10/20      Kianté  Fernandez                   refactored for sharing
#
# Libraries ----------------------------------------------------------------
library(purrr)          # Functional Programming Tools
library(tidyverse)      # Data manipulation and visualization
library(jsonlite)       # JSON Parser
library(brms)           # Bayesian regression
library(cmdstanr)       # Stan interface

# Load helper functions ----------------------------------------------------
source(here::here("src", "utils.R"))
source(here::here("src", "exploratory_graph_analysis.R"))

# Load Data from Raw -----------------------------------------------------
net_degree <- calculate_net_stats(g)
df <- organize_group_data(experiment = 1)

# Response Time Exclusions  ---------------------------------------
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
  
  if (temp_df$pct_excluded > 0.40) {
    print(paste0("######## subject: ", subject_idx, " #######"))
    print(paste0("######## percent trials excluded: ", temp_df$pct_excluded, " #######"))
  }
}

# Print mean exclusions
mean(rt_exclude_pct)

# Value Difference exclusions & set strategy selection -------------------------
p_values <- vector(mode = "numeric", length = 30)
set_strategy_winner <- vector(mode = "numeric", length = 30)

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
    filter(!rt <= 250) %>%
    filter(!rt >= 9000) %>%
    mutate(
      vd = left_rating - right_rating,
      sd = left_sim - right_sim,
      left_range = left_MAX - left_MIN,
      right_range = right_MAX - right_MIN
    )
  
  if (subject_idx %in% c(9, 13, 30)) { #remove subjects from RT exclusions (most (more than half) trials were removed)
    p_values[[subject_idx]] <- NA
    next
  }
  
  # Fit models for different selection strategies
  temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating + choose_max + choose_min, 
                              family = binomial, data = temp_df))
  
  # Model comparisons
  temp_res0 <- glm(choice ~ left_rating + right_rating, family = binomial, data = temp_df)
  temp_res1 <- glm(choice ~ choose_max, family = binomial, data = temp_df)
  temp_res2 <- glm(choice ~ choose_min, family = binomial, data = temp_df)
  temp_res3 <- glm(choice ~ left_range + right_range, family = binomial, data = temp_df)
  
  xx <- performance::compare_performance(
    temp_res0, temp_res1, temp_res2, temp_res3, 
    rank = TRUE, 
    metrics = c("AIC", "AICc", "BIC", "RMSE", "R2")
  )
  
  print(paste0("############### Subject data:", subject_idx, "  ###############"))
  print(xx[, c(1, 8)])
  set_strategy_winner[[subject_idx]] <- xx[1, 1]
  
  temp_res$p.value <- round(temp_res$p.value, 2)
  # Check significance of coefficients
  if ((temp_res[2, 5][[1]] > 0.05) & 
      (temp_res[3, 5][[1]] > 0.05) & 
      (temp_res[4, 5][[1]] > 0.05) & 
      (temp_res[5, 5][[1]] > 0.05)) {
    p_values[[subject_idx]] <- unique(temp_df$subject_id)
  } else {
    p_values[[subject_idx]] <- NA
  }
}

# Print exclusion summary
print(paste0("############### Subject data exlclusions:", 
             ((length(as.numeric(na.omit(p_values)))) / 30)*100,"%  ###############"))
as.numeric(na.omit(p_values))
length(set_strategy_winner[is.na(as.numeric(p_values))])
length(set_strategy_winner)
set_strategy_winner <- set_strategy_winner[is.na(as.numeric(p_values))]

# Data exclusion function
exlusions <- function(df) {
  temp <- df %>%
    filter(!subject_id %in% as.numeric(na.omit(p_values))) %>%
    group_by(subject_id) %>%
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    ungroup() %>%
    filter(!rt <= 250) %>%
    filter(!rt >= 9000)
  
  return(temp)
}

# Apply exclusions and save
for_save <- df %>% exlusions()
# write_csv(for_save, "data/ISDN_poster_exp1.csv")

# Regression  Analysis on Network stats ----------------------------------------
net_stats <- c("strength", "betweenness", "closeness", "weighted_transitivity", 
               "eigen", "edge_density", "modularity", "pca1", "pca2")

res_netstats <- vector(mode = "list", length = length(net_stats))
# net_idx <- 9
for (net_idx in 1:length(net_stats)) {
  print(paste0("############### ", net_stats[[net_idx]], " ###############"))
  
  # Prepare network statistics
  df$left_net1 <- select(df, contains(net_stats[[net_idx]]))[[1]]
  df$right_net1 <- select(df, contains(net_stats[[net_idx]]))[[2]]
  df$left_net2 <- select(df, contains(net_stats[[9]]))[[1]] # pc2
  df$right_net2 <- select(df, contains(net_stats[[9]]))[[2]] # pc2
  
  # Choice model analysis
  print(paste0("############### CHOICE ###############"))
  df_temp <- create_dataset(df, type = "choice")
  
  models_choice <- brm(
    choice ~ zleft_rating * (zleft_net1) + zright_rating * (zright_net1) +
      (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 | subject_id),
    data = df_temp, 
    family = "bernoulli", 
    iter = 10000,
    chains = 4, 
    cores = 4,
    file = here::here("fits", paste0(net_stats[[net_idx]], "_exp_1_fit_choice03"))
  )
  
  # Response time analysis
  print(paste0("############### RT ###############"))
  df_temp <- create_dataset(df, type = "correct/rt")
  
  models_rt <- brm(
    log(rt) ~ vd + ov + nd1 + (vd + ov + nd1 | subject_id),
    data = df_temp, 
    iter = 10000,
    chains = 4, 
    cores = 4, 
    file = here::here("fits", paste0(net_stats[[net_idx]], "_exp_1_fit_rt02"))
  )
  
  res_netstats[[net_idx]] <- list(models_choice, models_rt)
}



