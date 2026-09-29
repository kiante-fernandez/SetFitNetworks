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
# 2024/10/20      Kianté  Fernandez                   refactored for sharing
#
# Libraries ----------------------------------------------------------------
library(purrr)          # Functional Programming Tools
library(tidyverse)      # Data manipulation
library(jsonlite)       # JSON Parser
library(brms)           # Bayesian regression
library(cmdstanr)       # Stan interface

# Load helper functions ----------------------------------------------------
source(here::here("src", "utils.R"))
source(here::here("src", "exploratory_graph_analysis.R"))

# Load Data from Raw -----------------------------------------------------
net_degree <- calculate_net_stats(g)
df <- organize_group_data(experiment = 2)

# Response Time Exclusions  ---------------------------------------
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
    filter(!rt <= 250) %>%
    filter(!rt >= 9000) %>%
    summarise(pct_excluded = (100 - n()) / 100)
  
  rt_exclude_pct[[subject_idx]] <- temp_df$pct_excluded
  
  if (temp_df$pct_excluded > 0.40) {
    print(paste0("######## subject: ", subject_idx, " #######"))
    print(paste0("######## percent trials excluded: ", temp_df$pct_excluded, " #######"))
  }
}

mean(rt_exclude_pct)

# Value Difference exclusions & set strategy selection -------------------------
p_values <- vector(mode = "numeric", length = 75)
set_strategy_winner <- vector(mode = "numeric", length = 75)

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
    filter(!rt <= 250) %>%
    filter(!rt >= 9000) %>%
    mutate(
      vd = left_rating - right_rating,
      sd = left_sim - right_sim,
      left_range = left_MAX - left_MIN,
      right_range = right_MAX - right_MIN
    )
  
  # Skip odd trial subjects (not removed from main anaysis so long as they have enough trials) 
  if (subject_idx %in% c(12, 35, 37)) {
    p_values[[subject_idx]] <- NA
    next
  }
  
  # Fit models for different selection strategies
  temp_res0 <- glm(choice ~ left_rating + right_rating, family = binomial, data = temp_df)
  temp_res1 <- glm(choice ~ choose_max, family = binomial, data = temp_df)
  temp_res2 <- glm(choice ~ choose_min, family = binomial, data = temp_df)
  temp_res3 <- glm(choice ~ left_range + right_range, family = binomial, data = temp_df)

  # Compare model performance
  xx <- performance::compare_performance(
    temp_res0, temp_res1, temp_res2, temp_res3,
    rank = TRUE,
    metrics = c("AIC", "AICc", "BIC", "RMSE", "R2")
  )
  
  print(paste0("############### Subject data:", subject_idx, "  ###############"))
  print(xx[, c(1, 8)])
  set_strategy_winner[[subject_idx]] <- xx[1, 1]
  
  # Check significance
  temp_res <- broom::tidy(glm(choice ~ left_rating + right_rating, family = binomial, data = temp_df))
  temp_res$p.value <- round(temp_res$p.value, 2)
  
  if ((temp_res[2, 5][[1]] > 0.05) & (temp_res[3, 5][[1]] > 0.05)) {
    # Create diagnostic plot for significant cases
    plt <- df %>%
      filter(subject_id == subject_idx) %>%
      mutate(trial = 1:100) %>%
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
        binned_value_diff = as.numeric(cut_number(vd, 9)) - 5
      ) %>%
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
    p_values[[subject_idx]] <- unique(temp_df$subject_id)
  } else {
    p_values[[subject_idx]] <- NA
  }
}

# Print exclusion summary
print(paste0("############### Subject data exlclusions:", 
             ((length(as.numeric(na.omit(p_values))) + 2) / 75)*100,"%  ###############"))
length(as.numeric(na.omit(p_values)))
dput(as.numeric(na.omit(p_values)))
length(set_strategy_winner[is.na(as.numeric(p_values))])
length(set_strategy_winner)
set_strategy_winner <- set_strategy_winner[is.na(as.numeric(p_values))]

# Data Exclusion Function ------------------------------------------------
exlusions <- function(df) {
  temp <- df %>%
    filter(!subject_id %in% c(29, 35, 37)) %>% #RT exclusions
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

# Apply exclusions and save data
for_save <- df %>% exlusions()
# write_csv(for_save, "data/ISDN_poster_exp2.csv")

# Regression  Analysis on Network stats ----------------------------------------
net_stats <- c("strength", "betweenness", "closeness", "weighted_transitivity",
               "eigen", "edge_density", "modularity", "pca1", "pca2")

res_netstats <- vector(mode = "list", length = length(net_stats))
net_idx = 9
for (net_idx in 1:length(net_stats)) {
  print(paste0("############### ", net_stats[[net_idx]], " ###############"))
  
  # Prepare network statistics
  df$left_net1 <- select(df, contains(net_stats[[net_idx]]))[[1]]
  df$right_net1 <- select(df, contains(net_stats[[net_idx]]))[[2]]
  df$left_net2 <- select(df, contains(net_stats[[9]]))[[1]]
  df$right_net2 <- select(df, contains(net_stats[[9]]))[[2]]
  
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
    file = here::here("fits", paste0(net_stats[[net_idx]], "_exp_2_fit_choice03"))
  )
  print(paste0("############### RT ###############"))
  df_temp <- create_dataset(df, type = "correct/rt")
  
  models_rt <- brm(
    log(rt) ~ vd + ov + nd1 + sd + (vd + ov + nd1 + sd | subject_id),
    data = df_temp,
    iter = 10000,
    chains = 4,
    cores = 4,
    file = here::here("fits", paste0(net_stats[[net_idx]], "_exp_2_fit_rt02"))
  )
  
  res_netstats[[net_idx]] <- list(models_choice, models_rt)
}

