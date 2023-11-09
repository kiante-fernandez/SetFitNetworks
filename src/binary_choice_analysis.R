# binary_choice_analysis.R - 2AFC analysis of previous datasets
# the reliablity and stability of the graph
#
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
# 08/10/23      Kianté  Fernandez                       wrote code

# Load necessary libraries
library(here)
library(tidyverse)
library(purrr)
library(lme4)
library(lmerTest)
library(patchwork)

# Uncomment below if needed
# library(brms)
# library(cmdstanr)

# Load data
Lee_Hare_2023_choice_data_exp2 <- read_csv("data/Lee_Hare_2023_OSF/Lee_Hare_2023_choice_data_exp2.csv")

# Source functions for network analysis
source("exploratory_graph_analysis.R")
source(here::here("src", "utils.R"))

# Calculate network statistics
net_degree <- calculate_net_stats(g)

# Prepare and mutate data
df <- Lee_Hare_2023_choice_data_exp2 %>%
  mutate(rt = rt * 1000) %>%
  rowwise() %>%
  mutate(
    name_left = net_degree$Name[net_degree$Image == item_number_left],
    name_right = net_degree$Name[net_degree$Image == item_number_right],
    PCA1_left = net_degree$PCA1[net_degree$Image == item_number_left],
    PCA1_right = net_degree$PCA1[net_degree$Image == item_number_right],
    PCA2_left = net_degree$PCA2[net_degree$Image == item_number_left],
    PCA2_right = net_degree$PCA2[net_degree$Image == item_number_right],
    choice = if_else(choice == 1, 0, 1) # Reverse choice coding
  )

# Response time exclusions
rt_exclude_pct <- vector("numeric", length(unique(df$subject_id)))
for (subject_idx in 1:length(unique(df$subject_id))) {
  temp_df <- df %>%
    ungroup() %>%
    filter(subject_id == subject_idx) %>%
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    filter(rt > 250 & rt < 9000) %>% # Apply response time cutoffs
    summarise(pct_excluded = (30 - n()) / 30)

  rt_exclude_pct[[subject_idx]] <- temp_df$pct_excluded

  # Warning for high exclusion rates
  if (temp_df$pct_excluded > 0.40) {
    cat("######## subject:", subject_idx, "#######\n")
    cat("######## percent trials excluded:", temp_df$pct_excluded, "#######\n")
  }
}

# Calculate mean exclusion percentage
mean(rt_exclude_pct)

# Function for data exclusions
exclusions <- function(df) {
  df %>%
    filter(!subject_id %in% c(1, 6, 17, 19, 27, 28, 32, 37, 38, 42, 44, 45, 46, 51, 56, 71, 73, 74, 86, 91, 92)) %>%
    group_by(subject_id) %>%
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    ungroup() %>%
    filter(rt > 250 & rt < 9000) # Apply response time cutoffs
}

# Prepare data for model
standardized <- TRUE

for_model <- df %>%
  exclusions() %>%
  group_by(subject_id) %>%
  mutate(
    zleft_rating = scale(item_value_left, center = standardized, scale = standardized),
    zright_rating = scale(item_value_right, center = standardized, scale = standardized),
    zleft_net1 = scale(PCA1_left, center = standardized, scale = standardized),
    zright_net1 = scale(PCA1_right, center = standardized, scale = standardized),
    zleft_net2 = scale(PCA2_left, center = standardized, scale = standardized),
    zright_net2 = scale(PCA2_right, center = standardized, scale = standardized),
    nd1 = scale(abs(PCA1_left - PCA1_right), center = standardized, scale = standardized),
    nd2 = scale(abs(PCA2_left - PCA2_right), center = standardized, scale = standardized),
    vd = scale(abs(item_value_left - item_value_right), center = standardized, scale = standardized),
    ov = scale(item_value_left + item_value_right, center = standardized, scale = standardized)
  ) %>%
  ungroup() %>%
  select(subject_id, trial, choice, rt, zleft_rating, zright_rating, zleft_net1, zright_net1, zleft_net2, zright_net2, vd, nd1, nd2, ov)

# Model for choice
models_choice <- glmer(
  choice ~ zleft_rating * (zleft_net1 + zleft_net2) + zright_rating * (zright_net1 + zright_net2) +
    (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 + zleft_net2 + zright_net2 | subject_id),
  data = for_model,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7))
)

# Model for response time
models_rt <- lmer(
  log(rt) ~ vd + ov + nd1 + nd2 + (vd + ov + nd1 + nd2 | subject_id),
  data = for_model,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Output model summaries
summary(models_choice)
summary(models_rt)
