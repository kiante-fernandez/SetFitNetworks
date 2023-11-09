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

# Libraries
library(here)
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(purrr) # Functional Programming Tools
library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4
library(lmerTest) # Tests in Linear Mixed Effects Models
library(patchwork) # The Composer of Plots

# library(brms) # Bayesian Regression Models using 'Stan'
# library(cmdstanr)

#load data
Lee_Hare_2023_choice_data_exp2 <- read_csv("data/Lee_Hare_2023_OSF/Lee_Hare_2023_choice_data_exp2.csv")

######
# calculate a bunch of network measures to look at relationship to stuff

source("exploratory_graph_analysis.R")
source(here::here("src", "utils.R"))

net_degree <- calculate_net_stats(g)

df <- Lee_Hare_2023_choice_data_exp2 %>% 
  mutate(rt = rt*1000) %>%
  rowwise() %>% 
  mutate(name_left = net_degree$Name[net_degree$Image == item_number_left], 
         name_right = net_degree$Name[net_degree$Image == item_number_right],
         PCA1_left = net_degree$PCA1[net_degree$Image == item_number_left], 
         PCA1_right = net_degree$PCA1[net_degree$Image == item_number_right],
         PCA2_left = net_degree$PCA2[net_degree$Image == item_number_left], 
         PCA2_right = net_degree$PCA2[net_degree$Image == item_number_right],
         choice = if_else(choice == 1, 0, 1) #flip the choice to choose left rather than choose right
         )

### check for response time exclusions (before or after choice?)
rt_exclude_pct <- vector(mode = "numeric", length = length(unique(df$subject_id)))
# subject_idx = 2
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
    filter(!rt <= 250) %>% # response times cutoffs
    filter(!rt >= 9000) %>%
    summarise(pct_excluded = (30 - n()) / 30)
  
  rt_exclude_pct[[subject_idx]] <- temp_df$pct_excluded
  
  if (temp_df$pct_excluded > 0.40) {
    print(paste0("######## subject: ", subject_idx, " #######"))
    print(paste0("######## percent trials excluded: ", temp_df$pct_excluded, " #######"))
  }
}

mean(rt_exclude_pct)

exlusions <- function(df) {
  # function for data exclusions following the preregistration specs
  temp <- df %>%
    filter(!subject_id %in% as.numeric(c(1,6,17,19,27,28,32,37,38,42,44,45,46,51,56,71,73,74,86,91,92))) %>%
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

standardized = TRUE

for_model <- df %>% exlusions() %>% 
  exlusions() %>%
  group_by(subject_id) %>%
  mutate(
    zleft_rating = scale(item_value_left, center = standardized, scale = standardized),
    zright_rating = scale(item_value_right, center = standardized, scale = standardized),
    zleft_net1 = scale(PCA1_left, center = standardized, scale = standardized),
    zright_net1 = scale(PCA1_right, center = standardized, scale = standardized),
    zleft_net2 = scale(PCA2_left, center = standardized, scale = standardized),
    zright_net2 = scale(PCA2_right, center = standardized, scale = standardized),
  ) %>%
  ungroup() %>%
  select(subject_id, trial, choice, zleft_rating, zright_rating, zleft_net1, zright_net1, zleft_net2, zright_net2)

models_choice <- glmer(choice ~ zleft_rating*(zleft_net1 + zleft_net2) + zright_rating*(zright_net1 + zright_net2) + 
                         (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 +  zleft_net2 + zright_net2 | subject_id),
                       data = for_model,  family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))

summary(models_choice)
report::report(models_choice)
plot(ggeffects::ggpredict(models_choice, terms = c("zleft_rating[all]")))


