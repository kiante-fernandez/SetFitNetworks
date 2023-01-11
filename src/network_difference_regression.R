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

library(sjPlot)
library(sjmisc)
library(sjlabelled)

library(brms)

#load helper functions
source(here::here("src", "utils.R"))
source(here::here("src", "utils_plotting.R"))

# get correlations between items
lee_2021_rating1 <- readr::read_csv(here::here("data", "lee_2021_rating1.csv"), col_names = FALSE)
cor.snack_food <- SemNeT::similarity(lee_2021_rating1, method = "cor")
cor_snack_food <- data.frame(matrix(cor.snack_food[cor.snack_food != 1], 59, 60))
names(cor_snack_food) <- load_food_names()$FoodNames$Name

######
# calculate a bunch of network measures to look at relationship to stuff
source("exploratory_graph_analysis.R")
net_degree <- calculate_net_stats(g)

##### loading the data#####
df <- organize_group_data(experiment = 1, net_stat = "modularity")

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
    filter(subject_id != 1) %>% # people with no vd effect
    filter(subject_id != 4) %>%
    filter(subject_id != 8) %>%
    filter(subject_id != 24) %>%
    filter(subject_id != 25) %>%
    filter(subject_id != 27) %>%
    # filter(subject_id != 16) %>%#new subjects start here
    # filter(subject_id != 26) %>%
    # filter(subject_id != 29) %>%
    # filter(subject_id != 30) %>%
    ungroup() %>%
    filter(!rt <= 300) %>% # response times cutoffs
    filter(!rt >= 9000)
  return(temp)
}

net_stats <- c("strength", "eigen", "edge_density", "modularity")

plts <- vector("list", length = length(net_stats))
net_idx <- 4

for (net_idx in 1:length(net_stats)) {
  print(paste0("############### ", net_stats[[net_idx]], " ###############"))
  df <- organize_group_data(experiment = 1, net_stat = net_stats[[net_idx]])

  #### data analysis

  fit1 <- brm(choice ~ vd + ov + nd + on + vd:nd + ov:on + (vd + ov + nd + on + vd:nd + ov:on | subject_id), 
              data = create_dataset(df, type = "choice"), family = "bernoulli", cores = 4, iter = 10000)
  fit2 <- brm(log(rt) ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id), data = create_dataset(df, type = "correct/rt"), cores = 10, iter = 10000)
  fit3 <- brm(correct ~ vd + ov + nd + on + vd:nd + ov:on + (vd + ov + nd + on + vd:nd + ov:on | subject_id), 
              data = create_dataset(df, type = "correct/rt"), family = "bernoulli", cores = 4, iter = 10000)
  
  bayestestR::sexit(fit1)
  bayestestR::sexit(fit3)
  
  knitr::kable(bayestestR::sexit(fit3), digits = 3) 
  
  
  knitr::kable(bayestestR::sexit(fit2), digits = 3) 
  
  #### choice

  formulas <- c(
    "choice ~ vd + ov + (vd + ov | subject_id)",
    "choice ~ vd + ov + nd + (vd + ov + nd | subject_id)",
    "choice ~ vd + ov + nd + on  +  (vd + ov + nd + on | subject_id)",
    "choice ~ vd + ov + nd + on  + vd:nd + ov:on + (vd + ov + nd + on + vd:nd + ov:on | subject_id)"
  )

  estimate_mlm <- function (formula){glmer(formula,
                                     data = create_dataset(df, type = "choice"),
                                     family = binomial(link = "logit"),
                                     control = glmerControl(
                                               optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5)))
  }
  map(formulas, estimate_mlm)
  
}


