# exp2_network_difference_regression_analysis.R - analysis of experiment two
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
# 2022/12/22      Kianté  Fernandez                   coded up version one
# 2022/12/25      Kianté  Fernandez                   moved many functions to utils
# 2022/12/27      Kianté  Fernandez                   created table functions

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

source(here::here("src", "utils.R"))

# get correlations between items
lee_2021_rating1 <- readr::read_csv(here::here("data", "lee_2021_rating1.csv"), col_names = FALSE)
cor.snack_food <- SemNeT::similarity(lee_2021_rating1, method = "cor")
cor_snack_food <- data.frame(matrix(cor.snack_food[cor.snack_food != 1], 59, 60))
names(cor_snack_food) <- load_food_names()$FoodNames$Name

######
# calculate a bunch of network measures to look at relationship to stuff
source("exploratory_graph_analysis.R")

# source('fernandez_rating_network.R') #load the EGA from the new rating data

net_degree <- calculate_net_stats(g)

##### loading the data#####
df <- organize_group_data(experiment = 2)

# describe_exlusions <- function(){
#   ###this function will take the RT exclusion below and print the subject were
    ###70% of the trails were removed. Thus we will not run the exclusion 
    ###regressions because we don't have enough trials to preform them. 

    ##then it will take the remaining data and run individual level logistic 
    ##regressions with value difference regressed onto choice. It will print the 
    ## not significant subjects and add there names to a list for the exclusion function
# }

### check for response time exclusions (before or after choice?)

for (subject_idx in 1:length(unique(df$subject_id)) ) {
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
    summarise(pct_excluded = (100 - n()) / 100)


  if (temp_df$pct_excluded > 0.70) {
    print(paste0("######## subject: ", subject_idx, " #######"))
    print(paste0("######## percent trials excluded: ", temp_df$pct_excluded, " #######"))
  }
}

## value difference exclusion
p_values <- vector(mode = "numeric", length = 75)

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
    mutate(nd = left_net - right_net) %>%
    mutate(sd = left_sim - right_sim)

  if (subject_idx %in% c(35)) { # remove the one subject that removes all the trials
    p_values[[subject_idx]] <- NA
    next
  }

  # print(ggplot(temp_df, aes(trial, rt)) + geom_point()+
  #         geom_smooth(method = "lm") +
  #         geom_hline(yintercept = 300, linetype = "dashed")+
  #         geom_hline(yintercept = 9000, linetype = "dashed")+
  #         labs(title = paste0("subject: ",subject_idx)))

  temp_res <- broom::tidy(glm(choice ~ vd, family = binomial, data = temp_df))

  temp_res$p.value <- round(temp_res$p.value, 7)
  if (temp_res[2, 5][[1]] > 0.10) { # check p-value (prereg -- 0.05. check robustness across values)
    
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
    print(plt)
    print(temp_res)
  } else {
    (p_values[[subject_idx]] <- NA)
  }
}

print(p_values)
# so far we have a 34% exclusion rate
# so if we relax the exclusion criterion to p = 0.1 we get 26%

length(as.numeric(na.omit(p_values))) / 75

as.numeric(na.omit(p_values))

exlusions <- function(df) {
  # function for data exclusions following the preregistration specs
  temp <- df %>%
    filter(subject_id != 35) %>% # rt exclusions
    filter(!subject_id %in% as.numeric(na.omit(p_values))) %>%
    group_by(subject_id) %>% # response times (IQR exclusion)
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    ungroup() %>%
    filter(!rt <= 300) %>% # response times cutoffs
    filter(!rt >= 9000)
  return(temp)
}

net_stats <- c("strength", "eigen", "edge_density", "modularity")

res_netstats <- vector(mode = "list", length = length(net_stats))

net_idx <- 4
for (net_idx in 1:length(net_stats)) {
  # for each network statistic...
  print(paste0("############### ", net_stats[[net_idx]], " ###############"))

  # generate the dataset with the network statistic of interest
  df <- organize_group_data(experiment = 2, net_stat = net_stats[[net_idx]])

  #### data analysis (regressions)
  #### coded for choice
  choice_res <- estimate_mlms(create_dataset(df, type = "choice"), outcome = "choice")
  ### coded for correct
  correct_res <- estimate_mlms(create_dataset(df, type = "correct/rt"), outcome = "correct")
  ### response times
  rt_res <- estimate_mlms(create_dataset(df, type = "correct/rt"), outcome = "rt")
  
  # store the results from each set of models
  res_netstats[[net_idx]] <- list(choice_res, correct_res, rt_res)
  # map(rt_res, summary)
  # map(correct_res, summary)
  # map(choice_res, summary)

  # Bayes analysis

  choice_res <- estimate_brms(create_dataset(df, type = "choice"), outcome = "choice")
  correct_res <- estimate_brms(create_dataset(df, type = "correct/rt"), outcome = "correct")
  rt_res <- estimate_brms(create_dataset(df, type = "correct/rt"), outcome = "rt")

  # model metrics
  #does the correct and choice model map to one another
  model_compare_choice_res <- map(choice_res, loo)
  model_compare_correct_res <- map(correct_res, loo)
  names(model_compare_choice_res) <- c(1,2,3,4)
  names(model_compare_correct_res) <- c(1,2,3,4)
  
  # names(model_compare_res) <- c(1:9)
  loo_compare(model_compare_choice_res)
  loo_compare(model_compare_correct_res)
  
  map(correct_res, bayestestR::sexit)
  map(rt_res, bayestestR::sexit)
  
}
#generate tables
for (foo in 1:4){
  print(generate_table(res_netstats[[foo]][[1]], type = "choice", net_stat = net_stats[[foo]], save = T))
  print(generate_table(res_netstats[[foo]][[2]], type = "correct", net_stat = net_stats[[foo]], save = T))
  print(generate_table(res_netstats[[foo]][[3]], type = "rt", net_stat = net_stats[[foo]], save = T))
}


# prepare datasets
# check correlations
# model_dat %>%
#   ungroup() %>%
#   select(vd,nd,sd,ov,on,os) %>%
#   correlation::correlation() %>%
#   print()


# Run many brms models in parallel using futures
# https://rpubs.com/mvuorre/brms-parallel
# this will let you fun the choice and correct model at the same time
# library(future)
#
# plan(
#   list(
#     tweak(multisession, workers = 4),
#     tweak(multisession, workers = 4)
#   )
# )
# #you need to make sure you feed the correct data. But this should work otherwise?
# #need to change the number of cores used in the function as well I think.
# fits1 %<-% estimate_brms(df=create_dataset(df, type = "choice"), outcome ="choice")
# fits2 %<-% estimate_brms(df=create_dataset(df, type = "correct/rt"), outcome = "correct")
#
