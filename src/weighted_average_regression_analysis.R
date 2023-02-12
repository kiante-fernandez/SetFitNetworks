# weighted_average_regression_analysis.R - individual level model comparisons with weighted average models
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
# 2023/02/01      Kianté  Fernandez                   coded up version one

# Libraries
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(purrr) # Functional Programming Tools
library(jsonlite) # A Simple and Robust JSON Parser and Generator for R

library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4
library(lmerTest) # Tests in Linear Mixed Effects Models
library(brms) # Bayesian Regression Models using 'Stan'

library(patchwork) # The Composer of Plots
library(modelsummary)
library(kableExtra)
library(gt)

source(here::here("src", "utils.R"))

options(scipen=999)

######
# calculate a bunch of network measures to look at relationship to stuff

source("exploratory_graph_analysis.R")
# source('fernandez_rating_network.R') #load the EGA from the new rating data

#look at the emprical network res
# A <- ega_res[["EGA"]][["network"]]
# # dimattributes <- ega_res[["EGA"]][["wc"]]
# dimattributes <- ega_res[["typicalGraph"]][["wc"]]
# g <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
# V(g)$snack_type <- dimattributes

net_degree <- calculate_net_stats(g)

weights = c("degree", "strength", "weighted_transitivity", "eigen", "closeness", "betweenness")
#also look at just weighting by the variance
subj_weights_models <- subj_weights_models <- rep(list(vector(mode = "list", length = 6)),105)

counter_sub <- 1
counter_weight <- 1

# weight_idx = 6
for (weight_idx in 1:length(weights)) {
  ##### loading the data#####
df <- organize_group_data(experiment = 2, net_stat = "conductance", weight = weights[[weight_idx]])

## model comparisons with weighted average models
p_res <- data.frame(matrix(NA, nrow = 1, ncol = 14))
names(p_res) <- c("Name", "Model", "R2_Tjur", "RMSE", "Sigma", "Log_loss", "Score_log",
                  "Score_spherical", "PCP", "AIC_wt", "BIC_wt", "Performance_Score",
                  "subject_id","bfm")
# p_res <- data.frame(matrix(NA, nrow = 1, ncol = 9))
# names(p_res) <- c("Name", "Model", "R2", "R2_adjusted", "RMSE", "Sigma", "Performance_Score",
#                   "subject_id", "bfm")

# p_res <- data.frame(matrix(NA, nrow = 1, ncol = 7))
# names(p_res) <- c("Name", "Model", "WAIC_wt", "LOOIC_wt", "Performance_Score",
#                   "subject_id","bfm")

model_compare_res <-  vector(mode = "list", length = 75)

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
    mutate(wtvd = left_wtrating - left_wtrating)

  if (subject_idx %in% c(35, 37)) { # remove the one subject that removes all the trials
    next
  }
  m1 <- glm(choice ~ left_rating + right_rating, family = binomial, data = temp_df)
  m2 <- glm(choice ~ left_wtrating + right_wtrating , family = binomial, data = temp_df)
  
  # m1 <- lm(log(rt) ~ left_rating+right_rating, data = temp_df)
  # m2 <- lm(log(rt) ~ left_wtrating+right_wtrating , data = temp_df)
  
  subj_weights_models[[counter_sub]][[counter_weight]] <- list(m1, m2)
  
  # m1 <- brm(choice ~ left_rating + right_rating, data = temp_df, family = "bernoulli", cores = 4, iter = 10000)
  # m2 <- brm(choice ~ left_wtrating + right_wtrating, data = temp_df, family = "bernoulli", cores = 4, iter = 10000)
  # m1_waic <- waic(m1)
  # m2_waic <- waic(m2)
  # loo_compare(loo(m1),loo(m2))
  # loo_compare(loo(m1),loo(m2))

  res_temp1 <- broom::glance(m1)
  res_temp1$m <- 1
  res_temp2 <- broom::glance(m2)
  res_temp2$m <- 2
  res_temp <- rbind(res_temp1, res_temp2)
  res_temp$subject_id <- subject_idx

  # p_res_temp <- performance::compare_performance(m1,m2, rank = TRUE, metrics = c("WAIC","LOOIC"))
  p_res_temp <- performance::compare_performance(m1,m2, rank = TRUE)
  
  p_res_temp$subject_id <- subject_idx

  if (p_res_temp$Performance_Score[p_res_temp$Name == "m1"] > p_res_temp$Performance_Score[p_res_temp$Name == "m2"]){
    p_res_temp$bfm <- 1
  }else{
    p_res_temp$bfm <- 0
  }
  p_res <- rbind(p_res, p_res_temp)
  model_compare_res[[subject_idx]] <- res_temp
  
  counter_sub <- counter_sub + 1
  
}

test <- do.call(rbind,model_compare_res)

test %>%   ggplot(aes(x = factor(subject_id),y = BIC, fill = factor(m)))+
  geom_col(position="dodge",width = 1)

  
p_res %>%
  na.omit() %>%
  # filter(!subject_id %in% as.numeric(na.omit(p_values))) %>%
  ggplot(aes(reorder(factor(subject_id),bfm), factor(bfm)))+
  geom_point()+
  theme_classic()

p_temp_2 <- p_res %>%
  na.omit() %>%
  # filter(!subject_id %in% as.numeric(na.omit(p_values))) %>%
  ggplot(aes(x = factor(subject_id),y = BIC_wt, fill = factor(Name, 
                                                              levels = c("m1", "m2"),
                                                              labels = c("un-weighted", 
                                                                         "weighted"))))+
  geom_col()+
  # geom_col(position="dodge",width = 1)+
  geom_hline(yintercept = .5, size = 1.4)+
  theme_classic()+coord_flip()+
  labs(y = "BIC wt",x = "Subject",fill = "model",
       title = paste0("experiment two model comparisons for : ", weights[[weight_idx]])
  )
# print(p_temp_2)
# p_res %>%
#   na.omit() %>%
#   # filter(!subject_id %in% as.numeric(na.omit(p_values))) %>%
#   ggplot(aes(x = factor(subject_id),y = LOOIC_wt, fill = Name))+
#   geom_col()+
#   geom_hline(yintercept = .5)+
#   theme_classic()+coord_flip()

#########experiment one
df <- organize_group_data(experiment = 1, net_stat = "conductance", weight = weights[[weight_idx]])

## model comparisons with weighted average models
p_res <- data.frame(matrix(NA, nrow = 1, ncol = 14))
names(p_res) <- c("Name", "Model", "R2_Tjur", "RMSE", "Sigma", "Log_loss", "Score_log",
                  "Score_spherical", "PCP", "AIC_wt", "BIC_wt", "Performance_Score",
                  "subject_id","bfm")
# p_res <- data.frame(matrix(NA, nrow = 1, ncol = 9))
# names(p_res) <- c("Name", "Model", "R2", "R2_adjusted", "RMSE", "Sigma", "Performance_Score",
#                   "subject_id", "bfm")

# p_res <- data.frame(matrix(NA, nrow = 1, ncol = 7))
# names(p_res) <- c("Name", "Model", "WAIC_wt", "LOOIC_wt", "Performance_Score",
#                   "subject_id","bfm")

model_compare_res <-  vector(mode = "list", length = 30)

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
    mutate(wtvd = left_wtrating - left_wtrating)
  
  m1 <- glm(choice ~ left_rating + right_rating, family = binomial, data = temp_df)
  m2 <- glm(choice ~ left_wtrating + right_wtrating , family = binomial, data = temp_df)

  # m1 <- lm(log(rt) ~ left_rating+right_rating, data = temp_df)
  # m2 <- lm(log(rt) ~ left_wtrating+right_wtrating , data = temp_df)
  
  
  subj_weights_models[[counter_sub]][[counter_weight]] <- list(m1, m2)
  
  # m1 <- brm(choice ~ left_rating + right_rating, data = temp_df, family = "bernoulli", cores = 4, iter = 10000)
  # m2 <- brm(choice ~ left_wtrating + right_wtrating, data = temp_df, family = "bernoulli", cores = 4, iter = 10000)
  # m1_waic <- waic(m1)
  # m2_waic <- waic(m2)
  # loo_compare(loo(m1),loo(m2))
  # loo_compare(loo(m1),loo(m2))
  # m1 <- lm(log(rt) ~ left_rating+right_rating, data = temp_df)
  # m2 <- lm(log(rt) ~ left_wtrating+right_wtrating , data = temp_df)
  
  res_temp1 <- broom::glance(m1)
  res_temp1$m <- 1
  res_temp2 <- broom::glance(m2)
  res_temp2$m <- 2
  res_temp <- rbind(res_temp1, res_temp2)
  res_temp$subject_id <- subject_idx
  
  p_res_temp <- performance::compare_performance(m1,m2, rank = TRUE)
  p_res_temp$subject_id <- subject_idx
  
  if (p_res_temp$Performance_Score[p_res_temp$Name == "m1"] > p_res_temp$Performance_Score[p_res_temp$Name == "m2"]){
    p_res_temp$bfm <- 1
  }else{
    p_res_temp$bfm <- 0
  }
  p_res <- rbind(p_res, p_res_temp)
  model_compare_res[[subject_idx]] <- res_temp
 
  counter_sub <- counter_sub + 1
}

test <- do.call(rbind,model_compare_res)

p_res %>%
  na.omit() %>%
  # filter(!subject_id %in% as.numeric(na.omit(p_values))) %>%
  ggplot(aes(reorder(factor(subject_id),bfm), factor(bfm)))+
  geom_point()+
  theme_classic()

p_temp_1 <- p_res %>%
  na.omit() %>%
  # filter(!subject_id %in% as.numeric(na.omit(p_values))) %>%
  ggplot(aes(x = factor(subject_id),y = BIC_wt, fill = factor(Name, 
                                                              levels = c("m1", "m2"),
                                                              labels = c("un-weighted", 
                                                                         "weighted"))))+
  geom_col()+
  # geom_col(position="dodge",width = 1)+
  geom_hline(yintercept = .5, size = 1.4)+
  theme_classic()+coord_flip()+
  labs(y = "BIC wt",x = "Subject",fill = "model",
       title = paste0("experiment one model comparisons for : ", weights[[weight_idx]])
  ) 
# print(p_temp_1)

counter_sub <- 1
counter_weight <- counter_weight + 1

}
#############################################
#####   compare across all models  ##########
#############################################

p_res <- data.frame(matrix(NA, nrow = 1, ncol = 6))
names(p_res) <- c("Name", "Model", "AIC_wt", "BIC_wt", "Performance_Score",
                  "subject_id")

for (i in seq_len(length(subj_weights_models))){
  
  if (is.null(subj_weights_models[[i]][[1]])) {
    next
  }
  
  #compare each subject across all the models
  m0 <- subj_weights_models[[i]][[1]][[1]] #model base
  m1 <- subj_weights_models[[i]][[1]][[2]] #weighted degree
  m2 <- subj_weights_models[[i]][[2]][[2]] #weighted strength
  m3 <- subj_weights_models[[i]][[3]][[2]] #weighted weighted_transitivity
  m4 <- subj_weights_models[[i]][[4]][[2]] #weighted eigen
  m5 <- subj_weights_models[[i]][[5]][[2]] #weighted closeness
  m6 <- subj_weights_models[[i]][[6]][[2]] #weighted betweenness
  
  models <- list(m0,m1,m2,m3,m4,m5,m6)
  
  # print(performance::compare_performance(models, rank = TRUE, metrics = c("AIC","BIC")))
  # print(parameters::compare_models(models,  style = "ci_p"))
  print(performance::test_performance(models))
  
  # mp <- modelplot(models, coef_omit = "Interc") +
  #   geom_vline(xintercept = 0, linetype = "dashed") +
  #   labs(x = "Coefficients",y = "Terms") +
  #   theme_classic() +
  #   scale_color_brewer(palette = "Set1")
  # print(mp)
  
  p_res_temp <- performance::compare_performance(models, rank = TRUE, metrics = c("AIC","BIC"))
  p_res_temp$subject_id <- i
  
  p_res <- rbind(p_res, p_res_temp)
  # p_res_temp <- performance::compare_performance(models, rank = TRUE)
  # p_res_temp$subject_id <- subject_idx
}


p_res %>%
  na.omit() %>%
  ggplot(aes(x = factor(subject_id),y = BIC_wt, fill = factor(Name, 
                                                              levels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7"),
                                                              labels = c("un-weighted",
                                                                         "weighted degree",
                                                                         "weighted strength",
                                                                         "weighted transitivity",
                                                                         "weighted eigen",
                                                                         "weighted closeness",
                                                                         "weighted betweenness"))))+
  geom_col()+
  theme_classic()+coord_flip()+
  labs(y = "BIC wt",x = "Subject",fill = "model")+
  scale_fill_brewer(palette = "Set1")

p_res %>%
  arrange(subject_id, desc(BIC_wt)) %>% 
  group_by(subject_id) %>% 
  slice_max(BIC_wt) %>% 
  ungroup() %>% 
  group_by(Name) %>% 
  summarise(nbest = n())

# 1 Model 1    49
# 2 Model 2     3
# 3 Model 3    15
# 4 Model 4    15
# 5 Model 5     5
# 6 Model 6     9
# 7 Model 7     7

#w/o within set normalization
# 1 Model 1    19
# 2 Model 2     9
# 3 Model 3    21
# 4 Model 4    19
# 5 Model 5    14
# 6 Model 6    11
# 7 Model 7    10



#weights are computed as w = exp(-0.5 * delta_ic) / sum(exp(-0.5 * delta_ic)), 
#where delta_ic is the difference between the model's IC value and the smallest IC value in the model set (Burnham and Anderson, 2002)


