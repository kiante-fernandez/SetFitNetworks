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

##### loading the data#####
temp_files <- list.files(path = here::here("data", "exp_2"), pattern = ".json", full.names = T)
#load subgraphs
load(file = here::here("data", "modularity_100_6.RData"))

network_stats <- c("LowHighWithinBetween","modularity")

load_food_names()

# get correlations between items
lee_2021_rating1 <- read_csv(here::here("data", "lee_2021_rating1.csv"), col_names = FALSE)
cor.snack_food <- SemNeT::similarity(lee_2021_rating1, method = "cor")
cor_snack_food <- data.frame(matrix(cor.snack_food[cor.snack_food != 1], 59, 60))
names(cor_snack_food) <- load_food_names()$FoodNames$Name


######
# calculate a bunch of network measures to look at relationship to stuff
source("exploratory_graph_analysis.R")

G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
path_lengths <- distances(G)
diag(path_lengths) <- NA # path length to oneself is zero

adj_temp <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")

# here I calculate a range of metrics on the graph
net_degree <- data.frame(
  degree = degree(g),
  strength = strength(g),
  eigen = igraph::eigen_centrality(G)$vector,
  page_rank = page_rank(g)$vector, # weighted
  weighted_transitivity = transitivity(g, type = "weighted"),
  closeness = NetworkToolbox::closeness(adj_temp, weighted = TRUE),
  closeness2 = closeness(G), # weighted
  betweenness = betweenness(G),
  participation = NetworkToolbox::participation(adj_temp, comm = V(g)$snack_type)$overall,
  sds = apply(cor_snack_food, 2, sd)
) %>%
  tibble::rownames_to_column("Name") %>%
  dplyr::left_join(load_food_names()$foods_in_image, "Name")

net_degree$snack_type <- V(g)$snack_type

file_idx <- 75
net_stat <- "modularity"
df <- organize_group_data()
#check for exclusions
for (subject_idx in 1:75){
  
  temp_df <- df %>% 
    filter(subject_id == subject_idx) %>% 
    mutate(Q1 = quantile(rt, .25),
           Q3 = quantile(rt, .75),
           IQR = IQR(rt)) %>%
    filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
    filter(!rt <= 300) %>% # response times cutoffs
    filter(!rt >= 9000) %>% 
    summarise(pct_excluded = (100 - n())/100)
  if (temp_df$pct_excluded > 0.2){
    print(paste0("######## subject: ", subject_idx, " #######"))
    # print(paste0("######## percent trials excluded: ", temp_df$pct_excluded, " #######"))
  }
}

##value difference exclusion
p_values <- vector(mode = "numeric", length = 75)
for (subject_idx in 1:75){

  if(subject_idx %in% c(17,29,35,37,38,56,65,68)){
    p_values[[subject_idx]] <- NA
    next}  
  df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))
  
  temp_df <- df %>% 
    filter(subject_id == subject_idx) %>% 
    mutate(Q1 = quantile(rt, .25),
           Q3 = quantile(rt, .75),
           IQR = IQR(rt)) %>%
    filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
    filter(!rt <= 300) %>% # response times cutoffs
    filter(!rt >= 9000) %>% 
    mutate(
      vd = scale(abs(left_rating - right_rating)))
  
  # print(paste0("######## subject: ", subject_idx, " #######"))
  # print(summary(glm(correct ~ vd*nd, family = binomial, data = temp_df)))
  # print(broom::tidy(glm(correct ~ vd*nd, family = binomial, data = temp_df)))
  # temp_res <- broom::tidy(glm(choice ~ vd, family = binomial, data = temp_df))
  temp_res <- broom::tidy(glm(correct ~ vd, family = binomial, data = temp_df))
  
  # temp_res <- broom::tidy(lm(rt ~ eq, data = temp_df))
  temp_res$p.value <-  round(temp_res$p.value, 4)
  if (temp_res[2,5][[1]] > 0.05){ #check p-value
    # print(paste0("######## subject: ", unique(temp_df$subject_id), " #######"))
    # print(unique(temp_df$subject_id))
    # print(paste0("######## percent trials excluded: ", temp_df$pct_excluded, " #######"))
    p_values[[subject_idx]] <- unique(temp_df$subject_id)

  }else (  p_values[[subject_idx]] <- NA)
}
#so far we have a 40%! exclusion rate...
# as.numeric(na.omit(p_values))

exlusions <- function(df) {
  # function for data exclusions
  temp <- df %>%
     filter(subject_id != 17) %>%
     filter(subject_id != 29) %>%
     filter(subject_id != 35) %>%
     filter(subject_id != 37) %>%
     filter(subject_id != 38) %>%
     filter(subject_id != 56) %>%
     filter(subject_id != 65) %>%
     filter(subject_id != 68) %>% #rt exclusions
    # filter(!subject_id %in%  as.numeric(na.omit(p_values))) %>%
    group_by(subject_id) %>% # response times
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

plts <- vector("list", length = length(net_stats))

net_idx <- 4
for (net_idx in 1:length(net_stats)) {
  print(paste0("############### ", net_stats[[net_idx]], " ###############"))
  df <- organize_group_data(net_stat = net_stats[[net_idx]])
  
  ### code it as correct incorrect instead
  df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))
  
  #### data analysis
  
  model_dat <- df %>%
    exlusions() %>%
    mutate(
      nd = scale(left_net - right_net),
      vd = scale(left_rating - right_rating),
      cd = scale(left_correlation - right_correlation),
      sds = scale(left_sd - right_sd),
      ov = scale(left_rating + right_rating),
      on = scale(left_net + right_net)
    )
  # without normalization
  # model_dat <- df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>% #what if we do variable wise standardization?
  #   mutate(
  #     nd = (left_net - right_net),
  #     vd = (left_rating - right_rating),
  #     cd = (left_correlation - right_correlation),
  #     sds = (left_sd - right_sd),
  #     ov = (left_rating + right_rating),
  #     on = (left_net + right_net)
  #   )
  
  # pca_res <- prcomp(model_dat[,c("nd","vd","ov","on")],center = T, scale. = T)
  # summary(pca_res)
  # plot(pca_res)
  # biplot(pca_res, scale = 0,choices = c(1,2))
  # model_dat$PC1 <- pca_res$x[,1]
  # model_dat$PC2 <- pca_res$x[,3]
  
  # check correlations
  # model_dat %>%
  #   ungroup() %>%
  #   select(vd, nd, cd,sds,ov,on) %>%
  #   correlation::correlation() %>%
  #   print()
  
  #### choice
  
  mlm2_0 <- glmer(choice ~ vd + ov + (vd + ov | subject_id), data = model_dat,
                family=binomial(link="logit"),
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
  mlm2_1 <- glmer(choice ~ vd + ov + nd + (vd + ov + nd| subject_id), data = model_dat,
                  family=binomial(link="logit"),
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5)))
  mlm2_2 <- glmer(choice ~ vd + ov + nd + on + (vd + ov + nd + on| subject_id), data = model_dat,
                  family=binomial(link="logit"),
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5)))
  
  mlm2_3 <- glmer(choice ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id),data = model_dat,
                  family = binomial(link = "logit"),
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)))
  
  mlm2_4 <- glmer(choice ~ vd*nd + ov*on + (vd*nd + ov*on | subject_id), data = model_dat,
                  family=binomial(link="logit"),
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5)))
  
  file_name <- here::here("tables", paste0("exp_2_choice_", net_stats[[net_idx]], ".html"))

  # print(tab_model(mlm2_3,
  #                 show.intercept = T,
  #                 show.aic = T,
  #                 show.re.var = F,
  #                 show.ci = F,
  #                 digits = 4,
  #                 dv.labels = paste0(net_stats[[net_idx]]),
  #                 pred.labels = c(
  #                   "Intercept", "Value Difference (vd)", "Overall Value (ov)",
  #                   "Nework Difference (nd)", "Overall Network (on)",
  #                   "vd:nd"
  #                 ),
  #                 file = file_name
  # ))
  # get_prior(choice ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id), data = model_dat, family = "bernoulli", cores = 10, iter = 10000)
  fit1 <- brm(choice ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id), data = model_dat, family = "bernoulli", cores = 10, iter = 10000)
  # summary(fit1)
  print(bayestestR::sexit(fit1, significant = "default", large = "default", ci = 0.95))
  
  
  # report::report(mlm2_3)
  # print(tab_model(mlm2_0,mlm2_1,mlm2_2,mlm2_3,mlm2_4,
  #           show.intercept = F,
  #           show.aic = T,
  #           show.re.var = F,
  #           show.ci = FALSE,
  #           digits = 4,
  #           dv.labels = c("M1", "M2", "M3", "M4", "M5"),
  #           pred.labels = c("Value Difference (vd)", "Overall Value (ov)",
  #                           "Nework Difference (nd)", "Overall Network (on)",
  #                           "vd:nd","ov:on"
  #                           ),
  #           file = file_name))
  
  # summary(mlm2_3)
  # temp_res <- broom.mixed::tidy(mlm2_4)
  # temp_res$p.value <-  round(temp_res$p.value, 4)
  # print(knitr::kable(temp_res[temp_res$effect == "fixed",3:7],digits = 3,
  #                    caption = paste0("Choice ",net_stats[[net_idx]])))
  #### RT
  model_dat <- df %>%
    exlusions() %>%
    group_by(subject_id) %>%
    mutate(
      nd = scale(abs(left_net - right_net)),
      vd = scale(abs(left_rating - right_rating)),
      cd = scale(abs(left_correlation - right_correlation)),
      sds = scale(abs(left_sd - right_sd)),
      ov = scale(left_rating + right_rating),
      on = scale(left_net + right_net)
    )
  
  # mlm1_0 <- lmer(log(rt) ~ vd + ov + (vd + ov | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm1_1 <- lmer(log(rt)  ~ vd + ov + nd + (vd + ov + nd| subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  mlm1_2 <- lmer(log(rt) ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id),
                 data = model_dat,
                 control = lmerControl(
                   optimizer = "bobyqa",
                   optCtrl = list(maxfun = 2e5)
                 )
  )
  # mlm1_3 <- lmer(log(rt)  ~ vd*nd + ov + on + (vd*nd + ov + on | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm1_4 <- lmer(log(rt)  ~ vd*nd + ov*on + (vd*nd + ov*on | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))

  
  file_name <- here::here("tables", paste0("rt_", net_stats[[net_idx]], ".html"))
  
  print(tab_model(mlm1_2,
                  show.intercept = T,
                  show.aic = T,
                  show.re.var = F,
                  show.ci = FALSE,
                  show.icc = FALSE,
                  digits = 4,
                  dv.labels = paste0(net_stats[[net_idx]]),
                  pred.labels = c(
                    "Intercept", "Value Difference (vd)", "Overall Value (ov)",
                    "Nework Difference (nd)", "Overall Network (on)"
                  ),
                  file = file_name
  ))
  
  fit2 <- brm(log(rt) ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id), data = model_dat, cores = 10, iter = 10000)
  # summary(fit2)
  print(bayestestR::sexit(fit2, significant = "default", large = "default", ci = 0.95))
  # print(tab_model(mlm1_0,mlm1_1,mlm1_2,mlm1_3,mlm1_4,
  #           show.intercept = F,
  #           show.aic = T,
  #           show.re.var = F,
  #           show.ci = FALSE,
  #           digits = 4,
  #           dv.labels = c("M1", "M2", "M3", "M4", "M5"),
  #           pred.labels = c("Value Difference (vd)", "Overall Value (ov)",
  #                           "Nework Difference (nd)", "Overall Network (on)",
  #                           "vd:nd","ov:on"),
  #           file = file_name))
  
  
  model_dat <- df %>%
    exlusions() %>%
    # group_by(subject_id) %>%
    mutate(
      nd = scale(abs(left_net - right_net)),
      vd = scale(abs(left_rating - right_rating)),
      cd = scale(abs(left_correlation - right_correlation)),
      sds = scale(abs(left_sd - right_sd)),
      ov = scale(left_rating + right_rating),
      on = scale(left_net + right_net)
    ) %>%
    select(correct, vd, nd, cd, sds, ov, on, subject_id)
  
  # mlm2_0 <- glmer(correct ~ vd + ov + (vd + ov | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_1 <- glmer(correct ~ vd + ov + nd + (vd + ov + nd| subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_2 <- glmer(correct ~ vd + ov + nd + on + (vd + ov + nd + on| subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  mlm2_3 <- glmer(correct ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id),
                  data = model_dat,
                  family = binomial(link = "logit"),
                  control = glmerControl(
                    optimizer = "bobyqa",
                    optCtrl = list(maxfun = 2e5)))

  # mlm2_4 <- glmer(correct ~ vd*nd + ov*on + (vd*nd + ov*on | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))

  # create table for the correct incorrect model
  file_name <- here::here("tables", paste0("correct_", net_stats[[net_idx]], ".html"))
  print(tab_model(mlm2_3,
                  show.intercept = T,
                  show.aic = T,
                  show.re.var = F,
                  show.ci = FALSE,
                  digits = 4,
                  dv.labels = paste0(net_stats[[net_idx]]),
                  pred.labels = c(
                    "Intercept", "Value Difference (vd)", "Overall Value (ov)",
                    "Nework Difference (nd)", "Overall Network (on)",
                    "vd:nd"
                  ),
                  file = file_name
  ))
  
  fit3 <- brm(correct ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id), data = model_dat, family = "bernoulli", cores = 10, iter = 10000)
  # summary(fit3)
  print(bayestestR::sexit(fit3, significant = "default", large = "default", ci = 0.95))
  
  # print(tab_model(mlm2_0,mlm2_1,mlm2_2,mlm2_3,mlm2_4,
  #                 show.intercept = F,
  #                 show.aic = T,
  #                 show.re.var = F,
  #                 show.ci = FALSE,
  #                 digits = 4,
  #                 dv.labels = c("M1", "M2", "M3", "M4", "M5"),
  #                 pred.labels = c("Value Difference (vd)", "Overall Value (ov)",
  #                                 "Nework Difference (nd)", "Overall Network (on)",
  #                                 "vd:nd","ov:on"
  #                 ),
  #                 file = file_name))
  
}

