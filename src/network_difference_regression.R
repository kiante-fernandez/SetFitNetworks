# network_difference_regression_analysis.R - algorithm for selecting sub graphs from preference network

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
# 10/28/22      Kianté  Fernandez                       coded up version one

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
  df <- organize_group_data(net_stat = net_stats[[net_idx]])

  ## make a plot of the vd:nd interaction
  plt <- df %>%
    exlusions() %>%
    group_by(subject_id) %>%
    mutate(
      vd = left_rating - right_rating,
      nd = left_net - right_net
    ) %>%
    mutate(
      binned_value_diff = as.numeric(cut_number(vd, 7)) - 4,
    ) %>%
    group_by(subject_id, binned_value_diff) %>%
    mutate(
      binned_net_diff = as.numeric(cut_number(nd, 3)) - 2
    ) %>%
    group_by(binned_net_diff, binned_value_diff) %>%
    mutate(
      n = n(),
      m_left = mean(choice),
      se = sqrt(var(choice) / length(choice))
    ) %>%
    ungroup() %>%
    ggplot(aes(x = binned_value_diff, y = m_left, color = factor(binned_net_diff))) +
    geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
    theme_classic() +
    geom_line(size = 1) +
    geom_hline(yintercept = .5, linetype = "dashed") +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(limits = c(0, 1.01)) +
    labs(
      title = paste0(net_stats[[net_idx]]),
      y = "Probability of Choosing Left",
      x = "Value Difference (L-R) bins",
      color = "Network Difference (L-R) bins"
    ) +
    theme(legend.position = "none")
  #
  print(plt)
  plts[[net_idx]] <- plt

  df %>%
    exlusions() %>%
    group_by(subject_id) %>%
    mutate(
      vd = abs(left_rating - right_rating),
      nd = abs(left_net - right_net)
    ) %>%
    mutate(
      binned_value_diff = as.numeric(cut_number(vd, 6)) - 1,
    ) %>%
    group_by(subject_id, binned_value_diff) %>%
    mutate(
      binned_net_diff = as.numeric(cut_number(nd, 2)) - 1
    ) %>%
    group_by(binned_net_diff, binned_value_diff) %>%
    mutate(
      n = n(),
      m_rt = mean(rt),
      se = sqrt(var(rt) / length(rt))
    ) %>%
    ungroup() %>%
    ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(binned_net_diff))) +
    geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
    theme_classic() +
    geom_line(size = 1) +
    scale_color_brewer(palette = "Set1") +
    labs(
      y = "RT(ms)",
      x = "Abs Value Difference (L-R)",
      color = "Abs Network Difference (L-R)"
    )

  df %>%
    exlusions() %>%
    group_by(subject_id) %>%
    mutate(
      vd = left_rating + right_rating,
      nd = left_net + right_net
    ) %>%
    mutate(
      binned_value_diff = as.numeric(cut_number(vd, 6)) - 1,
    ) %>%
    group_by(subject_id, binned_value_diff) %>%
    mutate(
      binned_net_diff = as.numeric(cut_number(nd, 2)) - 1
    ) %>%
    group_by(binned_net_diff, binned_value_diff) %>%
    mutate(
      n = n(),
      m_rt = mean(rt),
      se = sqrt(var(rt) / length(rt))
    ) %>%
    ungroup() %>%
    ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(binned_net_diff))) +
    geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
    theme_classic() +
    geom_line(size = 1) +
    scale_color_brewer(palette = "Set1") +
    labs(
      y = "RT(ms)",
      x = "Value Magnitude (L+R)",
      color = "Network Magnitude (L+R)"
    )

  # next
  # correct absolute value plot
  print(df %>%
    exlusions() %>%
    group_by(subject_id) %>%
    # mutate(eq = left_cluster_condition == right_cluster_condition) %>%
    # filter(eq == 1) %>%
    mutate(
      vd = abs(left_rating - right_rating),
      nd = abs(left_net - right_net)
    ) %>%
    mutate(
      binned_value_diff = as.numeric(cut_number(vd, 4)) - 1,
    ) %>%
    group_by(binned_value_diff) %>%
    mutate(
      binned_net_diff = as.numeric(cut_number(nd, 6)) - 1
    ) %>%
    group_by(binned_net_diff, binned_value_diff) %>%
    mutate(
      n = n(),
      m_correct = mean(correct),
      se = sqrt(var(correct) / length(correct))
    ) %>%
    ungroup() %>%
    ggplot(aes(x = binned_net_diff, y = m_correct, color = factor(binned_value_diff))) +
    geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
    theme_classic() +
    geom_line(size = 1) +
    geom_hline(yintercept = .5, linetype = "dashed") +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(limits = c(0, 1.01)) +
    labs(
      title = paste0(net_stats[[net_idx]]),
      y = "Accuracy",
      x = "Absolute Network Difference (L-R)",
      color = "Absolute Value Difference (L-R)"
    ) +
    theme(legend.position = "top"))


  print(df %>%
    exlusions() %>%
    group_by(subject_id) %>%
    # mutate(eq = left_cluster_condition == right_cluster_condition) %>%
    # filter(eq == 0) %>%
    mutate(nd = abs(left_net - right_net)) %>%
    mutate(
      binned_net_diff = as.numeric(cut_number(nd, 10)) - 1
    ) %>%
    group_by(binned_net_diff) %>%
    mutate(
      n = n(),
      m_correct = mean(correct),
      se = sqrt(var(correct) / length(correct))
    ) %>%
    ungroup() %>%
    ggplot(aes(x = binned_net_diff, y = m_correct)) +
    geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
    theme_classic() +
    geom_line(size = 1) +
    geom_hline(yintercept = .5, linetype = "dashed") +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(limits = c(0, 1.01)) +
    labs(
      title = paste0(net_stats[[net_idx]]),
      y = "Accuracy",
      x = "Absolute Network Difference (L-R)"
    ) +
    theme(legend.position = "top"))

  # ##make a plot of the vd:nd interaction
  # plt <- df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>%
  #   mutate(vd = left_rating - right_rating,
  #          nd = left_net - right_net) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd,5)) - 3,
  #   ) %>%
  #   group_by(subject_id,binned_value_diff) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,3 )) - 2
  #   ) %>%
  #   group_by(binned_net_diff,binned_value_diff) %>%
  #   mutate(n = n(),
  #          m_left = mean(choice),
  #          se = sqrt(var(choice) / length(choice))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_value_diff, y = m_left, color = factor(binned_net_diff))) +
  #   geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   geom_hline(yintercept = .5, linetype = "dashed") +
  #   scale_color_brewer(palette = "Set1") +
  #   scale_y_continuous(limits = c(0, 1.01)) +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #     y = "Probability of Choosing Left",
  #     x = "Value Difference (L-R) bins",
  #     color = "Network Difference (L-R) bins"
  #   )
  # # print(plt)
  # df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>%
  #   mutate(vd = abs(left_rating - right_rating),
  #          nd = abs(left_net - right_net)) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,3)) - 1
  #       ) %>%
  #   group_by(subject_id,binned_net_diff) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd,5)) - 1,
  #   ) %>%
  #   group_by(binned_net_diff,binned_value_diff) %>%
  #   mutate(n = n(),
  #          m_c = mean(correct),
  #          se = sqrt(var(correct) / length(correct))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_value_diff, y = m_c, color = factor(binned_net_diff))) +
  #   geom_pointrange(aes(ymin = m_c - se, ymax = m_c + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   geom_hline(yintercept = .5, linetype = "dashed") +
  #   scale_color_brewer(palette = "Set1") +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #        y = "Accuracy",
  #        x = "Absolute Value Difference (L-R)",
  #        color = "Absolute Network Difference (L-R)"
  #   )
  # df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>%
  #   mutate(vd = abs(left_rating - right_rating),
  #          nd = abs(left_net - right_net)) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd,5)) - 1,
  #   ) %>%
  #   group_by(subject_id,binned_value_diff) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,3)) - 1
  #   ) %>%
  #   group_by(binned_net_diff,binned_value_diff) %>%
  #   mutate(n = n(),
  #          m_rt = mean(rt),
  #          se = sqrt(var(rt) / length(rt))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(binned_net_diff))) +
  #   geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   scale_color_brewer(palette = "Set1") +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #     y = "RT(ms)",
  #     x = "Absolute Value Difference (L-R)",
  #     color = "Absolute Network Difference (L-R)"
  #   )
  # df %>%
  #   exlusions() %>%
  #   group_by(subject_id) %>%
  #   mutate(vd = left_rating + right_rating,
  #          nd = left_net + right_net) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd,5)) - 1,
  #   ) %>%
  #   group_by(subject_id,binned_value_diff) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,3)) - 1
  #   ) %>%
  #   group_by(binned_net_diff,binned_value_diff) %>%
  #   mutate(n = n(),
  #          m_rt = mean(rt),
  #          se = sqrt(var(rt) / length(rt))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(binned_net_diff))) +
  #   geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   scale_color_brewer(palette = "Set1") +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #        y = "RT(ms)",
  #        x = "Absolute Value Magnitude (L+R)",
  #        color = "Absolute Network Magnitude (L+R)"
  #   )
  #
  # print(plt)
  #
  # plts[[net_idx]] <- plt

  # #correct absolute value plot
  # print(df %>%
  #   exlusions() %>%
  #   # mutate(eq = left_cluster_condition == right_cluster_condition) %>%
  #   # filter(eq == 1) %>%
  #   mutate(vd = abs(left_rating - right_rating),
  #          nd = abs(left_net - right_net)) %>%
  #   mutate(
  #     binned_value_diff = as.numeric(cut_number(vd,4)) - 1,
  #   ) %>%
  #   group_by(binned_value_diff) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,4)) - 1
  #   ) %>%
  #   group_by(binned_net_diff,binned_value_diff) %>%
  #   mutate(n = n(),
  #          m_correct = mean(correct),
  #          se = sqrt(var(correct) / length(correct))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_net_diff, y = m_correct, color = factor(binned_value_diff))) +
  #   geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   geom_hline(yintercept = .5, linetype = "dashed") +
  #   scale_color_brewer(palette = "Set1") +
  #   scale_y_continuous(limits = c(0, 1.01)) +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #        y = "Accuracy",
  #        x = "Absolute Network Difference (L-R)",
  #        color = "Absolute Value Difference (L-R)"
  #   ) +  theme(legend.position="top"))
  # #
  # #
  # print(df %>%
  #   exlusions() %>%
  #   # mutate(eq = left_cluster_condition == right_cluster_condition) %>%
  #   # filter(eq == 0) %>%
  #   mutate(nd = abs(left_net - right_net)) %>%
  #   mutate(
  #     binned_net_diff = as.numeric(cut_number(nd,4)) - 1
  #   ) %>%
  #   group_by(binned_net_diff) %>%
  #   mutate(n = n(),
  #          m_correct = mean(correct),
  #          se = sqrt(var(correct) / length(correct))
  #   ) %>%
  #   ungroup() %>%
  #   ggplot(aes(x = binned_net_diff, y = m_correct)) +
  #   geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
  #   theme_classic() +
  #   geom_line(size = 1) +
  #   geom_hline(yintercept = .5, linetype = "dashed") +
  #   scale_color_brewer(palette = "Set1") +
  #   scale_y_continuous(limits = c(0, 1.01)) +
  #   labs(title = paste0(net_stats[[net_idx]]),
  #        y = "Accuracy",
  #        x = "Absolute Network Difference (L-R)"
  #   ) +  theme(legend.position="top"))

  #### data analysis

  model_dat <- create_dataset(df, type = "choice")

  #### choice

  # mlm2_0 <- glmer(choice ~ vd + ov + (vd + ov | subject_id), data = model_dat,
  #               family=binomial(link="logit"),
  #               control=glmerControl(optimizer="bobyqa",
  #                                    optCtrl=list(maxfun=2e5)))
  # mlm2_1 <- glmer(choice ~ vd + ov + nd + (vd + ov + nd| subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_2 <- glmer(choice ~ vd + ov + nd + on + (vd + ov + nd + on| subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  #TODO just make it so that we has a list of model strings 
  #     then make the code in the utils use the strings, but remove the sd for exp 1 models
 #somthing like the example below ()it has bugso
   # formula <- "choice ~  vd + ov + nd + sd + (vd + ov + nd + sd | subject_id)"
  # 
  # stringr::str_remove(formula, "sd")
  
  formula <- "choice ~ vd * nd + ov * on + (vd * nd + ov * on | subject_id)"


  mlm2_3 <- glmer(formula,
    data = model_dat,
    family = binomial(link = "logit"),
    control = glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 2e5)
    )
  )
  summary(mlm2_3)
  # mlm2_4 <- glmer(choice ~ vd*nd + ov*on + (vd*nd + ov*on | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_5 <- glmer(choice ~ vd*nd + ov*on + cd + (vd*nd + ov*on + cd | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_6 <- glmer(choice ~ vd*nd + ov*on + sds + ( vd*nd + ov*on + sds | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_7 <- glmer(choice ~ vd*nd + ov*on + sds + cd + ( vd*nd + ov*on + sds + cd| subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))

  file_name <- here::here("tables", paste0("choice_", net_stats[[net_idx]], ".html"))

  print(tab_model(mlm2_0, mlm2_1, mlm2_2, mlm2_3, mlm2_4, mlm2_5, mlm2_6, mlm2_7,
    show.intercept = F,
    show.aic = T,
    show.re.var = F,
    show.ci = FALSE,
    dv.labels = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8"),
    pred.labels = c(
      "Value Difference (vd)", "Overall Value (ov)",
      "Nework Difference (nd)", "Overall Network (on)",
      "vd:nd", "ov:on", "Correlation Difference",
      "Standard-Deviation Difference"
    ),
    file = file_name
  ))

  print(tab_model(mlm2_3,
    show.intercept = T,
    show.aic = T,
    show.re.var = F,
    show.ci = F,
    digits = 4,
    dv.labels = paste0(net_stats[[net_idx]]),
    pred.labels = c(
      "Intercept", "Value Difference (vd)", "Overall Value (ov)",
      "Nework Difference (nd)", "Overall Network (on)",
      "vd:nd"
    ),
    file = file_name
  ))

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

  model_dat <- create_dataset(df, type = "correct/rt")
  
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
  summary(mlm1_2)
  # mlm1_3 <- lmer(log(rt)  ~ vd*nd + ov + on + (vd*nd + ov + on | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm1_4 <- lmer(log(rt)  ~ vd*nd + ov*on + (vd*nd + ov*on | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm1_5 <- lmer(log(rt)  ~ vd*nd + ov*on + cd + (vd*nd + ov*on + cd | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm1_6 <- lmer(log(rt)  ~ vd*nd + ov*on + sds + ( vd*nd + ov*on + sds | subject_id), data = model_dat,
  #                 control=lmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm1_7 <- lmer(log(rt)  ~ vd*nd + ov*on + sds + cd + ( vd*nd + ov*on + sds + cd| subject_id), data = model_dat,
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
      optCtrl = list(maxfun = 2e5)
    )
  )
  summary(mlm2_3)
  # mlm2_4 <- glmer(correct ~ vd*nd + ov*on + (vd*nd + ov*on | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_5 <- glmer(correct ~ vd*nd + ov*on + cd + (vd*nd + ov*on + cd | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_6 <- glmer(correct ~ vd*nd + ov*on + sds + ( vd*nd + ov*on + sds | subject_id), data = model_dat,
  #                 family=binomial(link="logit"),
  #                 control=glmerControl(optimizer="bobyqa",
  #                                      optCtrl=list(maxfun=2e5)))
  # mlm2_7 <- glmer(correct ~ vd*nd + ov*on + sds + cd + ( vd*nd + ov*on + sds + cd| subject_id), data = model_dat,
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

# try bayes
# library(brms)
#
# df <- organize_group_data(net_stat = net_stats[[5]])
# model_dat <- df %>%
#   exlusions() %>%
#   group_by(subject_id) %>%
#   mutate(
#     nd = scale(left_net - right_net),
#     vd = scale(left_rating - right_rating),
#     ov = scale(left_rating + right_rating),
#     on = scale(left_net + right_net)
#   )
#
# fit <- brm(choice ~ vd*nd + ov*on +  (vd*nd + ov*on| subject_id), data = model_dat, family = "bernoulli", cores = 10, iter = 10000)
# summary(fit)
# bayestestR::sexit(fit, significant = "default", large = "default", ci = 0.95)
#
# plot(fit)
# report::report(fit)

# plot(ggeffects::ggpredict(fit, terms = c("vd [all]", "nd [-2, -1, 0, 1, 2]"), type = "simulate"))
# #check correlations
# df %>%
#   exlusions() %>%
#   mutate(vd = left_rating - right_rating,
#          nd = left_net - right_net,
#          cd = left_correlation - right_correlation
#   ) %>%
#   select(vd, nd, cd) %>%
#   correlation::correlation() %>%
#   print()
#
#
# #when you look at comparisons of groups between the stim, you do not get the effect
#
# ###response times
# model_dat <- df %>%
#   exlusions() %>%
#   mutate(eq = left_cluster_condition == right_cluster_condition) %>%
#   group_by(subject_id) %>%
#   mutate(
#     nd = scale(abs(left_net - right_net)),
#     vd = scale(abs(left_rating - right_rating)),
#     cd = scale(abs(left_correlation - right_correlation))
#   )
# model_dat$eq = factor(model_dat$eq,labels = c("Distinct", "Same")) #groups of items sampled
#
# mlm1 <- lmer(log(rt) ~ vd*nd + (vd*nd | subject_id), data = model_dat,
#              control=lmerControl(optimizer="bobyqa",
#                                   optCtrl=list(maxfun=2e5)))
# mlm1 <- lmer(log(rt) ~ vd*nd*degree_condition + (vd*nd*degree_condition| subject_id), data = model_dat,
#              control=lmerControl(optimizer="bobyqa",
#                                  optCtrl=list(maxfun=2e5)))
# summary(mlm1)
#
# # mlm1 <- lmer(log(rt) ~  poly(vd,degree = 2, raw = TRUE)*poly(nd,degree = 2, raw = TRUE) + (vd*nd| subject_id), data = model_dat)
# # mlm1 <- lmer(log(rt) ~ poly(vd,degree = 2, raw = TRUE)*poly(nd,degree = 2, raw = TRUE)*cluster_condition*eq + (1| subject_id), data = model_dat)
# mlm1 <- lmer(log(rt) ~ vd*nd*eq  + (vd*nd*eq| subject_id), data = model_dat,
#              control=lmerControl(optimizer="bobyqa",
#                                  optCtrl=list(maxfun=2e5)))
# # mlm1 <- lmer(log(rt) ~ vd*nd*cd + (vd*nd*cd| subject_id), data = model_dat)
# summary(mlm1)
(plts[[1]] | plts[[2]] | plts[[3]] | plts[[4]]) / (plts[[5]] | plts[[6]] | plts[[7]] | plts[[8]])
# (plts[[1]] | plts[[2]] |  plts[[3]] |  plts[[4]])/(plts[[5]] | plts[[6]] |  plts[[7]] |plts[[8]])
# (plts[[1]] | plts[[2]] |  plts[[3]] |  plts[[4]] | plts[[5]])

#
# library(brms)
#
# df <- organize_group_data(net_stat = net_stats[[1]])
# ###code it as correct incorrect instead
# df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))
#
# model_dat <- df %>%
#   exlusions() %>%
#   group_by(subject_id) %>%
#   mutate(
#     nd = scale(abs(left_net - right_net)),
#     vd = scale(abs(left_rating - right_rating)),
#     ov = scale(left_rating + right_rating),
#     on = scale(left_net + right_net)
#   ) %>%
#   select(choice,correct,vd,nd,ov,on,rt,subject_id)
#
# fit<- brm(correct ~ pca1*pca2 +  (pca1*pca2 | subject_id), data = model_dat, family = "bernoulli", cores = 10,
#           iter = 10000)
# summary(fit)
# bayestestR::sexit(fit, significant = "default", large = "default", ci = 0.95)

# plot(fit)

# pca_res <- prcomp(model_dat[,c("vd","nd")])
# summary(pca_res)
# pca_res
# biplot(pca_res)
# model_dat$pca1 <- pca_res$x[,1]
# model_dat$pca2 <- pca_res$x[,2]

# report::report(fit)
# plot(ggeffects::ggpredict(fit, terms = c("nd[all]", "vd")))
# #see the the frame work example
# #https://easystats.github.io/bayestestR/reference/sexit.html#:~:text=The%20SEXIT%20is%20a%20new,parameters%20under%20a%20Bayesian%20framework.
# bayestestR::sexit(fit, significant = "default", large = "default", ci = 0.95)
