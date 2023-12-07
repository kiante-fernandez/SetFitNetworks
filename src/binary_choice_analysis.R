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
# 08/12/06      Kianté  Fernandez                       added interaction

# Load necessary libraries
library(here)
library(tidyverse)
library(purrr)
library(lme4)
library(lmerTest)
library(patchwork)

library(sjPlot)
library(magrittr)
library(ggeffects)

# Uncomment below if needed
library(brms)
library(rstantools)
library(cmdstanr)

# Load data

##binary data
Lee_Hare_2023_choice_data_exp2 <- read_csv("data/Lee_Hare_2023_OSF/Lee_Hare_2023_choice_data_exp2.csv")
Lee_Holyoak_2021_choice_data_exp2_5 <- read_csv("data/lee_2021_exp2_5.csv")

Lee_Hare_2023_choice_data_exp2$choice_type = "binary"
Lee_Holyoak_2021_choice_data_exp2_5$choice_type = "binary"

#set data 
set_exp1 <- readr::read_csv("data/ISDN_poster_exp1.csv")
set_exp2 <- readr::read_csv("data/ISDN_poster_exp2.csv")
set_exp3 <- readr::read_csv("data/ISDN_poster_exp3.csv")

set_exp1$subject_id <- set_exp1$subject_id + 100
set_exp2$subject_id <- set_exp2$subject_id + 200
set_exp3$subject_id <- set_exp3$subject_id + 300

set_exp1$study <- 1
set_exp2$study <- 2
set_exp3$study <- 3

cols_select <- c("study","left", "right", "subject_id", "rt", "choice",
                 "left_rating", "right_rating","left_net_pca1", "right_net_pca1", "left_net_pca2", "right_net_pca2" )

set_df <- rbind(set_exp1[,cols_select], set_exp2[,cols_select], set_exp3[,cols_select])
set_df$choice_type = "set"

set_df <- set_df %>% select(subject_id, rt, choice, choice_type, left_rating, right_rating,
                            left_net_pca1, right_net_pca1, 
                            left_net_pca2, right_net_pca2)
# Source functions for network analysis
source("exploratory_graph_analysis.R")
source(here::here("src", "utils.R"))

# Calculate network statistics
net_degree <- calculate_net_stats(g)

##%######################################################%##
#                                                          #
####               Lee, D. G., & Hare, T.               ####
####  A. (2023). Value certainty and choice confidence  ####
####          are multidimensional constructs           ####
####            that guide decision-making.             ####
####                Cognitive, Affective,               ####
####                    & Behavioral                    ####
####                   Neuroscience.                    ####
####     https://doi.org/10.3758/s13415-022-01054-4     ####
#                                                          #
##%######################################################%##

# Prepare and mutate data
binary_df <- Lee_Hare_2023_choice_data_exp2 %>%
  mutate(rt = rt * 1000) %>%
  rowwise() %>%
  mutate(
    name_left = net_degree$Name[net_degree$Image == item_number_left],
    name_right = net_degree$Name[net_degree$Image == item_number_right],
    left_net_pca1 = net_degree$PCA1[net_degree$Image == item_number_left],
    right_net_pca1 = net_degree$PCA1[net_degree$Image == item_number_right],
    left_net_pca2 = net_degree$PCA2[net_degree$Image == item_number_left],
    right_net_pca2 = net_degree$PCA2[net_degree$Image == item_number_right],
    choice = if_else(choice == 1, 0, 1), # Reverse choice coding
    left_rating = item_value_left,
    right_rating= item_value_right
  ) %>% 
  select(subject_id, rt, choice, choice_type, left_rating, right_rating,
         left_net_pca1, right_net_pca1, 
         left_net_pca2, right_net_pca2)

full_df <- rbind(binary_df, set_df)
# Response time exclusions
# rt_exclude_pct <- vector("numeric", length(unique(df$subject_id)))
# for (subject_idx in 1:length(unique(df$subject_id))) {
#   temp_df <- df %>%
#     ungroup() %>%
#     filter(subject_id == subject_idx) %>%
#     mutate(
#       Q1 = quantile(rt, .25),
#       Q3 = quantile(rt, .75),
#       IQR = IQR(rt)
#     ) %>%
#     filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
#     filter(rt > 250 & rt < 9000) %>% # Apply response time cutoffs
#     summarise(pct_excluded = (30 - n()) / 30)
# 
#   rt_exclude_pct[[subject_idx]] <- temp_df$pct_excluded
# 
#   # Warning for high exclusion rates
#   if (temp_df$pct_excluded > 0.40) {
#     cat("######## subject:", subject_idx, "#######\n")
#     cat("######## percent trials excluded:", temp_df$pct_excluded, "#######\n")
#   }
# }

# Calculate mean exclusion percentage
# mean(rt_exclude_pct)

# Function for data exclusions
exclusions <- function(df) {
  df %>%
    # filter(!subject_id %in% c(1, 6, 17, 19, 27, 28, 32, 37, 38, 42, 44, 45, 46, 51, 56, 71, 73, 74, 86, 91, 92)) %>%
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

full_df$choice_type <- factor(full_df$choice_type)
for_model <- full_df %>%
  exclusions() %>%
  group_by(subject_id) %>%
  mutate(
    zleft_rating = scale(left_rating, center = standardized, scale = standardized),
    zright_rating = scale(right_rating, center = standardized, scale = standardized),
    zleft_net1 = scale(left_net_pca1, center = standardized, scale = standardized),
    zright_net1 = scale(right_net_pca1, center = standardized, scale = standardized),
    zleft_net2 = scale(left_net_pca2, center = standardized, scale = standardized),
    zright_net2 = scale(right_net_pca2, center = standardized, scale = standardized),
    nd1 = scale(abs(left_net_pca1 - right_net_pca1), center = standardized, scale = standardized),
    nd2 = scale(abs(left_net_pca2 - right_net_pca2), center = standardized, scale = standardized),
    vd = scale(abs(left_rating - right_rating), center = standardized, scale = standardized),
    ov = scale(left_rating + right_rating, center = standardized, scale = standardized)
  ) %>%
  ungroup() %>%
  select(choice_type, subject_id, choice, rt, zleft_rating, zright_rating, zleft_net1, zright_net1, zleft_net2, zright_net2, vd, nd1, nd2, ov)

# Model for choice
# models_choice <- glmer(
#   choice ~ zleft_rating * (zleft_net1 + zleft_net2) + zright_rating * (zright_net1 + zright_net2) +
#     (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 + zleft_net2 + zright_net2 | subject_id),
#   data = for_model,
#   family = binomial(link = "logit"),
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7))
# )
# # Model for response time
# models_rt <- lmer(
#   log(rt) ~ vd + ov + nd1 + nd2 + (vd + ov + nd1 + nd2 | subject_id),
#   data = for_model,
#   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )

models_choice1 <- brm(choice ~ zleft_rating*(zleft_net1 + zleft_net2) + zright_rating*(zright_net1 + zright_net2) +
                                  choice_type*zleft_rating + choice_type*zright_rating + 
                                  choice_type*zleft_net1 + choice_type*zleft_net2+
                                  choice_type*zright_net1 + choice_type*zright_net2+
                       (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 +  zleft_net2 + zright_net2 | subject_id), 
                     data = for_model, family = "bernoulli", iter = 10000, 
                     chains = 4, cores = 4, backend = "cmdstanr", threads = threading(2),
                     file = here::here("fits", paste0("PCA", "_Lee_Hare_2023_choice_data_exp2_fit_choice04")))

models_rt1 <- brm(log(rt) ~ choice_type*(vd + ov + nd1 + nd2) +
                   (vd + ov + nd1 + nd2 | subject_id), 
                 data = for_model, iter = 10000, 
                 chains = 4, cores = 4, backend = "cmdstanr", threads = threading(2),
                 file = here::here("fits", paste0("PCA", "_Lee_Hare_2023_choice_data_exp2_fit_rt03")))

# Output model summaries
summary(models_choice1)
summary(models_rt1)

# bayestestR::sexit(models_choice1)
bayestestR::sexit(models_rt1)
plot(ggeffects::ggpredict(models_rt1, 
                          terms = c("vd[all]", "choice_type")))
plot(ggeffects::ggpredict(models_rt1, 
                          terms = c("ov[all]", "choice_type")))
plot(ggeffects::ggpredict(models_rt1, 
                          terms = c("nd1[all]", "choice_type")))
plot(ggeffects::ggpredict(models_rt1, 
                          terms = c("nd2[all]", "choice_type")))
plot(ggeffects::ggpredict(models_choice1, 
                          terms = c("zleft_rating[all]", "choice_type")))+
  theme_classic()+
  # scale_y_continuous(limits = c(.25, .8))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0.5, linetype = "dashed")+
  labs(
    title = "",
    y = "Probability of Choosing Left",
    x = "left item-rating",
    color = "Choice type"
  ) +
  theme(text = element_text(size = 15),
        legend.position = c(0.25, 0.85),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))

# plot(ggeffects::ggpredict(models_choice1, 
#                           terms = c("zleft_net1[all]", "choice_type")))
plot(ggeffects::ggpredict(models_choice1, 
                          terms = c("zleft_net2[all]", "choice_type")))+
  theme_classic()+
  scale_y_continuous(limits = c(.25, .8))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0.5, linetype = "dashed")+
  labs(
    title = "",
    y = "Probability of Choosing Left",
    x = "left item-score",
    color = "Choice type"
  ) +
  theme(text = element_text(size = 15),
        legend.position = c(0.85, 0.85),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))

plot(ggeffects::ggpredict(models_choice1, 
                          terms = c("zright_net2[all]", "choice_type")))+
  theme_classic()+
  scale_y_continuous(limits = c(.25, .8))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0.5, linetype = "dashed")+
  labs(
    title = "",
    y = "Probability of Choosing Left",
    x = "right item-score",
    color = "Choice type"
  ) +
  theme(text = element_text(size = 15),
        legend.position = c(0.85, 0.85),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))
  
  
##%######################################################%##
#                                                          #
####      Lee, D. G., & Holyoak, K. J. Coherence       ####
####          shifts in attribute evaluations.          ####
####                  Decision, 8(4),                  ####
####      257. https://doi.org/10.1037/dec0000151       ####
#                                                          #
##%######################################################%##


# Calculate network statistics

dat_pca1 <- net_degree[,c("degree","strength","eigen","weighted_transitivity","closeness","betweenness")]
rownames(dat_pca1) <- net_degree$Name
pca_res <- prcomp(dat_pca1, center = TRUE, scale. = TRUE)

# Source functions for network new association network
source("fernandez_rating_network.R") #load the other network

net_degree <- calculate_net_stats(g)
dat_pca2 <- net_degree[,c("degree","strength","eigen","weighted_transitivity","closeness","betweenness")]
#do projection
project.b = predict(pca_res, dat_pca2)
#replace the scores with the projection scores. 
# net_degree$PCA1 <-  pca_res$x[,1] * -1 #change the scale w/ linear transformation
net_degree$PCA1 <-  project.b[,1]
net_degree$PCA2 <- project.b[,2]
net_degree$PCA3 <- project.b[,3]
net_degree$PCA4 <-  project.b[,4]
net_degree$PCA5 <- project.b[,5]
net_degree$PCA6 <- project.b[,6]

# Prepare and mutate data
df2 <- Lee_Holyoak_2021_choice_data_exp2_5 %>%
  mutate(rt = rt * 1000) %>%
  rowwise() %>%
  mutate(
    name_left = net_degree$Name[net_degree$Image == item_name_left],
    name_right = net_degree$Name[net_degree$Image == item_name_right],
    PCA1_left = net_degree$PCA1[net_degree$Image == item_name_left],
    PCA1_right = net_degree$PCA1[net_degree$Image == item_name_right],
    PCA2_left = net_degree$PCA2[net_degree$Image == item_name_left],
    PCA2_right = net_degree$PCA2[net_degree$Image == item_name_right],
    weighted_transitivity_left = net_degree$weighted_transitivity[net_degree$Image == item_name_left],
    weighted_transitivity_right = net_degree$weighted_transitivity[net_degree$Image == item_name_right],
    degree_left = net_degree$degree[net_degree$Image == item_name_left],
    degree_right = net_degree$degree[net_degree$Image == item_name_right],
    strength_left = net_degree$strength[net_degree$Image == item_name_left],
    strength_right = net_degree$strength[net_degree$Image == item_name_right],
    closeness_left = net_degree$closeness[net_degree$Image == item_name_left],
    closeness_right = net_degree$closeness[net_degree$Image == item_name_right],
    choice = if_else(response == 1, 0, 1) # Reverse choice coding
  )

for_model <- df2 %>%
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
  select(experiment, subject_id, choice, rt, zleft_rating, zright_rating, zleft_net1, zright_net1, zleft_net2, zright_net2, vd, nd1, nd2, ov, )

# Model for choice
# models_choice <- glmer(
#   choice ~ zleft_rating * (zleft_net1 + zleft_net2) + zright_rating * (zright_net1 + zright_net2) +
#     (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 + zleft_net2 + zright_net2 | subject_id),
#   data = for_model,
#   family = binomial(link = "logit"),
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7))
# )
models_choice2 <- brm(choice ~ zleft_rating*(zleft_net1 + zleft_net2) + zright_rating*(zright_net1 + zright_net2) +
                       (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 +  zleft_net2 + zright_net2 | subject_id), 
                     data = for_model, family = "bernoulli", iter = 10000, 
                     chains = 4, cores = 4, backend = "cmdstanr", threads = threading(2),
                     file = here::here("fits", paste0("PCA", "_Lee_Holyoak_2021_choice_data_exp2_5_fit_choice03")))
models_rt2 <- brm(log(rt) ~ vd + ov + nd1 + nd2 +
                   (vd + ov + nd1 + nd2 | subject_id), 
                 data = for_model, iter = 10000, 
                 chains = 4, cores = 4, backend = "cmdstanr", threads = threading(2),
                 file = here::here("fits", paste0("PCA", "_Lee_Holyoak_2021_choice_data_exp2_5_fit_rt02")))
# Model for response time
# models_rt <- lmer(
#   log(rt) ~ vd + ov + nd1 + nd2 + (vd + ov + nd1 + nd2 | subject_id),
#   data = for_model,
#   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# )

# Output model summaries
# summary(models_choice2)
# summary(models_rt2)

pca_Lee_Hare_2023_fit_choice03 <- readRDS("~/Documents/SetFitNetworks/fits/PCA_Lee_Hare_2023_choice_data_exp2_fit_choice03.rds")
pca_Lee_Holyoak_2021_fit_choice03 <- readRDS("~/Documents/SetFitNetworks/fits/PCA_Lee_Holyoak_2021_choice_data_exp2_5_fit_choice03.rds")

test <- plot_models(pca_Lee_Hare_2023_fit_choice03,
                    pca_Lee_Holyoak_2021_fit_choice03,
                    transform = NULL,
                    show.values = TRUE,
                    show.p = FALSE,
                    m.labels = c("Lee & Hare 2023", "Lee & Holyoak 2021"),
                    ci.lvl = 0.95)
# write.csv(test$data, here::here("data","internal_meta_analysis_choice.csv"), row.names=FALSE)

pd <- position_dodge(1)
plt_data <- test$data
plt_data$term <- factor(plt_data$term)
dput(levels(plt_data$term))
levels(plt_data$term) <- c("right rating × PCA2", "right rating × PCA1", 
                           "left rating × PCA2", "left rating × PCA1", "right PCA2", 
                           "right PCA1", "right liking rating", "left PCA2", "left PCA1", 
                           "left liking rating", "intercept")
plt_data %>% 
  dplyr::filter(term != "intercept") %>% 
  dplyr::mutate(estimate =  round(estimate, 2),
                conf.low = round(conf.low, 2),
                conf.high = round(conf.high, 2)) %>% 
  ggplot(aes(y = forcats::fct_reorder(term, estimate), color = group)) +
  theme_classic()+
  geom_point(aes(x=estimate), shape=15, size=2,position = pd) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), position = pd, size=.7)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = .4)+
  scale_color_brewer(palette = "Set2")+
  labs(
    y = "terms",
    x = "estimate",
    color = ""
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        axis.title = element_text(face="bold")
  )+
  geom_text(aes( label = paste0(estimate, " [", conf.low,",",conf.high, "]"), 
                 x = estimate, y = term, group = group, color = group), 
            position = pd, vjust = -0.7,size=3,
            show.legend = FALSE, check_overlap = FALSE)

pca_Lee_Hare_2023_fit_rt02 <- readRDS("~/Documents/SetFitNetworks/fits/PCA_Lee_Hare_2023_choice_data_exp2_fit_rt02.rds")
pca_Lee_Holyoak_2021_fit_rt2 <- readRDS("~/Documents/SetFitNetworks/fits/PCA_Lee_Holyoak_2021_choice_data_exp2_5_fit_rt02.rds")

test <- plot_models(pca_Lee_Hare_2023_fit_rt02,
                    pca_Lee_Holyoak_2021_fit_rt2,
                    transform = NULL,
                    show.values = TRUE,
                    show.p = FALSE,
                    m.labels = c("Lee & Hare 2023", "Lee & Holyoak 2021"),
                    ci.lvl = 0.95)

pd <- position_dodge(1)
plt_data <- test$data
dput(levels(plt_data$term))
plt_data$term <- factor(plt_data$term)
levels(plt_data$term) <- c("PCA2 Difference", "PCA1 Difference", 
                           "Overall Value", "Value Difference", "intercept")
plt_data %>% 
  dplyr::filter(term != "intercept") %>% 
  dplyr::mutate(estimate =  round(estimate, 2),
                conf.low = round(conf.low, 2),
                conf.high = round(conf.high, 2)) %>% 
  ggplot(aes(y = forcats::fct_reorder(term, estimate), color = group)) +
  theme_classic()+
  geom_point(aes(x=estimate), shape=15, size=2,position = pd) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), position = pd, size=.7)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = .4)+
  scale_color_brewer(palette = "Set2")+
  labs(
    y = "terms",
    x = "estimate",
    color = ""
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        axis.title = element_text(face="bold")
  )+
  geom_text(aes( label = paste0(estimate, " [", conf.low,",",conf.high, "]"), 
                 x = estimate, y = term, group = group, color = group), 
            position = pd, vjust = -0.7,size=3,
            show.legend = FALSE, check_overlap = FALSE)


bayestestR::sexit(pca_Lee_Hare_2023_fit_choice03)
bayestestR::sexit(pca_Lee_Holyoak_2021_fit_choice03)
bayestestR::sexit(pca_Lee_Hare_2023_fit_rt02)
bayestestR::sexit(pca_Lee_Holyoak_2021_fit_rt2)

