# binary_choice_analysis.R - Binary-Choice Studies 1-2: choice and RT regressions on item centrality.
# Study 1 (Lee & Hare, 2023) uses the Rating Study 1 network. Study 2 (Lee & Holyoak, 2021) chose between the Rating
# Study 1 items, so to avoid circularity its centralities come from the Set-Choice ratings network, projected onto the
# Rating Study 1 principal components. net1/nd1 = PC1, net2/nd2 = PC2; RT exclusions as in the set-choice studies.
#
# Copyright (C) 2023 Kianté Fernandez, <kiantefernan@gmail.com>. GPL-3.0.

library(here)
library(tidyverse)
library(brms)
library(cmdstanr)

source(here("src", "exploratory_graph_analysis.R"))  # Rating Study 1 network (g)
source(here("src", "utils.R"))

exclusions <- function(df) {
  df %>%
    group_by(subject_id) %>%
    mutate(Q1 = quantile(rt, .25), Q3 = quantile(rt, .75), IQR = IQR(rt)) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    ungroup() %>%
    filter(rt > 250 & rt < 9000)
}

# choices -> RT exclusions -> within-participant z-scores -> choice and RT models (fits/PCA_<name>_fit_*)
fit_binary <- function(choices, net, name) {
  pc <- function(item, k) net[[k]][match(item, net$Image)]
  d <- choices %>%
    mutate(rt = rt * 1000, choice = if_else(choice == 1, 0, 1),  # recoded so 1 = chose left
           l1 = pc(item_number_left, "PCA1"), r1 = pc(item_number_right, "PCA1"),
           l2 = pc(item_number_left, "PCA2"), r2 = pc(item_number_right, "PCA2")) %>%
    exclusions() %>%
    group_by(subject_id) %>%
    mutate(zleft_rating = scale(item_value_left), zright_rating = scale(item_value_right),
           zleft_net1 = scale(l1), zright_net1 = scale(r1), zleft_net2 = scale(l2), zright_net2 = scale(r2),
           nd1 = scale(abs(l1 - r1)), nd2 = scale(abs(l2 - r2)),
           vd = scale(abs(item_value_left - item_value_right)), ov = scale(item_value_left + item_value_right)) %>%
    ungroup()
  list(
    choice = brm(choice ~ zleft_rating * (zleft_net1 + zleft_net2) + zright_rating * (zright_net1 + zright_net2) +
                   (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 + zleft_net2 + zright_net2 | subject_id),
                 data = d, family = "bernoulli", iter = 10000, chains = 4, cores = 4, backend = "cmdstanr",
                 threads = threading(2), file = here("fits", paste0("PCA_", name, "_fit_choice03"))),
    rt = brm(log(rt) ~ vd + ov + nd1 + nd2 + (vd + ov + nd1 + nd2 | subject_id),
             data = d, iter = 10000, chains = 4, cores = 4, backend = "cmdstanr",
             threads = threading(2), file = here("fits", paste0("PCA_", name, "_fit_rt02"))))
}

measures <- c("degree", "strength", "eigen", "weighted_transitivity", "closeness", "betweenness")
net_rs1 <- calculate_net_stats(g)
pca_rs1 <- prcomp(net_rs1[, measures], center = TRUE, scale. = TRUE)

source(here("src", "fernandez_rating_network.R"))    # replaces g with the Set-Choice ratings network
net_set <- calculate_net_stats(g)
net_set[c("PCA1", "PCA2")] <- predict(pca_rs1, net_set[, measures])[, 1:2]

study1 <- fit_binary(read_csv(here("data", "Lee_Hare_2023_OSF", "Lee_Hare_2023_choice_data_exp2.csv")), net_rs1,
                     "Lee_Hare_2023_choice_data_exp2")
study2 <- fit_binary(read_csv(here("data", "lee_2021_exp2_5_v2.csv")), net_set, "Lee_Holyoak_2021_choice_data_exp2_5")
