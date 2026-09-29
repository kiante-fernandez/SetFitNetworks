# rt_individual_values_regression.R - Exp 1-3 RT regressions with left/right set values
# entered separately (zleft_rating + zright_rating) instead of |vd| + ov.
# Same participants/trials as the published PC2 RT fits. Usage: Rscript src/rt_individual_values_regression.R [1|2|3]

library(tidyverse)
library(jsonlite)
library(brms)
library(cmdstanr)

source(here::here("src", "utils.R"))
source(here::here("src", "exploratory_graph_analysis.R"))
net_degree <- calculate_net_stats(g)

# Usage: Rscript src/rt_individual_values_regression.R <exp> [variant]
#   indval   - left/right set values in place of |vd| + ov (default)
#   ov_lr    - single overall value term; PC2 (and similarity) as left/right terms
#   vd_ov_lr - as ov_lr but keeping the absolute value difference
args <- commandArgs(TRUE); exps <- if (length(args)) as.integer(args[1]) else 1:3
variant <- if (length(args) > 1) args[2] else "indval"

ref_fit <- c("pca2_exp_1_fit_rt02", "pca2_exp_2_fit_rt02", "strength_exp_3_fit_rt02")

f_all <- list(
  indval = list(
    log(rt) ~ zleft_rating + zright_rating + nd1 + (zleft_rating + zright_rating + nd1 | subject_id),
    log(rt) ~ zleft_rating + zright_rating + nd1 + sd + (zleft_rating + zright_rating + nd1 + sd | subject_id),
    log(rt) ~ zleft_rating + zright_rating + nd1 + (zleft_rating + zright_rating + nd1 | subject_id)),
  ov_lr = list(
    log(rt) ~ ov + zleft_net1 + zright_net1 + (ov + zleft_net1 + zright_net1 | subject_id),
    log(rt) ~ ov + zleft_net1 + zright_net1 + zleft_sim + zright_sim + (ov + zleft_net1 + zright_net1 + zleft_sim + zright_sim | subject_id),
    log(rt) ~ ov + zleft_net1 + zright_net1 + (ov + zleft_net1 + zright_net1 | subject_id)),
  vd_ov_lr = list(
    log(rt) ~ vd + ov + zleft_net1 + zright_net1 + (vd + ov + zleft_net1 + zright_net1 | subject_id),
    log(rt) ~ vd + ov + zleft_net1 + zright_net1 + zleft_sim + zright_sim + (vd + ov + zleft_net1 + zright_net1 + zleft_sim + zright_sim | subject_id),
    log(rt) ~ vd + ov + zleft_net1 + zright_net1 + (vd + ov + zleft_net1 + zright_net1 | subject_id)))
f_rt <- f_all[[variant]]
result_stem <- if (variant == "indval") "individual_values" else variant

# create_dataset() calls exlusions() by name; reproduce the published sample
exlusions <- function(df) {
  df %>%
    filter(subject_id %in% keep_subjects) %>%
    group_by(subject_id) %>%
    mutate(Q1 = quantile(rt, .25), Q3 = quantile(rt, .75), IQR = IQR(rt)) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    ungroup() %>%
    filter(rt > 250, rt < 9000)
}

summarise_fit <- function(m, e) {
  as_draws_df(m) %>%
    select(starts_with("b_")) %>%
    pivot_longer(everything(), names_to = "term") %>%
    group_by(term) %>%
    summarise(estimate = mean(value), se = sd(value),
              lower = quantile(value, .025), upper = quantile(value, .975),
              pd = max(mean(value > 0), mean(value < 0)), .groups = "drop") %>%
    mutate(outcome = "rt", experiment = e, .before = 1)
}

for (e in exps) {
  ref <- readRDS(here::here("fits", paste0(ref_fit[e], ".rds")))
  keep_subjects <- unique(ref$data$subject_id)

  df <- organize_group_data(experiment = e)
  df$left_net1 <- df$left_net_pca2;  df$right_net1 <- df$right_net_pca2
  df$left_net2 <- df$left_net_pca2;  df$right_net2 <- df$right_net_pca2

  d_rt <- create_dataset(df, type = "correct/rt")
  stopifnot(nrow(d_rt) == nrow(ref$data))  # identical trials to the published fit

  m_rt <- brm(f_rt[[e]], data = d_rt,
              iter = 10000, chains = 4, cores = 4, backend = "cmdstanr",
              file = here::here("fits", paste0("pca2_exp_", e, "_", variant, "_rt")))
  print(summary(m_rt))
  write_csv(summarise_fit(m_rt, e), here::here("results", paste0("rt_", result_stem, "_exp", e, ".csv")))
}
