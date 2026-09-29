# set_variance_regression.R - Exp 1-3 choice & RT regressions with within-set rating variance
#
# Adds each participant's variance of their own ratings within the left and right
# sets (left_VAR / right_VAR from organize_group_data) to the PC2 models used in
# the main text. Participant samples are taken from the published fits so the
# trials are identical. Usage: Rscript src/set_variance_regression.R [1|2|3]

library(tidyverse)
library(jsonlite)
library(brms)
library(cmdstanr)

source(here::here("src", "utils.R"))
source(here::here("src", "exploratory_graph_analysis.R"))
net_degree <- calculate_net_stats(g)

exps <- if (length(commandArgs(TRUE))) as.integer(commandArgs(TRUE)) else 1:3

ref_fit <- c("pca2_exp_1_fit_choice03", "pca2_exp_2_fit_choice03", "strength_exp_3_fit_choice03")

# Same fixed/random structure as the published fits per experiment, plus variance terms
f_choice <- list(
  choice ~ zleft_rating * zleft_net1 + zright_rating * zright_net1 + zleft_var + zright_var +
    (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 + zleft_var + zright_var | subject_id),
  choice ~ zleft_rating * zleft_net1 + zright_rating * zright_net1 + zleft_var + zright_var +
    (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 + zleft_var + zright_var | subject_id),
  choice ~ zleft_rating * zleft_net1 + zright_rating * zright_net1 + zleft_var + zright_var +
    (1 + zleft_rating * zleft_net1 + zright_rating * zright_net1 + zleft_var + zright_var | subject_id)
)
f_rt <- list(
  log(rt) ~ vd + ov + nd1 + vard + ovar + (vd + ov + nd1 + vard + ovar | subject_id),
  log(rt) ~ vd + ov + nd1 + sd + vard + ovar + (vd + ov + nd1 + sd + vard + ovar | subject_id),
  log(rt) ~ vd + ov + nd1 + vard + ovar + (vd + ov + nd1 + vard + ovar | subject_id)
)

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

summarise_fit <- function(m, outcome, e) {
  as_draws_df(m) %>%
    select(starts_with("b_")) %>%
    pivot_longer(everything(), names_to = "term") %>%
    group_by(term) %>%
    summarise(estimate = mean(value), se = sd(value),
              lower = quantile(value, .025), upper = quantile(value, .975),
              pd = max(mean(value > 0), mean(value < 0)), .groups = "drop") %>%
    mutate(outcome = outcome, experiment = e, .before = 1)
}

for (e in exps) {
  ref <- readRDS(here::here("fits", paste0(ref_fit[e], ".rds")))
  keep_subjects <- unique(ref$data$subject_id)

  df <- organize_group_data(experiment = e)
  df$left_net1 <- df$left_net_pca2;  df$right_net1 <- df$right_net_pca2
  df$left_net2 <- df$left_net_pca2;  df$right_net2 <- df$right_net_pca2

  d_choice <- create_dataset(df, type = "choice")
  stopifnot(nrow(d_choice) == nrow(ref$data))  # identical trials to the published fit

  m_choice <- brm(f_choice[[e]], data = d_choice, family = "bernoulli",
                  iter = 10000, chains = 4, cores = 4, backend = "cmdstanr",
                  file = here::here("fits", paste0("pca2_exp_", e, "_var_choice")))
  print(summary(m_choice))

  d_rt <- create_dataset(df, type = "correct/rt")
  m_rt <- brm(f_rt[[e]], data = d_rt,
              iter = 10000, chains = 4, cores = 4, backend = "cmdstanr",
              file = here::here("fits", paste0("pca2_exp_", e, "_var_rt")))
  print(summary(m_rt))

  bind_rows(summarise_fit(m_choice, "choice", e), summarise_fit(m_rt, "rt", e)) %>%
    write_csv(here::here("results", paste0("set_variance_exp", e, ".csv")))
}
