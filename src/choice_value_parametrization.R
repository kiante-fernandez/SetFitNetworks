# choice_value_parametrization.R - Does coding value as separate left/right set values fit set choice
# better or worse than a single value-difference term? Leave-one-out comparison on the published trials.
#   M1: choice ~ zleft_rating + zright_rating            M2: choice ~ vd            (value only)
#   M3: main-text model (left/right values x left/right PC2)   M4: choice ~ vd * nd   (difference formulation)
# Also reports the left/right value asymmetry (b_left + b_right) from the main-text model.
# Usage: Rscript src/choice_value_parametrization.R [1|2|3]
library(tidyverse); library(jsonlite); library(brms); library(cmdstanr)
source(here::here("src", "utils.R")); source(here::here("src", "exploratory_graph_analysis.R"))
net_degree <- calculate_net_stats(g)
exps <- if (length(commandArgs(TRUE))) as.integer(commandArgs(TRUE)) else 1:3
ref_fit <- c("pca2_exp_1_fit_choice03", "pca2_exp_2_fit_choice03", "pca2_exp_3_fit_choice03")
exlusions <- function(df) df %>% filter(subject_id %in% keep_subjects) %>% group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25), Q3 = quantile(rt, .75), IQR = IQR(rt)) %>%
  filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>% ungroup() %>% filter(rt > 250, rt < 9000)
fit <- function(f, d, name) brm(f, data = d, family = "bernoulli", iter = 10000, chains = 4, cores = 4, backend = "cmdstanr", file = here::here("fits", name))

for (e in exps) {
  m3 <- readRDS(here::here("fits", paste0(ref_fit[e], ".rds"))); keep_subjects <- unique(m3$data$subject_id)
  df <- organize_group_data(experiment = e)
  df$left_net1 <- df$left_net_pca2; df$right_net1 <- df$right_net_pca2; df$left_net2 <- df$left_net_pca2; df$right_net2 <- df$right_net_pca2
  d <- create_dataset(df, type = "choice"); stopifnot(nrow(d) == nrow(m3$data))
  m1 <- fit(choice ~ zleft_rating + zright_rating + (1 + zleft_rating + zright_rating | subject_id), d, paste0("pca2_exp_", e, "_valueLR_choice"))
  m2 <- fit(choice ~ vd + (1 + vd | subject_id), d, paste0("pca2_exp_", e, "_valueVD_choice"))
  m4 <- fit(choice ~ vd * nd + (1 + vd + nd | subject_id), d, paste0("pca2_exp_", e, "_vdnd_choice"))
  l <- map(list(M1_leftright = m1, M2_vd = m2, M3_maintext = m3, M4_vd_x_nd = m4), loo)
  elpd <- map_dfr(l, ~tibble(elpd = .x$estimates["elpd_loo", 1], se = .x$estimates["elpd_loo", 2]), .id = "model") %>% mutate(experiment = e, .before = 1)
  cmp_value <- loo_compare(l$M1_leftright, l$M2_vd); cmp_full <- loo_compare(l$M3_maintext, l$M4_vd_x_nd)
  asym <- as_draws_df(m3) %>% transmute(s = b_zleft_rating + b_zright_rating) %>% pull(s)
  cat(sprintf("\n=== Exp %d ===\n", e)); print(elpd); cat("value-only comparison:\n"); print(cmp_value); cat("full-model comparison:\n"); print(cmp_full)
  cat(sprintf("left/right value asymmetry (b_left + b_right) from main-text model: %.3f [%.3f, %.3f], pd = %.2f\n", mean(asym), quantile(asym, .025), quantile(asym, .975), max(mean(asym > 0), mean(asym < 0))))
  write_csv(bind_rows(elpd, tibble(experiment = e, model = "asymmetry_bleft_plus_bright", elpd = mean(asym), se = sd(asym))), here::here("results", paste0("choice_value_parametrization_exp", e, ".csv")))
}
