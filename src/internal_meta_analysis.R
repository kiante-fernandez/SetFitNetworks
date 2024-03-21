# Internal meta-analysis
# Load data
library(tidyverse)
library(brms)

internal_meta_df <- readr::read_csv("data/internal_meta_analysis_choice.csv")

dat <- internal_meta_df %>% 
  select(term, estimate, std.error, group) %>%
  mutate(estimate = abs(estimate)) %>% 
  filter(term == "b_zright_net2" | term == "b_zleft_net2")
# filter(term == "b_zleft_net2")

#term == "b_zright_net2"
# 
# dat <- internal_meta_df %>% 
#   select(term, estimate, std.error, group) %>%
#   mutate(estimate = abs(estimate)) %>% 
#   filter(term == "b_zright_rating:zright_net2" | term == "b_zleft_rating:zleft_net2")

brm_out1 <- brm(
  estimate | se(std.error) ~ 1 + (1 | group),
  data = dat,
  # data = dat[dat$term == "b_zleft_rating:zleft_net2",], #b_zleft_rating:zleft_net2
  cores = 4,
  iter = 200000
  # file = here::here("fits", "metaanalysismodel")
)
brm_out2 <- brm(
  estimate | se(std.error) ~ 1 + (1 | group),
  data = dat[dat$term == "b_zright_rating:zright_net2",], #b_zright_rating:zright_net2
  cores = 4,
  iter = 4000
  # file = here::here("fits", "metaanalysismodel")
)
#what the average effect size less than zero?
hypothesis(brm_out1, "Intercept > 0.0")
#what the average effect size greater than zero?
hypothesis(brm_out2, "Intercept < 0.0")


library(tidybayes)
library(ggdist)
# Study-specific effects are deviations + average
out_r <- spread_draws(brm_out1, r_group[Experiment,], b_Intercept) %>% 
  mutate(b_Intercept = r_group + b_Intercept) 
# Average effect
out_f <- spread_draws(brm_out1, b_Intercept) %>% 
  mutate(Experiment = "Average")
# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r, out_f) %>% 
  ungroup() %>%
  # Ensure that Average effect is on the bottom of the forest plot
  mutate(group = fct_relevel(Experiment, "Average")) %>% 
  # tidybayes garbles names so fix here
  mutate(Experiment = str_replace_all(Experiment, "\\.", " "))
# Data frame of summary numbers
out_all_sum <- group_by(out_all, Experiment) %>% 
  mean_qi(b_Intercept)
# Draw plot
exp_plot_a <- out_all %>%   
ggplot(aes(b_Intercept, Experiment)) +
  # Zero!
  geom_vline(xintercept = 0, size = .25, lty = 2) +
  stat_halfeye(.width = c(.8, .95), fill = "dodgerblue") +
  # Add text labels
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = str_glue("{b_Intercept} [{.lower}, {.upper}]"), x = 0.75),
    hjust = "inward"
  )+
  # Observed as empty points
  geom_point(
    data = dat[dat$term == "b_zleft_net2",] %>% mutate(group = str_replace_all(group, "\\.", " "),
                          Experiment = group), 
    aes(x=estimate), position = position_nudge(y = -.2), shape = 1 
  )+theme_classic()+
  labs(x = "standardized regression (beta) coefficient",
       y = "Experiment")


# Study-specific effects are deviations + average
out_r <- spread_draws(brm_out2, r_group[Experiment,], b_Intercept) %>% 
  mutate(b_Intercept = r_group + b_Intercept) 
# Average effect
out_f <- spread_draws(brm_out2, b_Intercept) %>% 
  mutate(Experiment = "Average")
# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r, out_f) %>% 
  ungroup() %>%
  # Ensure that Average effect is on the bottom of the forest plot
  mutate(group = fct_relevel(Experiment, "Average")) %>% 
  # tidybayes garbles names so fix here
  mutate(Experiment = str_replace_all(Experiment, "\\.", " "))
# Data frame of summary numbers
out_all_sum <- group_by(out_all, Experiment) %>% 
  mean_qi(b_Intercept)
# Draw plot
exp_plot_b <- out_all %>%   
ggplot(aes(b_Intercept, Experiment)) +
  # Zero!
  geom_vline(xintercept = 0, size = .25, lty = 2) +
  stat_halfeye(.width = c(.8, .95), fill = "dodgerblue") +
  # Add text labels
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = str_glue("{b_Intercept} [{.lower}, {.upper}]"), x = 0.75),
    hjust = "inward"
  )+
  # Observed as empty points
  geom_point(
    data = dat[dat$term == "b_zright_net2",] %>% mutate(group = str_replace_all(group, "\\.", " "),
                                                       Experiment = group), 
    aes(x=estimate), position = position_nudge(y = -.2), shape = 1 
  )+theme_classic()+
  labs(x = "standardized regression (beta) coefficient",
       y = "Experiment")

library(patchwork) 
exp_plot_a +exp_plot_b

  