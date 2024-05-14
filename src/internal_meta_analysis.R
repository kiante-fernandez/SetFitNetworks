# Internal meta-analysis
# Load data
library(tidyverse)
library(brms)
library(RColorBrewer)  # For accessing color palettes


internal_meta_df <- readr::read_csv("data/internal_meta_analysis_choice.csv")

internal_meta_df <- readr::read_csv("data/internal_meta_analysis_fullR.csv")

#make sure the input is correct. These are not the right estimates. 
#also get all the estimates from set and binary choice studies

dat <- internal_meta_df %>% 
  select(term, estimate, std.error, group) %>%
  filter(str_detect(term, "net2"))
# filter(term == "b_zright_net2" | term == "b_zleft_net2")

#term == "b_zright_net2"
# 
# dat <- internal_meta_df %>% 
#   select(term, estimate, std.error, group) %>%
#   mutate(estimate = abs(estimate)) %>% 
#   filter(term == "b_zright_rating:zright_net2" | term == "b_zleft_rating:zleft_net2")

brm_out1 <- brm(
  estimate | se(std.error) ~ 1 + (1 | group) + (1 | term),
  data = dat,
  prior = c(prior(normal(0, .5), class = Intercept),
            prior(cauchy(0, .5), class = sd)),
  cores = 4,
  iter = 600000,
  # file = here::here("fits", "metaanalysismodel")
)

hypothesis(dat, "b_Intercept > 0")

#what the average effect size less than zero?
hypothesis(brm_out1, "Intercept > 0.0")

bayestestR::sexit(testing1)
report::report(testing1)
testing1 <- hypothesis(brm_out1, "Intercept > 0.0", scope = "coef", group  = "term")
plot(testing1)
testing2 <- hypothesis(brm_out1, "Intercept < 0.0",
                       scope = "coef", group  = "term")
plot(testing2) + theme_classic()


library(tidybayes)
library(ggdist)
# Study-specific effects are deviations + average
out_r <- spread_draws(brm_out1, r_group[group,], r_term[term,], b_Intercept) %>% 
  mutate(b_Intercept = r_term + r_group + b_Intercept)
# Average effect
out_f <- spread_draws(brm_out1, b_Intercept, r_term[term,]) %>% 
  group_by(term) %>% 
  mutate(b_Intercept = r_term + b_Intercept) %>% 
  mutate(term = paste0("Average.", term),
         group = "Average") %>%
  ungroup()

# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

# Data frame of summary numbers
out_all_sum <- group_by(out_all, group, term) %>% 
  mean_qi(b_Intercept) %>% 
  mutate(interaction = interaction(group, term))

brewer.pal(3, "Set1")

out_all %>%
  mutate(interaction = interaction(group, term),
         color_group = case_when(
           str_detect(group, "Experiment.One") ~ "Experiment One",
           str_detect(group, "Experiment.Two") ~ "Experiment Two",
           str_detect(group, "Experiment.Three") ~ "Experiment Three",
           TRUE ~ "Average"
         )) %>%
  mutate(interaction = fct_reorder(interaction, str_detect(interaction, "Average"), .desc = TRUE)) %>%
  ggplot(aes(x = b_Intercept, y = interaction)) +
  geom_vline(xintercept = 0, size = .25, lty = 2) +
  stat_halfeye(aes(fill = color_group),.width = c(.5, .95)) +
  theme_classic() +
  labs(x = "Standardized Regression (Beta) Coefficient",
       y = "Experiment & Term") +
  scale_fill_manual(values = c("Experiment One" = "#4DAF4A", 
                               "Experiment Two" = "#E41A1C", 
                               "Experiment Three" = "#377EB8",
                               "Average" = "orange"))+
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 3),
    aes(label = str_glue("{b_Intercept} [{.lower}, {.upper}]"), x = 0.60),
    hjust = "inward"
  )


exclusions <- function(df) {
  df %>%
    group_by(subject_id) %>%
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    ungroup() %>%
    filter(rt > 250 & rt < 10000)
}

standardized = TRUE
for_model <- internal_meta_df %>%
  exclusions() %>%
  group_by(dataset, subject_id) %>%
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
  select(dataset, choice_type, subject_id, choice, rt, zleft_rating, zright_rating, zleft_net1, zright_net1, zleft_net2, zright_net2, vd, nd1, nd2, ov)

for_model$dataset <- factor(for_model$dataset)


# library(rstantools)
# library(cmdstanr)

# (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 +  zleft_net2 + zright_net2 | subject_id) + , 

models_choice <- brm(choice ~ zleft_rating*(zleft_net1 + zleft_net2) + zright_rating*(zright_net1 + zright_net2) +
                       dataset*zleft_rating + dataset*zright_rating + 
                       dataset*zleft_net1 + dataset*zleft_net2+
                       dataset*zright_net1 + dataset*zright_net2+
                       (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 +  zleft_net2 + zright_net2 | subject_id),
                       data = for_model, family = "bernoulli", iter = 10000, 
                      chains = 4, cores = 4)

bayestestR::sexit(models_choice)


plot(ggeffects::ggpredict(models_choice, 
                          terms = c("zleft_rating[all]", "dataset")))+
  theme_classic()+
  # scale_y_continuous(limits = c(.25, .8))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0.5, linetype = "dashed")+
  labs(
    title = "",
    y = "Probability of Choosing Left",
    x = "left item-rating",
    color = "Data set"
  ) +
  theme(text = element_text(size = 15),
        legend.position = c(0.25, 0.85),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))


plot(ggeffects::ggpredict(models_choice, 
                          terms = c("zleft_net2[all]","dataset")))+
  theme_classic()+
  # scale_y_continuous(limits = c(.25, .8))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0.5, linetype = "dashed")+
  labs(
    title = "",
    y = "Probability of Choosing Left",
    x = "left centrality",
    color = "Data set"
  ) +
  theme(text = element_text(size = 15),
        legend.position = c(0.25, 0.85),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))

plot(ggeffects::ggpredict(models_choice, 
                          terms = c("zright_net2[all]","dataset")))+
  theme_classic()+
  # scale_y_continuous(limits = c(.25, .8))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0.5, linetype = "dashed")+
  labs(
    title = "",
    y = "Probability of Choosing Left",
    x = "left centrality",
    color = "Data set"
  ) +
  theme(text = element_text(size = 15),
        legend.position = c(0.25, 0.85),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))


plot(ggeffects::ggpredict(models_choice, 
                          terms = c("dataset","zright_net2[-2,0,2]","zleft_net2[-2,0,2]")))+
  theme_classic()



test <- ggeffects::ggemmeans(models_choice, terms = c("dataset","zright_net2"))
                     
                     
                     