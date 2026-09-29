# centrality_interaction_plots.R - Visualize centrality × value interactions
#
# Purpose: Address Reviewer 2's request to show interaction plots, not just posteriors.
# Creates figures showing how centrality moderates the value-choice relationship.
#
# Copyright (C) 2025 Kianté Fernandez, <kiantefernan@gmail.com>

library(here)
library(tidyverse)
library(patchwork)

# Source helper functions
source(here::here("src", "utils.R"))
source(here::here("src", "exploratory_graph_analysis.R"))

# =============================================================================
# SETUP: Load and prepare data
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("CENTRALITY INTERACTION PLOTS\n")
cat(strrep("=", 70), "\n\n")

net_degree <- calculate_net_stats(g)

# Load all three set-choice experiments
df1 <- organize_group_data(experiment = 1)
df2 <- organize_group_data(experiment = 2)
df3 <- organize_group_data(experiment = 3)

# Add experiment labels
df1$experiment <- "Set-Choice Study 1"
df2$experiment <- "Set-Choice Study 2"
df3$experiment <- "Set-Choice Study 3"

# Offset subject IDs to make unique
df1$subject_id <- df1$subject_id + 100
df2$subject_id <- df2$subject_id + 200
df3$subject_id <- df3$subject_id + 300

# Combine data
df_all <- bind_rows(df1, df2, df3)

# =============================================================================
# PREPARE DATA: Apply exclusions and create centrality groups
# =============================================================================

# Exclusion function
apply_exclusions <- function(df) {
  df %>%
    group_by(subject_id) %>%
    mutate(
      Q1 = quantile(rt, .25),
      Q3 = quantile(rt, .75),
      IQR = IQR(rt)
    ) %>%
    filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
    ungroup() %>%
    filter(rt > 250 & rt < 9000)
}

df_clean <- apply_exclusions(df_all)

# Restrict to the participants retained in the main-text models (same offsets as above)
kept <- c(unique(readRDS(here::here("fits", "pca2_exp_1_fit_choice03.rds"))$data$subject_id) + 100,
          unique(readRDS(here::here("fits", "pca2_exp_2_fit_choice03.rds"))$data$subject_id) + 200,
          unique(readRDS(here::here("fits", "pca2_exp_3_fit_choice03.rds"))$data$subject_id) + 300)
df_clean <- df_clean %>% filter(subject_id %in% kept)
cat("Participants retained (matching main-text models):", length(unique(df_clean$subject_id)), "\n")

# Create centrality groups (median split on average centrality)
df_plot <- df_clean %>%
  mutate(
    # Value difference (left - right)
    value_diff = left_rating - right_rating,

    # Average set centrality (PC2 which showed effects)
    avg_centrality_left = left_net_pca2,
    avg_centrality_right = right_net_pca2,

    # Overall centrality (sum of both sides for trial-level metric)
    total_centrality = left_net_pca2 + right_net_pca2,

    # Centrality of chosen set
    chosen_centrality = if_else(choice == 1, left_net_pca2, right_net_pca2),

    # For plotting: left centrality terciles
    left_centrality_group = case_when(
      left_net_pca2 < quantile(left_net_pca2, 0.33, na.rm = TRUE) ~ "Low Centrality",
      left_net_pca2 > quantile(left_net_pca2, 0.67, na.rm = TRUE) ~ "High Centrality",
      TRUE ~ "Medium Centrality"
    ),
    left_centrality_group = factor(left_centrality_group,
                                    levels = c("Low Centrality", "Medium Centrality", "High Centrality"))
  ) %>%
  # RT is modeled on absolute differences (main text), so the RT plots use |value difference| and
  # terciles (within experiment) of |PC2 difference| between the two sets
  group_by(experiment) %>%
  mutate(
    abs_value_diff = abs(left_rating - right_rating),
    abs_pc2_diff = abs(left_net_pca2 - right_net_pca2),
    pc2_diff_group = case_when(
      abs_pc2_diff < quantile(abs_pc2_diff, 1/3, na.rm = TRUE) ~ "Small",
      abs_pc2_diff > quantile(abs_pc2_diff, 2/3, na.rm = TRUE) ~ "Large",
      TRUE ~ "Medium"
    ),
    pc2_diff_group = factor(pc2_diff_group, levels = c("Small", "Medium", "Large"))
  ) %>%
  ungroup()

cat("Total trials after exclusions:", nrow(df_plot), "\n")
cat("Unique participants:", length(unique(df_plot$subject_id)), "\n\n")

# =============================================================================
# PLOT 1: Choice probability by value difference at different centrality levels
# =============================================================================
cat("--- Creating Choice x Centrality Interaction Plot ---\n")

# Bin value differences for cleaner visualization
df_binned <- df_plot %>%
  mutate(value_diff_bin = cut(value_diff, breaks = 9, labels = FALSE)) %>%
  group_by(experiment, left_centrality_group, value_diff_bin) %>%
  summarise(
    mean_value_diff = mean(value_diff, na.rm = TRUE),
    mean_choice = mean(choice, na.rm = TRUE),
    se_choice = sqrt(var(choice, na.rm = TRUE) / n()),
    n = n(),
    .groups = "drop"
  )

# Choice probability plot
p_choice <- ggplot(df_plot, aes(x = value_diff, y = choice, color = left_centrality_group)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, alpha = 0.2, linewidth = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~experiment, ncol = 3) +
  scale_color_manual(values = c("#377EB8", "#999999", "#E41A1C"),
                     name = "Left Set Centrality") +
  scale_fill_manual(values = c("#377EB8", "#999999", "#E41A1C"),
                    name = "Left Set Centrality") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Choice Probability by Value Difference and Set Centrality",
    x = "Value Difference (Left - Right Rating)",
    y = "P(Choose Left Set)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray95"),
    strip.text = element_text(face = "bold")
  )

ggsave(here::here("output", "centrality_choice_interaction.pdf"),
       p_choice, width = 12, height = 5)
cat("Saved: output/centrality_choice_interaction.pdf\n")

# =============================================================================
# PLOT 2: Response time by value difference at different centrality levels
# =============================================================================
cat("--- Creating RT x Centrality Interaction Plot ---\n")

# For RT, we use log-transformed values, centered within participant (the models estimate
# within-participant slopes) and shifted back to each experiment's grand mean for readability
df_plot <- df_plot %>%
  group_by(experiment) %>% mutate(grand_log_rt = mean(log(rt))) %>%
  group_by(subject_id) %>% mutate(log_rt = log(rt) - mean(log(rt)) + grand_log_rt) %>%
  ungroup()

# RT plot
p_rt <- ggplot(df_plot, aes(x = abs_value_diff, y = log_rt, color = pc2_diff_group)) +
  stat_smooth(method = "lm", se = TRUE, alpha = 0.2, linewidth = 1.2) +
  facet_wrap(~experiment, ncol = 3) +
  scale_color_manual(values = c("#377EB8", "#999999", "#E41A1C"),
                     name = "Absolute Centrality (PC2) Difference Between Sets") +
  labs(
    title = "Response Time by Absolute Value Difference and Absolute Centrality Difference",
    x = "Absolute Value Difference |Left - Right Rating|",
    y = "Log Response Time (ms), within-participant centered"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray95"),
    strip.text = element_text(face = "bold")
  )

ggsave(here::here("output", "centrality_rt_interaction.pdf"),
       p_rt, width = 12, height = 5)
ggsave(here::here("output", "centrality_rt_interaction.png"),
       p_rt, width = 12, height = 5, dpi = 200)
cat("Saved: output/centrality_rt_interaction.pdf\n")

# =============================================================================
# PLOT 3: Combined meta-analytic view (all studies pooled)
# =============================================================================
cat("--- Creating Combined Meta-Analytic Plot ---\n")

# Combined choice plot
p_choice_combined <- ggplot(df_plot, aes(x = value_diff, y = choice, color = left_centrality_group)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, alpha = 0.15, linewidth = 1.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("#377EB8", "#999999", "#E41A1C"),
                     name = "Left Set Centrality (PC2)") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Set Choice by Value and Centrality",
    x = "Value Difference (Left - Right)",
    y = "P(Choose Left)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Combined RT plot
p_rt_combined <- ggplot(df_plot, aes(x = abs_value_diff, y = log_rt, color = pc2_diff_group)) +
  stat_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 1.5) +
  scale_color_manual(values = c("#377EB8", "#999999", "#E41A1C"),
                     name = "Absolute PC2 Difference") +
  labs(
    title = "Response Time by Absolute Value and Centrality Differences",
    x = "Absolute Value Difference |Left - Right|",
    y = "Log RT (ms)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Combine plots
p_combined <- p_choice_combined + p_rt_combined +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Centrality × Value Interactions in Set Choice",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
    )
  ) &
  theme(legend.position = "bottom")

ggsave(here::here("output", "centrality_interaction_combined.pdf"),
       p_combined, width = 14, height = 6)
cat("Saved: output/centrality_interaction_combined.pdf\n")

# =============================================================================
# PLOT 4: Simple marginal effects plot
# =============================================================================
cat("--- Creating Marginal Effects Plot ---\n")

# Value-adjusted P(choose left) by left-set centrality tercile. Left-set PC2 correlates ~.2 with the
# left set's own rating, so raw tercile means mostly reflect value; here P(choose left) is predicted
# from a logistic model with both sets' (within-participant z-scored) values and centralities, at
# the tercile's mean left-set centrality with values and right-set centrality held at their means.
marginal_effects <- df_plot %>%
  group_by(experiment) %>%
  group_modify(function(d, k) {
    d <- d %>%
      group_by(subject_id) %>%
      mutate(zl = as.numeric(scale(left_rating)), zr = as.numeric(scale(right_rating)),
             zn = as.numeric(scale(left_net_pca2)), zrn = as.numeric(scale(right_net_pca2))) %>%
      ungroup() %>%
      filter(complete.cases(zl, zr, zn, zrn))
    d$left_centrality_group <- cut(d$zn, quantile(d$zn, c(0, 1/3, 2/3, 1)), include.lowest = TRUE,
                                   labels = c("Low Centrality", "Medium Centrality", "High Centrality"))
    m <- glm(choice ~ zl + zr + zn + zrn, data = d, family = binomial)
    nd <- d %>% group_by(left_centrality_group) %>% summarise(zn = mean(zn), n = n(), .groups = "drop") %>%
      mutate(zl = 0, zr = 0, zrn = 0)
    pr <- predict(m, nd, type = "link", se.fit = TRUE)
    tibble(left_centrality_group = nd$left_centrality_group, n = nd$n,
           mean_choice = plogis(pr$fit), lo = plogis(pr$fit - pr$se.fit), hi = plogis(pr$fit + pr$se.fit))
  }) %>%
  ungroup()

p_marginal <- ggplot(marginal_effects,
                      aes(x = left_centrality_group, y = mean_choice, color = experiment)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(width = 0.6), width = 0.2, linewidth = 0.8) +
  geom_point(position = position_dodge(width = 0.6), size = 3.5) +
  scale_color_manual(values = c("#4DAF4A", "#E41A1C", "#377EB8"),
                     name = "Study") +
  scale_y_continuous(limits = c(0.4, 0.6), breaks = seq(0.4, 0.6, 0.05)) +
  labs(
    title = "Value-Adjusted Marginal Effect of Set Centrality on Choice",
    x = "Left Set Centrality (PC2 terciles)",
    y = "P(Choose Left) at equal set values"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave(here::here("output", "centrality_marginal_effects.pdf"),
       p_marginal, width = 8, height = 6)
cat("Saved: output/centrality_marginal_effects.pdf\n")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("SUMMARY STATISTICS\n")
cat(strrep("=", 70), "\n\n")

# Print mean choice by centrality
cat("Mean P(Choose Left) by Centrality Group:\n")
print(marginal_effects)

# Test for centrality effect
cat("\n\nLogistic regression test (pooled data):\n")
simple_model <- glm(choice ~ left_net_pca2 * left_rating,
                    data = df_plot, family = binomial)
print(summary(simple_model)$coefficients)

cat("\n", strrep("=", 70), "\n")
cat("INTERACTION PLOTS COMPLETE\n")
cat(strrep("=", 70), "\n")
cat("\nFiles created:\n")
cat("  - output/centrality_choice_interaction.pdf\n")
cat("  - output/centrality_rt_interaction.pdf\n")
cat("  - output/centrality_interaction_combined.pdf\n")
cat("  - output/centrality_marginal_effects.pdf\n")
