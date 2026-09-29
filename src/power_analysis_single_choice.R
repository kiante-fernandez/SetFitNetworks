# power_analysis_single_choice.R - Power analysis for single-choice studies
#
# Purpose: Address Reviewer 2's concern about power to detect centrality effects
# in Single-Choice Studies 1-3 where we report null findings.
#
# Approach: Simulation-based power analysis using simr package
#
# Copyright (C) 2025 Kianté Fernandez, <kiantefernan@gmail.com>

library(here)
library(tidyverse)
library(lme4)
library(simr)

# =============================================================================
# SETUP: Load data and prepare for analysis
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("POWER ANALYSIS FOR SINGLE-CHOICE STUDIES\n")
cat(strrep("=", 70), "\n\n")

# Source helper functions
source(here::here("src", "exploratory_graph_analysis.R"))
source(here::here("src", "utils.R"))

# Calculate network statistics
net_degree <- calculate_net_stats(g)

# =============================================================================
# LOAD DATA: Single-Choice Study 1 (Lee & Hare 2023)
# =============================================================================
cat("--- Loading Single-Choice Study 1 (Lee & Hare 2023) ---\n")

Lee_Hare_2023 <- read_csv(here::here("data", "Lee_Hare_2023_OSF",
                                      "Lee_Hare_2023_choice_data_exp2.csv"),
                           show_col_types = FALSE)

# Add network measures
study1_data <- Lee_Hare_2023 %>%
  mutate(rt = rt * 1000) %>%
  rowwise() %>%
  mutate(
    left_net_pca2 = net_degree$PCA2[net_degree$Image == item_number_left],
    right_net_pca2 = net_degree$PCA2[net_degree$Image == item_number_right],
    choice = if_else(choice == 1, 0, 1),
    left_rating = item_value_left,
    right_rating = item_value_right
  ) %>%
  ungroup()

# Apply exclusions
study1_clean <- study1_data %>%
  group_by(subject_id) %>%
  mutate(
    Q1 = quantile(rt, .25),
    Q3 = quantile(rt, .75),
    IQR = IQR(rt)
  ) %>%
  filter(rt > (Q1 - 2 * IQR) & rt < (Q3 + 2 * IQR)) %>%
  ungroup() %>%
  filter(rt > 250 & rt < 9000)

# Standardize within subject
study1_model_data <- study1_clean %>%
  group_by(subject_id) %>%
  mutate(
    zleft_rating = scale(left_rating),
    zright_rating = scale(right_rating),
    zleft_net2 = scale(left_net_pca2),
    zright_net2 = scale(right_net_pca2)
  ) %>%
  ungroup() %>%
  drop_na(zleft_rating, zright_rating, zleft_net2, zright_net2)

n_subjects_s1 <- length(unique(study1_model_data$subject_id))
n_trials_s1 <- nrow(study1_model_data)
cat("Study 1: N =", n_subjects_s1, "subjects,", n_trials_s1, "trials\n\n")

# =============================================================================
# NOTE: Single-Choice Study 2 (Lee & Holyoak 2021) uses different item numbering
# that doesn't directly map to our network. Skipping for now.
# =============================================================================
cat("--- Single-Choice Study 2 skipped (item numbering mismatch) ---\n\n")

# =============================================================================
# FIT BASELINE MODELS (frequentist for simr compatibility)
# =============================================================================
cat("--- Fitting Baseline Models ---\n")

# Model specification matches the brms models from main analysis:
# choice ~ zleft_rating + zright_rating + zleft_net2 + zright_net2 +
#          zleft_rating:zleft_net2 + zright_rating:zright_net2 + (1 | subject_id)

# Study 1 model
model_s1 <- glmer(
  choice ~ zleft_rating + zright_rating +
    zleft_net2 + zright_net2 +
    zleft_rating:zleft_net2 + zright_rating:zright_net2 +
    (1 | subject_id),
  data = study1_model_data,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7))
)

cat("\nStudy 1 Model Summary:\n")
print(summary(model_s1)$coefficients)

# Study 2 skipped due to item numbering mismatch

# =============================================================================
# POWER ANALYSIS: Using effect size from Set-Choice studies
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("POWER ANALYSIS\n")
cat(strrep("=", 70), "\n\n")

# Effect size from Set-Choice studies (from manuscript):
# Set-Choice Study 2: PC2 left effect β = 0.12 [0.03, 0.21], pd = 0.99
# We'll use this as the target effect size

target_effect <- 0.12  # standardized beta from Set-Choice Study 2

cat("Target effect size (from Set-Choice studies): β =", target_effect, "\n\n")

# --- Study 1 Power Analysis ---
cat("--- Study 1 Power Analysis ---\n")

# Set the effect size for centrality to the target
model_s1_power <- model_s1
fixef(model_s1_power)["zleft_net2"] <- target_effect
fixef(model_s1_power)["zright_net2"] <- -target_effect

# Run power simulation for the main effect of centrality (left)
cat("Running power simulation for Study 1 (this may take a while)...\n")
power_s1_left <- powerSim(model_s1_power,
                           test = fixed("zleft_net2", "z"),
                           nsim = 500,
                           progress = FALSE)

cat("\nStudy 1 Power for left centrality effect (β =", target_effect, "):\n")
print(power_s1_left)

# Study 2 power analysis skipped

# =============================================================================
# SENSITIVITY ANALYSIS: What effect could we detect at 80% power?
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("SENSITIVITY ANALYSIS\n")
cat(strrep("=", 70), "\n\n")

cat("Testing power across different effect sizes...\n\n")

effect_sizes <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30)

# Study 1 sensitivity
power_results_s1 <- data.frame(
  study = "Single-Choice Study 1",
  effect_size = effect_sizes,
  power = NA,
  n_subjects = n_subjects_s1,
  n_trials = n_trials_s1
)

for (i in seq_along(effect_sizes)) {
  model_temp <- model_s1
  fixef(model_temp)["zleft_net2"] <- effect_sizes[i]

  power_temp <- powerSim(model_temp,
                          test = fixed("zleft_net2", "z"),
                          nsim = 200,
                          progress = FALSE)

  power_results_s1$power[i] <- summary(power_temp)$mean
  cat("Study 1, β =", effect_sizes[i], ": Power =", round(power_results_s1$power[i], 2), "\n")
}

# Combine results (Study 1 only)
power_results <- power_results_s1

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("Sample sizes:\n")
cat("  Single-Choice Study 1 (Lee & Hare 2023): N =", n_subjects_s1, "participants,", n_trials_s1, "trials\n\n")

cat("Effect size from Set-Choice studies: β = 0.12\n\n")

cat("Power to detect β = 0.12 centrality effect:\n")
cat("  Study 1:", round(summary(power_s1_left)$mean * 100, 1), "%\n\n")

# Find minimum detectable effect at 80% power
mde_s1 <- power_results_s1$effect_size[which.min(abs(power_results_s1$power - 0.80))]

cat("Minimum detectable effect at 80% power:\n")
cat("  Study 1: β ≈", mde_s1, "\n\n")

# Save results
write_csv(power_results, here::here("output", "power_analysis_single_choice.csv"))
cat("Results saved to: output/power_analysis_single_choice.csv\n")

# =============================================================================
# PLOT: Power curves
# =============================================================================
cat("\n--- Creating Power Curve Plot ---\n")

library(ggplot2)

power_plot <- ggplot(power_results, aes(x = effect_size, y = power, color = study)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0.12, linetype = "dotted", color = "red", linewidth = 0.8) +
  annotate("text", x = 0.12, y = 0.95, label = "Set-Choice\neffect size",
           hjust = -0.1, size = 3, color = "red") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#3C5488", "#E64B35")) +
  labs(
    title = "Power Analysis: Single-Choice Studies",
    subtitle = "Power to detect centrality effect on choice at varying effect sizes",
    x = "Effect Size (standardized β)",
    y = "Statistical Power",
    color = "Study"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave(here::here("output", "power_analysis_single_choice.pdf"),
       power_plot, width = 8, height = 6)
cat("Plot saved to: output/power_analysis_single_choice.pdf\n")

cat("\n", strrep("=", 70), "\n")
cat("POWER ANALYSIS COMPLETE\n")
cat(strrep("=", 70), "\n")
