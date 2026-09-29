# Model extraction for internal meta
# Copyright (C) 2023-2025 Kianté Fernandez, <kiantefernan@gmail.com>

# Libraries ----------------------------------------------------------------
library(sjPlot)
library(here)
library(tidyverse)
library(brms)
library(RColorBrewer)
library(tidybayes)
library(ggdist)

# Helper Function ---------------------------------------------------------
save_if_not_exists <- function(data, filename) {
  filepath <- here("data", filename)
  if (!file.exists(filepath)) {
    write.csv(data, filepath, row.names = FALSE)
    message(sprintf("Saved: %s", filename))
  } else {
    message(sprintf("File already exists: %s", filename))
  }
}

# Load Model Files -------------------------------------------------------
model_path <- "~/Documents/SetFitNetworks/fits"

# Set-Level Models (Choice) ---------------------------------------------
strength_exp_1_fit_choice03 <- readRDS(file.path(model_path, "strength_exp_1_fit_choice03.rds"))
strength_exp_2_fit_choice03 <- readRDS(file.path(model_path, "strength_exp_2_fit_choice03.rds"))
strength_exp_3_fit_choice03 <- readRDS(file.path(model_path, "strength_exp_3_fit_choice03.rds"))

choiceset_plot <- plot_models(
  strength_exp_1_fit_choice03,
  strength_exp_2_fit_choice03,
  strength_exp_3_fit_choice03,
  transform = NULL,
  show.values = TRUE,
  show.p = FALSE,
  m.labels = c("Experiment One", "Experiment Two", "Experiment Three"),
  ci.lvl = 0.95
)
save_if_not_exists(choiceset_plot$data, "internal_meta_analysis_choice_setfit.csv")

# Item-Level Models (Choice) -------------------------------------------
pca2_exp_1_fit_choice03 <- readRDS(file.path(model_path, "pca2_exp_1_fit_choice03.rds"))
pca2_exp_2_fit_choice03 <- readRDS(file.path(model_path, "pca2_exp_2_fit_choice03.rds"))
pca2_exp_3_fit_choice03 <- readRDS(file.path(model_path, "pca2_exp_3_fit_choice03.rds"))

choice_plot <- plot_models(
  pca2_exp_1_fit_choice03,
  pca2_exp_2_fit_choice03,
  pca2_exp_3_fit_choice03,
  transform = NULL,
  show.values = TRUE,
  show.p = FALSE,
  m.labels = c("Experiment One", "Experiment Two", "Experiment Three"),
  ci.lvl = 0.95
)
save_if_not_exists(choice_plot$data, "internal_meta_analysis_choice.csv")

# Set-Level Models (RT) ------------------------------------------------
strength_exp_1_fit_rt02 <- readRDS(file.path(model_path, "strength_exp_1_fit_rt02.rds"))
strength_exp_2_fit_rt02 <- readRDS(file.path(model_path, "strength_exp_2_fit_rt02.rds"))
strength_exp_3_fit_rt02 <- readRDS(file.path(model_path, "strength_exp_3_fit_rt02.rds"))

rtset_plot <- plot_models(
  strength_exp_1_fit_rt02,
  strength_exp_2_fit_rt02,
  strength_exp_3_fit_rt02,
  transform = NULL,
  show.values = TRUE,
  show.p = FALSE,
  m.labels = c("Experiment One", "Experiment Two", "Experiment Three"),
  ci.lvl = 0.95
)
save_if_not_exists(rtset_plot$data, "internal_meta_analysis_rt.csv")

# Item-Level Models (RT) ----------------------------------------------
pca2_exp_1_fit_rt02 <- readRDS(file.path(model_path, "pca2_exp_1_fit_rt02.rds"))
pca2_exp_2_fit_rt02 <- readRDS(file.path(model_path, "pca2_exp_2_fit_rt02.rds"))
pca2_exp_3_fit_rt02 <- readRDS(file.path(model_path, "pca2_exp_3_fit_rt02.rds"))

rt_plot <- plot_models(
  pca2_exp_1_fit_rt02,
  pca2_exp_2_fit_rt02,
  pca2_exp_3_fit_rt02,
  transform = NULL,
  show.values = TRUE,
  show.p = FALSE,
  m.labels = c("Experiment One", "Experiment Two", "Experiment Three"),
  ci.lvl = 0.95
)
save_if_not_exists(rt_plot$data, "internal_meta_analysis_rt_setfit.csv")

# Single Choice Models ------------------------------------------------
pca_Lee_Hare_2023_fit_choice03 <- readRDS(file.path(model_path, "PCA_Lee_Hare_2023_choice_data_exp2_fit_choice03.rds"))
pca_Lee_Holyoak_2021_fit_choice03 <- readRDS(file.path(model_path, "PCA_Lee_Holyoak_2021_choice_data_exp2_5_fit_choice03.rds"))
pca_Lee_Hare_2023_fit_rt02 <- readRDS(file.path(model_path, "PCA_Lee_Hare_2023_choice_data_exp2_fit_rt02.rds"))
pca_Lee_Holyoak_2021_fit_rt2 <- readRDS(file.path(model_path, "PCA_Lee_Holyoak_2021_choice_data_exp2_5_fit_rt02.rds"))
#add the Smith & Krajbich files
pca_Smith_Krajbich_2018_fit_choice <- readRDS(file.path(model_path, "PCA_Smith_Krajbich_2018_choice_data_fit_choice.rds"))
pca_Smith_Krajbich_2018_fit_rt <- readRDS(file.path(model_path, "PCA_Smith_Krajbich_2018_choice_data_fit_rt.rds"))

single_choice_plot <- plot_models(
  pca_Lee_Hare_2023_fit_choice03,
  pca_Lee_Holyoak_2021_fit_choice03,
  pca_Smith_Krajbich_2018_fit_choice,
  transform = NULL,
  show.values = TRUE,
  show.p = FALSE,
  m.labels = c("Experiment One", "Experiment Two","Experiment Three"),
  ci.lvl = 0.95
)
save_if_not_exists(single_choice_plot$data, "internal_meta_analysis_choice_single_UPDATED.csv")

single_rt_plot <- plot_models(
  pca_Lee_Hare_2023_fit_rt02,
  pca_Lee_Holyoak_2021_fit_rt2,
  pca_Smith_Krajbich_2018_fit_rt,
  transform = NULL,
  show.values = TRUE,
  show.p = FALSE,
  m.labels = c("Experiment One", "Experiment Two","Experiment Three"),
  ci.lvl = 0.95
)
save_if_not_exists(single_rt_plot$data, "internal_meta_analysis_RT_single_UPDATED.csv")

# Load Data ---------------------------------------------------------------
meta_data <- list(
  # Set level
  set_choice = readr::read_csv("data/internal_meta_analysis_choice_setfit.csv"),
  set_rt = readr::read_csv("data/internal_meta_analysis_rt_setfit.csv"),
  # Item level
  item_choice = readr::read_csv("data/internal_meta_analysis_choice.csv"),
  item_rt = readr::read_csv("data/internal_meta_analysis_rt.csv"),
  # Single trial
  single_choice = readr::read_csv("data/internal_meta_analysis_choice_single_UPDATED.csv"),
  single_rt = readr::read_csv("data/internal_meta_analysis_RT_single_UPDATED.csv")
)

# Model Settings --------------------------------------------------------
model_settings <- list(
  prior = c(
    prior(normal(0, .25), class = Intercept),
    prior(cauchy(0, .25), class = sd)
  ),
  cores = 4,
  iter = 10000,
  control = list(adapt_delta = 0.999, stepsize = 0.01, max_treedepth = 20)
)

# 1. Set-Level Analyses ------------------------------------------------
# Set-level choice meta-analysis
set_choice_data <- meta_data$set_choice %>%
  select(term, estimate, std.error, group) %>%
  filter(str_detect(term, "net1"))

set_choice_model <- brm(
  estimate | se(std.error) ~ 1 + (1 | group) + (1 | term),
  data = set_choice_data,
  prior = model_settings$prior,
  cores = model_settings$cores,
  iter = model_settings$iter,
  control = model_settings$control
)

# Set-level RT meta-analysis
set_rt_data <- meta_data$set_rt %>%
  select(term, estimate, std.error, group) %>%
  filter(str_detect(term, "nd1"))

set_rt_model <- brm(
  estimate | se(std.error) ~ 1 + (1 | group) + (1 | term),
  data = set_rt_data,
  prior = model_settings$prior,
  cores = model_settings$cores,
  iter = model_settings$iter,
  control = model_settings$control
)

# 2. Item-Level Analyses -----------------------------------------------
# Item-level choice meta-analysis
item_choice_data <- meta_data$item_choice %>%
  select(term, estimate, std.error, group) %>%
  filter(str_detect(term, "net2"))

item_choice_model <- brm(
  estimate | se(std.error) ~ 1 + (1 | group) + (1 | term),
  data = item_choice_data,
  prior = model_settings$prior,
  cores = model_settings$cores,
  iter = model_settings$iter,
  control = model_settings$control
)

# Item-level RT meta-analysis
item_rt_data <- meta_data$item_rt %>%
  select(term, estimate, std.error, group) %>%
  filter(str_detect(term, "nd2"))

item_rt_model <- brm(
  estimate | se(std.error) ~ 1 + (1 | group) + (1 | term),
  data = item_rt_data,
  prior = model_settings$prior,
  cores = model_settings$cores,
  iter = model_settings$iter,
  control = model_settings$control
)

# 3. Single-Trial Analyses --------------------------------------------
# Single-trial choice meta-analysis
single_choice_data <- meta_data$single_choice %>%
  select(term, estimate, std.error, group) %>%
  filter(str_detect(term, "net2"))

single_choice_model <- brm(
  estimate | se(std.error) ~ 1 + (1 | group) + (1 | term),
  data = single_choice_data,
  prior = model_settings$prior,
  cores = model_settings$cores,
  iter = model_settings$iter,
  control = model_settings$control
)

# Single-trial RT meta-analysis
single_rt_data <- meta_data$single_rt %>%
  select(term, estimate, std.error, group) %>%
  filter(str_detect(term, "nd2"))

single_rt_model <- brm(
  estimate | se(std.error) ~ 1 + (1 | group) + (1 | term),
  data = single_rt_data,
  prior = model_settings$prior,
  cores = model_settings$cores,
  iter = model_settings$iter,
  control = model_settings$control
)

# Collect Results -----------------------------------------------------
meta_models <- list(
  # Set level
  set_choice = set_choice_model,
  set_rt = set_rt_model,
  # Item level
  item_choice = item_choice_model,
  item_rt = item_rt_model,
  # Single trial
  single_choice = single_choice_model,
  single_rt = single_rt_model
)

# Save Results -------------------------------------------------------
if (!file.exists(here("fits", "meta_analysis_models.rds"))) {
  saveRDS(meta_models, here("fits", "internal_meta_analysis_models.rds"))
}

# Process and Plot Results for Each Model -----------------------------------

# Process model results
process_model_draws <- function(model) {
  # Study-specific effects
  out_r <- spread_draws(model, r_group[group,], r_term[term,], b_Intercept) %>%
    mutate(b_Intercept = r_term + r_group + b_Intercept)
  
  # Average effect
  out_f <- spread_draws(model, b_Intercept, r_term[term,]) %>%
    group_by(term) %>%
    mutate(
      b_Intercept = r_term + b_Intercept,
      term = paste0("Average.", term),
      group = "Average"
    ) %>%
    ungroup()
  
  # Combine
  out_all <- bind_rows(out_r, out_f) %>%
    ungroup() %>%
    mutate(group = fct_relevel(group, "Average"))
  
  # Calculate summary stats
  out_all_sum <- group_by(out_all, group, term) %>%
    mean_qi(b_Intercept)
  
  list(out_all = out_all, out_all_sum = out_all_sum)
}

# Create forest plot
create_forest_plot <- function(results, data, is_single = FALSE, x_limits = NULL, title = "") {
  # Define color schemes
  multi_exp_colors <- c(
    "Experiment One" = "#4DAF4A",
    "Experiment Two" = "#E41A1C",
    "Experiment Three" = "#377EB8",
    "Average" = "orange"
  )
  
  single_exp_colors <- c(
    "Experiment One" = "#66C2A5",
    "Experiment Two" = "#8DA0CB",
    "Experiment Three" = "#A6D854",
    "Average" = "orange"
  )
  
  # Add color grouping
  out_all <- results$out_all %>%
    mutate(
      interaction = interaction(group, term),
      color_group = case_when(
        str_detect(group, "Experiment.One") ~ "Experiment One",
        str_detect(group, "Experiment.Two") ~ "Experiment Two",
        str_detect(group, "Experiment.Three") ~ "Experiment Three",
        TRUE ~ "Average"
      )
    )
  
  out_all_sum <- results$out_all_sum %>%
    mutate(
      interaction = interaction(group, term),
      color_group = case_when(
        str_detect(group, "Experiment.One") ~ "Experiment One",
        str_detect(group, "Experiment.Two") ~ "Experiment Two",
        str_detect(group, "Experiment.Three") ~ "Experiment Three",
        TRUE ~ "Average"
      )
    ) %>%
    merge(data, by.x = c("term", "color_group"), by.y = c("term", "group"), all.x = TRUE)
  
  # Set explicit order for color_group
  out_all$color_group <- factor(
    out_all$color_group,
    levels = c("Experiment One", "Experiment Two", "Experiment Three", "Average")
  )
  out_all_sum$color_group <- factor(
    out_all_sum$color_group,
    levels = c("Experiment One", "Experiment Two", "Experiment Three", "Average")
  )
  
  # Reorder by term first, then by experiment
  out_all <- out_all %>%
    arrange(term, color_group) %>%
    mutate(interaction = factor(interaction, levels = unique(interaction)))
  
  out_all_sum <- out_all_sum %>%
    arrange(term, color_group) %>%
    mutate(interaction = factor(interaction, levels = unique(interaction)))
  
  # Create base plot
  p <- out_all %>%
    ggplot(aes(x = b_Intercept, y = interaction)) +
    geom_vline(xintercept = 0, size = .25, lty = 2) +
    stat_halfeye(aes(fill = color_group), .width = c(.5, .95)) +
    theme_classic() +
    labs(
      x = "Standardized Regression (Beta) Coefficient",
      y = "Experiment & Term",
      title = title
    )
  
  # Add appropriate color scheme
  p <- p + scale_fill_manual(
    values = if(is_single) single_exp_colors else multi_exp_colors
  )
  
  # Add x-axis limits if specified
  if (!is.null(x_limits)) {
    p <- p + scale_x_continuous(limits = x_limits)
  }
  
  # Add text annotations
  p <- p +
    # geom_text(
    #   data = mutate_if(out_all_sum, is.numeric, round, 3),
    #   aes(label = str_glue("{b_Intercept} [{.lower}, {.upper}]"),
    #       x = if(is.null(x_limits)) 0.60 else x_limits[2]),
    #   hjust = "inward",
    #   color = "black"
    # )
    # geom_text(
    #   data = mutate_if(out_all_sum, is.numeric, round, 2),
    #   aes(label = str_glue("{estimate} [{estimate - std.error}, {estimate + std.error}]")),
    #   hjust = "inward",
    #   position = position_nudge(y = -.5),
    #   color = "black"
    # ) 
    # geom_point(
    #   data = out_all_sum,
    #   aes(x = estimate),
    #   position = position_nudge(y = -.2),
    #   shape = 1
    # )
  
  return(p)
}

# Generate Plots for All Models -------------------------------------------

# Process and plot set-level models
set_choice_results <- process_model_draws(meta_models$set_choice)
set_choice_plot <- create_forest_plot(
  set_choice_results, 
  set_choice_data,
  is_single = FALSE,
  x_limits = c(-0.3, 0.4),
  title = "Set-Level Choice Meta-Analysis"
)

set_rt_results <- process_model_draws(meta_models$set_rt)
set_rt_plot <- create_forest_plot(
  set_rt_results, 
  set_rt_data,
  is_single = FALSE,
  x_limits = c(-0.07, 0.09),
  title = "Set-Level RT Meta-Analysis"
)

# Process and plot item-level models
item_choice_results <- process_model_draws(meta_models$item_choice)
item_choice_plot <- create_forest_plot(
  item_choice_results, 
  item_choice_data,
  is_single = FALSE,
  x_limits = c(-0.3, 0.4),
  title = "Item-Level Choice Meta-Analysis"
)

item_rt_results <- process_model_draws(meta_models$item_rt)
item_rt_plot <- create_forest_plot(
  item_rt_results, 
  item_rt_data,
  is_single = FALSE,
  x_limits = c(-0.07, 0.09),
  title = "Item-Level RT Meta-Analysis"
)

# Process and plot single-trial models
single_choice_results <- process_model_draws(meta_models$single_choice)
single_choice_plot <- create_forest_plot(
  single_choice_results, 
  single_choice_data,
  is_single = TRUE,
  x_limits = c(-0.8, 1),
  title = "Single-Trial Choice Meta-Analysis"
)

single_rt_results <- process_model_draws(meta_models$single_rt)
single_rt_plot <- create_forest_plot(
  single_rt_results, 
  single_rt_data,
  is_single = TRUE,
  # x_limits = c(-0.03, 0.2),
  title = "Single-Trial RT Meta-Analysis"
)

# Save Plots ------------------------------------------------------------
plots <- list(
  set_choice = set_choice_plot,
  set_rt = set_rt_plot,
  item_choice = item_choice_plot,
  item_rt = item_rt_plot,
  single_choice = single_choice_plot,
  single_rt = single_rt_plot
)

# Save individual plots
# for (name in names(plots)) {
#   ggsave(
#     filename = here("figures", paste0("meta_analysis_", name, ".pdf")),
#     plot = plots[[name]],
#     width = 10,
#     height = if(str_detect(name, "single")) 6 else 8
#   )
# }
