# item_level_multi_alternative_analysis.R - Item-level choice analysis
#
# This script implements an item-level analysis where:
#   - Unit of observation: 1 row per item per trial
#   - Outcome: chosen (0/1 for each item)
#   - Predictors: item_value, PC1, PC2 (z-scored within subject)
#   - Random effects: (1 | subject_id) + (1 | trial_id)
#
# Applied to: Leng, Fernandez Exp 1, Fernandez Exp 2 (all set sizes)
#
# Copyright (C) 2025 Kianté Fernandez
# License: GPL-3

# Libraries ----------------------------------------------------------------
library(tidyverse)
library(here)
library(brms)
library(cmdstanr)

# Configuration ------------------------------------------------------------
set.seed(2025)

# Helper functions ---------------------------------------------------------

#' Extract posterior effect statistics
extract_effect <- function(post, param_name) {
  samples <- post[[param_name]]
  if (is.null(samples)) {
    return(list(mean = NA, sd = NA, lower = NA, upper = NA, pd = NA))
  }
  list(
    mean = mean(samples),
    sd = sd(samples),
    lower = quantile(samples, 0.025),
    upper = quantile(samples, 0.975),
    pd = max(mean(samples > 0), mean(samples < 0))
  )
}

#' RT exclusion function
exclude_rt <- function(df, rt_column = "total_rt", lower_bound = 250,
                       upper_bound = 10000, iqr_multiplier = 2) {
  df %>%
    filter(!!sym(rt_column) > lower_bound) %>%
    filter(!!sym(rt_column) < upper_bound) %>%
    group_by(subject_id) %>%
    mutate(
      Q1 = quantile(!!sym(rt_column), .25),
      Q3 = quantile(!!sym(rt_column), .75),
      IQR = IQR(!!sym(rt_column))
    ) %>%
    filter(!!sym(rt_column) > (Q1 - iqr_multiplier * IQR) &
             !!sym(rt_column) < (Q3 + iqr_multiplier * IQR)) %>%
    ungroup() %>%
    select(-Q1, -Q3, -IQR)
}

#' Reshape trial-level data to item-level format
#'
#' @param trial_data Dataframe with trial-level data
#' @param item_value_cols Character vector of item value column names (e.g., c("item_value_0", ...))
#' @param item_name_cols Character vector of item name column names (e.g., c("item_name_0", ...))
#' @param item_pca1_cols Character vector of PC1 column names (or NULL to merge later)
#' @param item_pca2_cols Character vector of PC2 column names (or NULL to merge later)
#' @param choice_col Name of column indicating chosen item position (0-indexed)
#' @param subject_col Name of subject ID column
#' @param trial_col Name of trial number column (or NULL to use row number)
#' @param rt_col Name of RT column (or NULL if not available)
#'
#' @return Long-format dataframe with one row per item per trial
reshape_to_item_level <- function(trial_data,
                                   item_value_cols,
                                   item_name_cols = NULL,
                                   item_pca1_cols = NULL,
                                   item_pca2_cols = NULL,
                                   choice_col = "choice_1",
                                   subject_col = "subject_id",
                                   trial_col = NULL,
                                   rt_col = NULL) {

  n_items <- length(item_value_cols)

  # Create unique trial identifier
  if (is.null(trial_col)) {
    trial_data <- trial_data %>%
      mutate(.row_id = row_number())
    trial_col <- ".row_id"
  }

  trial_data <- trial_data %>%
    mutate(trial_id = paste0(!!sym(subject_col), "_", !!sym(trial_col)))

  # Build long format data frame
  long_list <- list()

  for (i in seq_along(item_value_cols)) {
    item_pos <- i - 1  # 0-indexed position

    item_df <- trial_data %>%
      select(
        subject_id = !!sym(subject_col),
        trial_id,
        choice_pos = !!sym(choice_col),
        item_value = !!sym(item_value_cols[i]),
        any_of(rt_col)
      ) %>%
      mutate(item_position = item_pos)

    # Add item name if available
    if (!is.null(item_name_cols) && length(item_name_cols) >= i) {
      item_df <- item_df %>%
        mutate(item_id = trial_data[[item_name_cols[i]]])
    }

    # Add PC1 if available
    if (!is.null(item_pca1_cols) && length(item_pca1_cols) >= i) {
      item_df <- item_df %>%
        mutate(PC1 = trial_data[[item_pca1_cols[i]]])
    }

    # Add PC2 if available
    if (!is.null(item_pca2_cols) && length(item_pca2_cols) >= i) {
      item_df <- item_df %>%
        mutate(PC2 = trial_data[[item_pca2_cols[i]]])
    }

    long_list[[i]] <- item_df
  }

  long_data <- bind_rows(long_list)

  # Add chosen indicator (1 if this item's position matches the choice)
  long_data <- long_data %>%
    mutate(chosen = as.integer(item_position == choice_pos))

  # Filter out missing items (value = NA or -1)
  long_data <- long_data %>%
    filter(!is.na(item_value) & item_value != -1)

  # Z-score within subject
  long_data <- long_data %>%
    group_by(subject_id) %>%
    mutate(
      item_value_z = scale(item_value)[,1],
      PC1_z = if ("PC1" %in% names(.)) scale(PC1)[,1] else NA_real_,
      PC2_z = if ("PC2" %in% names(.)) scale(PC2)[,1] else NA_real_
    ) %>%
    ungroup() %>%
    mutate(across(ends_with("_z"), ~ifelse(is.na(.), 0, .)))

  return(long_data)
}

#' Fit item-level choice model
fit_item_level_model <- function(long_data, file_name, iter = 10000) {
  cat("Fitting item-level choice model...\n")
  cat("  Observations:", nrow(long_data), "\n")
  cat("  Trials:", length(unique(long_data$trial_id)), "\n")
  cat("  Subjects:", length(unique(long_data$subject_id)), "\n")

  m <- brm(
    chosen ~ item_value_z + PC1_z + PC2_z +
      item_value_z:PC1_z + item_value_z:PC2_z +
      (1 + item_value_z + PC1_z| subject_id) + (1 + item_value_z + PC2_z| trial_id),
    data = long_data,
    family = bernoulli(),
    iter = iter,
    chains = 4,
    cores = 4,
    backend = "cmdstanr",
    file = here("fits", file_name),
    file_refit = "on_change"
  )

  return(m)
}

#' Extract and save results from item-level model
save_item_level_results <- function(model, output_file) {
  posterior <- as_draws_df(model)

  effects <- list(
    item_value = extract_effect(posterior, "b_item_value_z"),
    PC1 = extract_effect(posterior, "b_PC1_z"),
    PC2 = extract_effect(posterior, "b_PC2_z"),
    value_x_PC1 = extract_effect(posterior, "b_item_value_z:PC1_z"),
    value_x_PC2 = extract_effect(posterior, "b_item_value_z:PC2_z")
  )

  results <- tibble(
    effect = names(effects),
    estimate = map_dbl(effects, "mean"),
    se = map_dbl(effects, "sd"),
    lower_ci = map_dbl(effects, "lower"),
    upper_ci = map_dbl(effects, "upper"),
    pd = map_dbl(effects, "pd")
  )

  write_csv(results, here("results", output_file))
  cat("Results saved to:", output_file, "\n")

  return(results)
}

# =============================================================================
# LOAD NETWORK DATA
# =============================================================================

cat("Loading network data with canonical PCA weights...\n")

source(here::here("src", "apply_pca_weights.R"))
canonical_loadings <- readRDS(here::here("output", "canonical_pca_loadings.rds"))

# Lee network (Fernandez data)
load(here::here("data", "lee_networkmetrics.RData"))
lee_net <- net_degree1
if ("weighted_transitivity" %in% names(lee_net)) {
  names(lee_net)[names(lee_net) == "weighted_transitivity"] <- "transitivity"
}
lee_net <- lee_net %>% select(-any_of(c("PCA1", "PCA2", "PC1", "PC2")))
lee_net <- apply_pca_weights(lee_net, canonical_loadings) %>%
  rename(PCA1 = PC1, PCA2 = PC2)

lee_centrality <- lee_net %>%
  select(item_id = Image, Name, PCA1, PCA2)

cat("Lee network items:", nrow(lee_centrality), "\n")

# Leng network
load(here::here("data", "leng_2024_networkmetrics.RData"))
leng_net <- net_degree
if ("weighted_transitivity" %in% names(leng_net)) {
  names(leng_net)[names(leng_net) == "weighted_transitivity"] <- "transitivity"
}
leng_net <- apply_pca_weights(leng_net, canonical_loadings) %>%
  rename(PCA1 = PC1, PCA2 = PC2)

leng_centrality <- leng_net %>%
  select(Name, PCA1, PCA2)

cat("Leng network items:", nrow(leng_centrality), "\n")

# =============================================================================
# ANALYSIS 1: LENG DATA
# =============================================================================

cat("\n")
cat("===========================================================\n")
cat("LENG ANALYSIS - Item Level\n")
cat("===========================================================\n")

# Load Leng data
shenhav_item_list <- read_csv(here::here("data", "shenhav_item_list.csv"),
                               show_col_types = FALSE)
Study3a_1 <- read_csv(here::here("data", "leng_2025", "Study3a_1.csv"),
                      col_types = cols(...1 = col_skip()))
Study3a_2 <- read_csv(here::here("data", "leng_2025", "Study3a_2.csv"),
                      col_types = cols(...1 = col_skip()))

# Match items between datasets 
Study3a_1$Item1 <- NA
Study3a_1$Item2 <- NA
Study3a_1$Item3 <- NA
Study3a_1$Item4 <- NA

pic_lookup <- setNames(shenhav_item_list$pic_name, shenhav_item_list$pic_path)
participant_ids <- unique(Study3a_1$participant)

for (participant_id in participant_ids) {
  study1_participant <- Study3a_1[Study3a_1$participant == participant_id, ]
  study2_participant <- Study3a_2[Study3a_2$participant == participant_id, ]

  if (nrow(study2_participant) == 0) next

  for (i in 1:nrow(study1_participant)) {
    value1 <- study1_participant$Value1[i]
    value2 <- study1_participant$Value2[i]
    value3 <- study1_participant$Value3[i]
    value4 <- study1_participant$Value4[i]

    assigned_pictures <- c()

    if (value1 != -1) {
      available_pictures <- setdiff(1:nrow(study2_participant), assigned_pictures)
      if (length(available_pictures) > 0) {
        match1_idx <- available_pictures[which.min(abs(study2_participant$value[available_pictures] - value1))]
        pic_path <- study2_participant$pic_path[match1_idx]
        study1_participant$Item1[i] <- pic_lookup[pic_path]
        assigned_pictures <- c(assigned_pictures, match1_idx)
      }
    }

    if (value2 != -1) {
      available_pictures <- setdiff(1:nrow(study2_participant), assigned_pictures)
      if (length(available_pictures) > 0) {
        match2_idx <- available_pictures[which.min(abs(study2_participant$value[available_pictures] - value2))]
        pic_path <- study2_participant$pic_path[match2_idx]
        study1_participant$Item2[i] <- pic_lookup[pic_path]
        assigned_pictures <- c(assigned_pictures, match2_idx)
      }
    }

    if (value3 != -1) {
      available_pictures <- setdiff(1:nrow(study2_participant), assigned_pictures)
      if (length(available_pictures) > 0) {
        match3_idx <- available_pictures[which.min(abs(study2_participant$value[available_pictures] - value3))]
        pic_path <- study2_participant$pic_path[match3_idx]
        study1_participant$Item3[i] <- pic_lookup[pic_path]
        assigned_pictures <- c(assigned_pictures, match3_idx)
      }
    }

    if (value4 != -1) {
      available_pictures <- setdiff(1:nrow(study2_participant), assigned_pictures)
      if (length(available_pictures) > 0) {
        match4_idx <- available_pictures[which.min(abs(study2_participant$value[available_pictures] - value4))]
        pic_path <- study2_participant$pic_path[match4_idx]
        study1_participant$Item4[i] <- pic_lookup[pic_path]
        assigned_pictures <- c(assigned_pictures, match4_idx)
      }
    }
  }

  idx <- which(Study3a_1$participant == participant_id)
  Study3a_1$Item1[idx] <- study1_participant$Item1
  Study3a_1$Item2[idx] <- study1_participant$Item2
  Study3a_1$Item3[idx] <- study1_participant$Item3
  Study3a_1$Item4[idx] <- study1_participant$Item4
}

# Filter valid items and add network metrics
valid_items_std <- tolower(gsub("_", " ", leng_centrality$Name))
standardized_pca1_map <- setNames(leng_centrality$PCA1, tolower(gsub("_", " ", leng_centrality$Name)))
standardized_pca2_map <- setNames(leng_centrality$PCA2, tolower(gsub("_", " ", leng_centrality$Name)))

is_valid_item <- function(item) {
  if (is.na(item)) return(TRUE)
  item_std <- tolower(item)
  return(item_std %in% valid_items_std)
}

valid_trials <- apply(Study3a_1[, c("Item1", "Item2", "Item3", "Item4")], 1,
                      function(row) all(sapply(row, is_valid_item)))

#only use data from the condition where people select a single item
leng_data <- Study3a_1[valid_trials, ] %>%
  filter(choiceCondition == 1, NChosen == 1)

# Add centrality
for (i in 1:4) {
  item_col <- paste0("Item", i)
  leng_data[[paste0("item_PCA1_", i-1)]] <-
    standardized_pca1_map[tolower(leng_data[[item_col]])]
  leng_data[[paste0("item_PCA2_", i-1)]] <-
    standardized_pca2_map[tolower(leng_data[[item_col]])]
}

# Determine choice position (which position was chosen first)
leng_data <- leng_data %>%
  mutate(
    choice_1 = case_when(
      Order1 == 1 ~ 0, Order2 == 1 ~ 1, Order3 == 1 ~ 2, Order4 == 1 ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  rename(subject_id = participant, trial = trialN)

cat("Leng k=1 trials:", nrow(leng_data), "\n")
cat("Subjects:", length(unique(leng_data$subject_id)), "\n")

# Reshape to item level
leng_long <- reshape_to_item_level(
  trial_data = leng_data,
  item_value_cols = c("Value1", "Value2", "Value3", "Value4"),
  item_name_cols = c("Item1", "Item2", "Item3", "Item4"),
  item_pca1_cols = c("item_PCA1_0", "item_PCA1_1", "item_PCA1_2", "item_PCA1_3"),
  item_pca2_cols = c("item_PCA2_0", "item_PCA2_1", "item_PCA2_2", "item_PCA2_3"),
  choice_col = "choice_1",
  subject_col = "subject_id",
  trial_col = "trial"
)

cat("Leng item-level observations:", nrow(leng_long), "\n")

# Filter to complete cases (have centrality data)
leng_long <- leng_long %>%
  filter(!is.na(PC1) & !is.na(PC2))

cat("Leng item-level (complete):", nrow(leng_long), "\n")

# Fit model
m_leng <- fit_item_level_model(leng_long, "item_level_leng_accuracy")

print(summary(m_leng))

# Save results
leng_results <- save_item_level_results(m_leng, "item_level_leng_accuracy_results.csv")
print(leng_results)

# =============================================================================
# ANALYSIS 2: FERNANDEZ EXP 1 (CHOOSE-K, k=1)
# =============================================================================

cat("\n")
cat("===========================================================\n")
cat("FERNANDEZ EXP 1 ANALYSIS - Item Level\n")
cat("===========================================================\n")

# Load Fernandez Exp 1 data
choosek <- read_csv(here::here("data", "choose_k", "choosek_R.csv"),
                    show_col_types = FALSE)

# Filter for k=1 trials
fern1_data <- choosek %>%
  filter(condition == 1)

cat("Fernandez Exp 1 k=1 trials:", nrow(fern1_data), "\n")
cat("Subjects:", length(unique(fern1_data$subject_id)), "\n")

# Add centrality for each item position
for (i in 0:3) {
  item_col <- paste0("item_name_", i)
  fern1_data[[paste0("item_PCA1_", i)]] <- lee_centrality$PCA1[match(fern1_data[[item_col]], lee_centrality$item_id)]
  fern1_data[[paste0("item_PCA2_", i)]] <- lee_centrality$PCA2[match(fern1_data[[item_col]], lee_centrality$item_id)]
}

# Reshape to item level
fern1_long <- reshape_to_item_level(
  trial_data = fern1_data,
  item_value_cols = c("item_value_0", "item_value_1", "item_value_2", "item_value_3"),
  item_name_cols = c("item_name_0", "item_name_1", "item_name_2", "item_name_3"),
  item_pca1_cols = c("item_PCA1_0", "item_PCA1_1", "item_PCA1_2", "item_PCA1_3"),
  item_pca2_cols = c("item_PCA2_0", "item_PCA2_1", "item_PCA2_2", "item_PCA2_3"),
  choice_col = "choice_1",
  subject_col = "subject_id",
  trial_col = "trial"
)

cat("Fernandez Exp 1 item-level observations:", nrow(fern1_long), "\n")

# Filter to complete cases
fern1_long <- fern1_long %>%
  filter(!is.na(PC1) & !is.na(PC2))

cat("Fernandez Exp 1 item-level (complete):", nrow(fern1_long), "\n")

# Fit model
m_fern1 <- fit_item_level_model(fern1_long, "item_level_fernandez_exp1_accuracy")

print(summary(m_fern1))

# Save results
fern1_results <- save_item_level_results(m_fern1, "item_level_fernandez_exp1_accuracy_results.csv")
print(fern1_results)

# =============================================================================
# ANALYSIS 3: FERNANDEZ EXP 2 (VARIABLE SET SIZES)
# =============================================================================

cat("\n")
cat("===========================================================\n")
cat("FERNANDEZ EXP 2 ANALYSIS - Item Level (by set size)\n")
cat("===========================================================\n")

# Load Fernandez Exp 2 data
exp2_data <- read_csv(here::here("data", "choose_k", "exp_2_processed_V2.csv"),
                      show_col_types = FALSE)

# Filter for k=1 trials
fern2_data <- exp2_data %>%
  filter(k == 1)

cat("Fernandez Exp 2 k=1 trials:", nrow(fern2_data), "\n")
cat("By set size:\n")
print(table(fern2_data$set_size))

# Function to process each set size
process_setsize <- function(data, set_size_val) {

  cat("\n--- Set Size", set_size_val, "---\n")

  ss_data <- data %>%
    filter(set_size == set_size_val)

  cat("Trials:", nrow(ss_data), "\n")

  # Add centrality for each item position
  for (i in 1:set_size_val) {
    item_col <- paste0("item_name_", i)
    ss_data[[paste0("item_PCA1_", i-1)]] <- lee_centrality$PCA1[match(ss_data[[item_col]], lee_centrality$item_id)]
    ss_data[[paste0("item_PCA2_", i-1)]] <- lee_centrality$PCA2[match(ss_data[[item_col]], lee_centrality$item_id)]
  }

  # For Exp 2, choice_1 contains the item ID, not position
  # We need to find which position has that item ID
  ss_data$choice_position <- NA_integer_
  for (i in 1:nrow(ss_data)) {
    chosen_item <- ss_data$choice_1[i]
    for (pos in 1:set_size_val) {
      if (ss_data[[paste0("item_name_", pos)]][i] == chosen_item) {
        ss_data$choice_position[i] <- pos - 1  # Convert to 0-indexed
        break
      }
    }
  }

  # Build column names for this set size
  value_cols <- paste0("item_value_", 1:set_size_val)
  name_cols <- paste0("item_name_", 1:set_size_val)
  pca1_cols <- paste0("item_PCA1_", 0:(set_size_val-1))
  pca2_cols <- paste0("item_PCA2_", 0:(set_size_val-1))

  # Reshape to item level (use choice_position instead of choice_1)
  long_data <- reshape_to_item_level(
    trial_data = ss_data,
    item_value_cols = value_cols,
    item_name_cols = name_cols,
    item_pca1_cols = pca1_cols,
    item_pca2_cols = pca2_cols,
    choice_col = "choice_position",
    subject_col = "subject_id",
    trial_col = "trial_index"
  )

  cat("Item-level observations:", nrow(long_data), "\n")

  # Filter to complete cases
  long_data <- long_data %>%
    filter(!is.na(PC1) & !is.na(PC2))

  cat("Item-level (complete):", nrow(long_data), "\n")

  return(long_data)
}

# Process each set size
fern2_ss4_long <- process_setsize(fern2_data, 4)
fern2_ss8_long <- process_setsize(fern2_data, 8)
fern2_ss12_long <- process_setsize(fern2_data, 12)

# Fit models for each set size
cat("\nFitting Set Size 4 model...\n")
m_fern2_ss4 <- fit_item_level_model(fern2_ss4_long, "item_level_fernandez_exp2_ss4_accuracy")
print(summary(m_fern2_ss4))
fern2_ss4_results <- save_item_level_results(m_fern2_ss4, "item_level_fernandez_exp2_ss4_accuracy_results.csv")
print(fern2_ss4_results)

cat("\nFitting Set Size 8 model...\n")
m_fern2_ss8 <- fit_item_level_model(fern2_ss8_long, "item_level_fernandez_exp2_ss8_accuracy")
print(summary(m_fern2_ss8))
fern2_ss8_results <- save_item_level_results(m_fern2_ss8, "item_level_fernandez_exp2_ss8_accuracy_results.csv")
print(fern2_ss8_results)

cat("\nFitting Set Size 12 model...\n")
m_fern2_ss12 <- fit_item_level_model(fern2_ss12_long, "item_level_fernandez_exp2_ss12_accuracy")
print(summary(m_fern2_ss12))
fern2_ss12_results <- save_item_level_results(m_fern2_ss12, "item_level_fernandez_exp2_ss12_accuracy_results.csv")
print(fern2_ss12_results)

# =============================================================================
# ANALYSIS 4: THOMAS 2021 (VARIABLE SET SIZES: 9, 16, 25, 36)
# =============================================================================

cat("\n")
cat("===========================================================\n")
cat("THOMAS 2021 ANALYSIS - Item Level (by set size)\n")
cat("===========================================================\n")

# Load Thomas stimulus mapping
thomas_mapping <- read_csv(here("data", "thomas2021_stimulus_mapping.csv"),
                           show_col_types = FALSE)

cat("Thomas stimulus mapping loaded:", nrow(thomas_mapping), "stimuli\n")

# Load Thomas network stats and apply canonical PCA
thomas_network <- read_csv(here("data", "thomas2021_network_stats.csv"),
                           show_col_types = FALSE) %>%
  rename(transitivity = weighted_transitivity)  # Match canonical column name

# Apply canonical PCA loadings
thomas_network <- thomas_network %>%
  select(-any_of(c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7")))

thomas_centrality_raw <- apply_pca_weights(thomas_network, canonical_loadings) %>%
  rename(PCA1 = PC1, PCA2 = PC2)

cat("Thomas network items:", nrow(thomas_centrality_raw), "\n")

# Create item centrality lookup by merging mapping with centrality
thomas_item_centrality <- thomas_mapping %>%
  left_join(thomas_centrality_raw, by = c("item_name" = "Name"))

# Fuzzy matching for items without centrality
fuzzy_match_map <- tribble(
  ~original_item,        ~network_item,
  "goldfishpretzel",     "goldfish",
  "milanomintchocolate", "milano",
  "pringles",            "pringlesred",
  "pringlesbbq",         "pringlesred"
)

# Apply fuzzy matches
for (i in 1:nrow(thomas_item_centrality)) {
  if (is.na(thomas_item_centrality$PCA1[i])) {
    item_name <- thomas_item_centrality$item_name[i]
    fuzzy_row <- fuzzy_match_map %>% filter(original_item == item_name)
    if (nrow(fuzzy_row) > 0) {
      network_item <- fuzzy_row$network_item[1]
      network_row <- thomas_centrality_raw %>% filter(Name == network_item)
      if (nrow(network_row) > 0) {
        thomas_item_centrality$PCA1[i] <- network_row$PCA1[1]
        thomas_item_centrality$PCA2[i] <- network_row$PCA2[1]
        cat("Fuzzy match:", item_name, "->", network_item, "\n")
      }
    }
  }
}

cat("Items with centrality:", sum(!is.na(thomas_item_centrality$PCA1)), "/",
    nrow(thomas_item_centrality), "\n")

# Create lookup by stimulus filename
thomas_pca1_lookup <- setNames(thomas_item_centrality$PCA1, thomas_item_centrality$stimulus)
thomas_pca2_lookup <- setNames(thomas_item_centrality$PCA2, thomas_item_centrality$stimulus)

# Thomas data directory
thomas_data_dir <- here::here("data", "thomas2021", "summary_files")
thomas_set_sizes <- c(9, 16, 25, 36)

# Function to process Thomas data for one set size
process_thomas_setsize <- function(set_size_val) {

  cat("\n--- Thomas Set Size", set_size_val, "---\n")

  # Load data for this set size
  file_path <- file.path(thomas_data_dir, paste0("setsize-", set_size_val, "_desc-data.csv"))
  ss_data <- read_csv(file_path, show_col_types = FALSE)

  cat("Trials:", nrow(ss_data), "\n")
  cat("Subjects:", length(unique(ss_data$subject)), "\n")

  # Rename subject column
  ss_data <- ss_data %>%
    rename(subject_id = subject) %>%
    mutate(trial_index = row_number())

  # Add centrality for each item position (0-indexed in Thomas data)
  for (i in 0:(set_size_val - 1)) {
    stim_col <- paste0("stimulus_", i)
    value_col <- paste0("item_value_", i)

    # Look up centrality by stimulus filename
    ss_data[[paste0("item_PCA1_", i)]] <- thomas_pca1_lookup[ss_data[[stim_col]]]
    ss_data[[paste0("item_PCA2_", i)]] <- thomas_pca2_lookup[ss_data[[stim_col]]]
  }

  # Build column names for this set size
  value_cols <- paste0("item_value_", 0:(set_size_val - 1))
  stim_cols <- paste0("stimulus_", 0:(set_size_val - 1))
  pca1_cols <- paste0("item_PCA1_", 0:(set_size_val - 1))
  pca2_cols <- paste0("item_PCA2_", 0:(set_size_val - 1))

  # Reshape to item level
  long_data <- reshape_to_item_level(
    trial_data = ss_data,
    item_value_cols = value_cols,
    item_name_cols = stim_cols,
    item_pca1_cols = pca1_cols,
    item_pca2_cols = pca2_cols,
    choice_col = "choice",  # Thomas uses 'choice' for chosen position (0-indexed)
    subject_col = "subject_id",
    trial_col = "trial_index"
  )

  cat("Item-level observations:", nrow(long_data), "\n")

  # Filter to complete cases
  long_data <- long_data %>%
    filter(!is.na(PC1) & !is.na(PC2))

  cat("Item-level (complete):", nrow(long_data), "\n")

  return(long_data)
}

# Process each Thomas set size
thomas_ss9_long <- process_thomas_setsize(9)
thomas_ss16_long <- process_thomas_setsize(16)
thomas_ss25_long <- process_thomas_setsize(25)
thomas_ss36_long <- process_thomas_setsize(36)

# Fit models for each Thomas set size
cat("\nFitting Thomas Set Size 9 model...\n")
m_thomas_ss9 <- fit_item_level_model(thomas_ss9_long, "item_level_thomas_ss9_accuracy")
print(summary(m_thomas_ss9))
thomas_ss9_results <- save_item_level_results(m_thomas_ss9, "item_level_thomas_ss9_accuracy_results.csv")
print(thomas_ss9_results)

cat("\nFitting Thomas Set Size 16 model...\n")
m_thomas_ss16 <- fit_item_level_model(thomas_ss16_long, "item_level_thomas_ss16_accuracy")
print(summary(m_thomas_ss16))
thomas_ss16_results <- save_item_level_results(m_thomas_ss16, "item_level_thomas_ss16_accuracy_results.csv")
print(thomas_ss16_results)

cat("\nFitting Thomas Set Size 25 model...\n")
m_thomas_ss25 <- fit_item_level_model(thomas_ss25_long, "item_level_thomas_ss25_accuracy")
print(summary(m_thomas_ss25))
thomas_ss25_results <- save_item_level_results(m_thomas_ss25, "item_level_thomas_ss25_accuracy_results.csv")
print(thomas_ss25_results)

cat("\nFitting Thomas Set Size 36 model...\n")
m_thomas_ss36 <- fit_item_level_model(thomas_ss36_long, "item_level_thomas_ss36_accuracy")
print(summary(m_thomas_ss36))
thomas_ss36_results <- save_item_level_results(m_thomas_ss36, "item_level_thomas_ss36_accuracy_results.csv")
print(thomas_ss36_results)

# =============================================================================
# SUMMARY
# =============================================================================

bayestestR::describe_posterior(m_leng)
bayestestR::describe_posterior(m_fern1)
bayestestR::describe_posterior(m_fern2_ss4)
bayestestR::describe_posterior(m_fern2_ss8)
bayestestR::describe_posterior(m_fern2_ss12)

bayestestR::describe_posterior(m_thomas_ss9)
bayestestR::describe_posterior(m_thomas_ss16)
bayestestR::describe_posterior(m_thomas_ss25)
bayestestR::describe_posterior(m_thomas_ss36)


cat("\n")
cat("===========================================================\n")
cat("ITEM-LEVEL ANALYSIS COMPLETE\n")
cat("===========================================================\n")

cat("\nResults files created:\n")
cat("  - results/item_level_leng_accuracy_results.csv\n")
cat("  - results/item_level_fernandez_exp1_accuracy_results.csv\n")
cat("  - results/item_level_fernandez_exp2_ss4_accuracy_results.csv\n")
cat("  - results/item_level_fernandez_exp2_ss8_accuracy_results.csv\n")
cat("  - results/item_level_fernandez_exp2_ss12_accuracy_results.csv\n")
cat("  - results/item_level_thomas_ss9_accuracy_results.csv\n")
cat("  - results/item_level_thomas_ss16_accuracy_results.csv\n")
cat("  - results/item_level_thomas_ss25_accuracy_results.csv\n")
cat("  - results/item_level_thomas_ss36_accuracy_results.csv\n")

