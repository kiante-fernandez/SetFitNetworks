# item_level_rt_analysis.R - RT analysis for multi-alternative choice
#
# This script analyzes response time (RT) at the trial level where:
#   - Unit of observation: 1 row per trial
#   - Outcome: log(RT in ms)
#   - Predictors: chosen item's value, PC1, PC2 (z-scored within subject)
#   - Random effects: (1 | subject_id)
#
# Applied to: Leng, Fernandez Exp 1, Fernandez Exp 2, Thomas 2021
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

#' Fit RT model
fit_rt_model <- function(rt_data, file_name, iter = 10000) {
  cat("Fitting RT model...\n")
  cat("  Trials:", nrow(rt_data), "\n")
  cat("  Subjects:", length(unique(rt_data$subject_id)), "\n")

  m <- brm(
    logtotal_rt ~ chosen_value_z + chosen_PC1_z + chosen_PC2_z +
      chosen_value_z:chosen_PC1_z + chosen_value_z:chosen_PC2_z +
      (1 | subject_id),
    data = rt_data,
    family = gaussian(),
    iter = iter,
    chains = 4,
    cores = 4,
    backend = "cmdstanr",
    file = here("fits", file_name)
  )

  return(m)
}

#' Extract and save results from RT model
save_rt_results <- function(model, output_file) {
  posterior <- as_draws_df(model)

  effects <- list(
    chosen_value = extract_effect(posterior, "b_chosen_value_z"),
    chosen_PC1 = extract_effect(posterior, "b_chosen_PC1_z"),
    chosen_PC2 = extract_effect(posterior, "b_chosen_PC2_z"),
    value_x_PC1 = extract_effect(posterior, "b_chosen_value_z:chosen_PC1_z"),
    value_x_PC2 = extract_effect(posterior, "b_chosen_value_z:chosen_PC2_z")
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

# Thomas network
thomas_mapping <- read_csv(here("data", "thomas2021_stimulus_mapping.csv"),
                           show_col_types = FALSE)
thomas_network <- read_csv(here("data", "thomas2021_network_stats.csv"),
                           show_col_types = FALSE) %>%
  rename(transitivity = weighted_transitivity)
thomas_network <- thomas_network %>%
  select(-any_of(c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7")))
thomas_centrality_raw <- apply_pca_weights(thomas_network, canonical_loadings) %>%
  rename(PCA1 = PC1, PCA2 = PC2)

thomas_item_centrality <- thomas_mapping %>%
  left_join(thomas_centrality_raw, by = c("item_name" = "Name"))

# Fuzzy matching for Thomas
fuzzy_match_map <- tribble(
  ~original_item,        ~network_item,
  "goldfishpretzel",     "goldfish",
  "milanomintchocolate", "milano",
  "pringles",            "pringlesred",
  "pringlesbbq",         "pringlesred"
)

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
      }
    }
  }
}

thomas_pca1_lookup <- setNames(thomas_item_centrality$PCA1, thomas_item_centrality$stimulus)
thomas_pca2_lookup <- setNames(thomas_item_centrality$PCA2, thomas_item_centrality$stimulus)

cat("Thomas network items with centrality:", sum(!is.na(thomas_item_centrality$PCA1)), "\n")

# =============================================================================
# ANALYSIS 1: LENG DATA - RT
# =============================================================================

cat("\n")
cat("===========================================================\n")
cat("LENG RT ANALYSIS\n")
cat("===========================================================\n")

# Load Leng data
shenhav_item_list <- read_csv(here::here("data", "shenhav_item_list.csv"),
                               show_col_types = FALSE)
Study3a_1 <- read_csv(here::here("data", "leng_2025", "Study3a_1.csv"),
                      col_types = cols(...1 = col_skip()))
Study3a_2 <- read_csv(here::here("data", "leng_2025", "Study3a_2.csv"),
                      col_types = cols(...1 = col_skip()))

# Match items (same as accuracy analysis)
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

leng_data <- Study3a_1[valid_trials, ] %>%
  filter(choiceCondition == 1, NChosen == 1)

# Add centrality for each position
for (i in 1:4) {
  item_col <- paste0("Item", i)
  leng_data[[paste0("item_PCA1_", i)]] <-
    standardized_pca1_map[tolower(leng_data[[item_col]])]
  leng_data[[paste0("item_PCA2_", i)]] <-
    standardized_pca2_map[tolower(leng_data[[item_col]])]
}

# Determine choice position (which position was chosen first)
leng_data <- leng_data %>%
  mutate(
    choice_pos = case_when(
      Order1 == 1 ~ 1, Order2 == 1 ~ 2, Order3 == 1 ~ 3, Order4 == 1 ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  rename(subject_id = participant)

# Extract chosen item features and RT
leng_rt_data <- leng_data %>%
  rowwise() %>%
  mutate(
    chosen_value = case_when(
      choice_pos == 1 ~ Value1,
      choice_pos == 2 ~ Value2,
      choice_pos == 3 ~ Value3,
      choice_pos == 4 ~ Value4
    ),
    chosen_PC1 = case_when(
      choice_pos == 1 ~ item_PCA1_1,
      choice_pos == 2 ~ item_PCA1_2,
      choice_pos == 3 ~ item_PCA1_3,
      choice_pos == 4 ~ item_PCA1_4
    ),
    chosen_PC2 = case_when(
      choice_pos == 1 ~ item_PCA2_1,
      choice_pos == 2 ~ item_PCA2_2,
      choice_pos == 3 ~ item_PCA2_3,
      choice_pos == 4 ~ item_PCA2_4
    ),
    total_rt = RT1 * 1000  # Convert to ms
  ) %>%
  ungroup() %>%
  filter(!is.na(chosen_PC1) & !is.na(chosen_PC2) & !is.na(total_rt))

cat("Leng trials before RT exclusion:", nrow(leng_rt_data), "\n")

# Apply RT exclusions
leng_rt_data <- exclude_rt(leng_rt_data)

cat("Leng trials after RT exclusion:", nrow(leng_rt_data), "\n")

# Z-score and log transform
leng_rt_data <- leng_rt_data %>%
  group_by(subject_id) %>%
  mutate(
    chosen_value_z = scale(chosen_value)[,1],
    chosen_PC1_z = scale(chosen_PC1)[,1],
    chosen_PC2_z = scale(chosen_PC2)[,1],
    logtotal_rt = log(total_rt)
  ) %>%
  ungroup() %>%
  mutate(across(ends_with("_z"), ~ifelse(is.na(.), 0, .)))

# Fit model
m_leng_rt <- fit_rt_model(leng_rt_data, "item_level_leng_rt")
print(summary(m_leng_rt))
leng_rt_results <- save_rt_results(m_leng_rt, "item_level_leng_rt_results.csv")
print(leng_rt_results)

# =============================================================================
# ANALYSIS 2: FERNANDEZ EXP 1 - RT
# =============================================================================

cat("\n")
cat("===========================================================\n")
cat("FERNANDEZ EXP 1 RT ANALYSIS\n")
cat("===========================================================\n")

# Load Fernandez Exp 1 data
choosek <- read_csv(here::here("data", "choose_k", "choosek_R.csv"),
                    show_col_types = FALSE)

# Filter for k=1 trials
fern1_data <- choosek %>%
  filter(condition == 1)

# Add centrality for each item position
for (i in 0:3) {
  item_col <- paste0("item_name_", i)
  fern1_data[[paste0("item_PCA1_", i)]] <- lee_centrality$PCA1[match(fern1_data[[item_col]], lee_centrality$item_id)]
  fern1_data[[paste0("item_PCA2_", i)]] <- lee_centrality$PCA2[match(fern1_data[[item_col]], lee_centrality$item_id)]
}

# Extract chosen item features (choice_1 is 0-indexed)
fern1_rt_data <- fern1_data %>%
  rowwise() %>%
  mutate(
    choice_pos = choice_1 + 1,  # Convert to 1-indexed
    chosen_value = case_when(
      choice_pos == 1 ~ item_value_0,
      choice_pos == 2 ~ item_value_1,
      choice_pos == 3 ~ item_value_2,
      choice_pos == 4 ~ item_value_3
    ),
    chosen_PC1 = case_when(
      choice_pos == 1 ~ item_PCA1_0,
      choice_pos == 2 ~ item_PCA1_1,
      choice_pos == 3 ~ item_PCA1_2,
      choice_pos == 4 ~ item_PCA1_3
    ),
    chosen_PC2 = case_when(
      choice_pos == 1 ~ item_PCA2_0,
      choice_pos == 2 ~ item_PCA2_1,
      choice_pos == 3 ~ item_PCA2_2,
      choice_pos == 4 ~ item_PCA2_3
    ),
    total_rt = rt_1
  ) %>%
  ungroup() %>%
  filter(!is.na(chosen_PC1) & !is.na(chosen_PC2) & !is.na(total_rt))

cat("Fernandez Exp1 trials before RT exclusion:", nrow(fern1_rt_data), "\n")

# Apply RT exclusions
fern1_rt_data <- exclude_rt(fern1_rt_data)

cat("Fernandez Exp1 trials after RT exclusion:", nrow(fern1_rt_data), "\n")

# Z-score and log transform
fern1_rt_data <- fern1_rt_data %>%
  group_by(subject_id) %>%
  mutate(
    chosen_value_z = scale(chosen_value)[,1],
    chosen_PC1_z = scale(chosen_PC1)[,1],
    chosen_PC2_z = scale(chosen_PC2)[,1],
    logtotal_rt = log(total_rt)
  ) %>%
  ungroup() %>%
  mutate(across(ends_with("_z"), ~ifelse(is.na(.), 0, .)))

# Fit model
m_fern1_rt <- fit_rt_model(fern1_rt_data, "item_level_fernandez_exp1_rt")
print(summary(m_fern1_rt))
fern1_rt_results <- save_rt_results(m_fern1_rt, "item_level_fernandez_exp1_rt_results.csv")
print(fern1_rt_results)

# =============================================================================
# ANALYSIS 3: FERNANDEZ EXP 2 - RT (by set size)
# =============================================================================

cat("\n")
cat("===========================================================\n")
cat("FERNANDEZ EXP 2 RT ANALYSIS (by set size)\n")
cat("===========================================================\n")

# Load Fernandez Exp 2 data
exp2_data <- read_csv(here::here("data", "choose_k", "exp_2_processed_V2.csv"),
                      show_col_types = FALSE)

# Filter for k=1 trials
fern2_data <- exp2_data %>%
  filter(k == 1)

# Function to process RT for each set size
process_fern2_rt <- function(data, set_size_val) {

  cat("\n--- Fernandez Exp2 Set Size", set_size_val, "---\n")

  ss_data <- data %>%
    filter(set_size == set_size_val)

  # Add centrality for each item position
  for (i in 1:set_size_val) {
    item_col <- paste0("item_name_", i)
    ss_data[[paste0("item_PCA1_", i-1)]] <- lee_centrality$PCA1[match(ss_data[[item_col]], lee_centrality$item_id)]
    ss_data[[paste0("item_PCA2_", i-1)]] <- lee_centrality$PCA2[match(ss_data[[item_col]], lee_centrality$item_id)]
  }

  # Find choice position (choice_1 contains item ID, not position)
  ss_data$choice_position <- NA_integer_
  for (i in 1:nrow(ss_data)) {
    chosen_item <- ss_data$choice_1[i]
    for (pos in 1:set_size_val) {
      if (ss_data[[paste0("item_name_", pos)]][i] == chosen_item) {
        ss_data$choice_position[i] <- pos - 1  # 0-indexed
        break
      }
    }
  }

  # Extract chosen item features using vectorized approach
  rt_data <- ss_data %>%
    mutate(
      choice_pos = choice_position + 1,  # Convert to 1-indexed
      total_rt = rt_1
    )

  # Extract chosen features for each row
  rt_data$chosen_value <- sapply(1:nrow(rt_data), function(i) {
    pos <- rt_data$choice_pos[i]
    if (is.na(pos) || pos < 1 || pos > set_size_val) return(NA)
    rt_data[[paste0("item_value_", pos)]][i]
  })

  rt_data$chosen_PC1 <- sapply(1:nrow(rt_data), function(i) {
    pos <- rt_data$choice_position[i]
    if (is.na(pos)) return(NA)
    rt_data[[paste0("item_PCA1_", pos)]][i]
  })

  rt_data$chosen_PC2 <- sapply(1:nrow(rt_data), function(i) {
    pos <- rt_data$choice_position[i]
    if (is.na(pos)) return(NA)
    rt_data[[paste0("item_PCA2_", pos)]][i]
  })

  rt_data <- rt_data %>%
    filter(!is.na(chosen_PC1) & !is.na(chosen_PC2) & !is.na(total_rt))

  cat("Trials before RT exclusion:", nrow(rt_data), "\n")

  # Apply RT exclusions
  rt_data <- exclude_rt(rt_data)

  cat("Trials after RT exclusion:", nrow(rt_data), "\n")

  # Z-score and log transform
  rt_data <- rt_data %>%
    group_by(subject_id) %>%
    mutate(
      chosen_value_z = scale(chosen_value)[,1],
      chosen_PC1_z = scale(chosen_PC1)[,1],
      chosen_PC2_z = scale(chosen_PC2)[,1],
      logtotal_rt = log(total_rt)
    ) %>%
    ungroup() %>%
    mutate(across(ends_with("_z"), ~ifelse(is.na(.), 0, .)))

  return(rt_data)
}

# Process each set size
fern2_ss4_rt <- process_fern2_rt(fern2_data, 4)
fern2_ss8_rt <- process_fern2_rt(fern2_data, 8)
fern2_ss12_rt <- process_fern2_rt(fern2_data, 12)

# Fit models
cat("\nFitting Fernandez Exp2 SS4 RT model...\n")
m_fern2_ss4_rt <- fit_rt_model(fern2_ss4_rt, "item_level_fernandez_exp2_ss4_rt")
print(summary(m_fern2_ss4_rt))
fern2_ss4_rt_results <- save_rt_results(m_fern2_ss4_rt, "item_level_fernandez_exp2_ss4_rt_results.csv")
print(fern2_ss4_rt_results)

cat("\nFitting Fernandez Exp2 SS8 RT model...\n")
m_fern2_ss8_rt <- fit_rt_model(fern2_ss8_rt, "item_level_fernandez_exp2_ss8_rt")
print(summary(m_fern2_ss8_rt))
fern2_ss8_rt_results <- save_rt_results(m_fern2_ss8_rt, "item_level_fernandez_exp2_ss8_rt_results.csv")
print(fern2_ss8_rt_results)

cat("\nFitting Fernandez Exp2 SS12 RT model...\n")
m_fern2_ss12_rt <- fit_rt_model(fern2_ss12_rt, "item_level_fernandez_exp2_ss12_rt")
print(summary(m_fern2_ss12_rt))
fern2_ss12_rt_results <- save_rt_results(m_fern2_ss12_rt, "item_level_fernandez_exp2_ss12_rt_results.csv")
print(fern2_ss12_rt_results)

# =============================================================================
# ANALYSIS 4: THOMAS 2021 - RT (by set size)
# =============================================================================

cat("\n")
cat("===========================================================\n")
cat("THOMAS 2021 RT ANALYSIS (by set size)\n")
cat("===========================================================\n")

thomas_data_dir <- here::here("data", "thomas2021", "summary_files")
thomas_set_sizes <- c(9, 16, 25, 36)

# Function to process Thomas RT for one set size
process_thomas_rt <- function(set_size_val) {

  cat("\n--- Thomas Set Size", set_size_val, "---\n")

  file_path <- file.path(thomas_data_dir, paste0("setsize-", set_size_val, "_desc-data.csv"))
  ss_data <- read_csv(file_path, show_col_types = FALSE)

  ss_data <- ss_data %>%
    rename(subject_id = subject)

  # Add centrality for each item position
  for (i in 0:(set_size_val - 1)) {
    stim_col <- paste0("stimulus_", i)
    ss_data[[paste0("item_PCA1_", i)]] <- thomas_pca1_lookup[ss_data[[stim_col]]]
    ss_data[[paste0("item_PCA2_", i)]] <- thomas_pca2_lookup[ss_data[[stim_col]]]
  }

  # Extract chosen item features using vectorized approach
  rt_data <- ss_data %>%
    mutate(
      choice_pos = choice + 1,  # Convert to 1-indexed (choice is 0-indexed position)
      total_rt = rt_choice_indication
    )

  # Extract chosen features for each row
  rt_data$chosen_value <- sapply(1:nrow(rt_data), function(i) {
    pos <- rt_data$choice[i]  # 0-indexed
    if (is.na(pos)) return(NA)
    rt_data[[paste0("item_value_", pos)]][i]
  })

  rt_data$chosen_PC1 <- sapply(1:nrow(rt_data), function(i) {
    pos <- rt_data$choice[i]  # 0-indexed
    if (is.na(pos)) return(NA)
    rt_data[[paste0("item_PCA1_", pos)]][i]
  })

  rt_data$chosen_PC2 <- sapply(1:nrow(rt_data), function(i) {
    pos <- rt_data$choice[i]  # 0-indexed
    if (is.na(pos)) return(NA)
    rt_data[[paste0("item_PCA2_", pos)]][i]
  })

  rt_data <- rt_data %>%
    filter(!is.na(chosen_PC1) & !is.na(chosen_PC2) & !is.na(total_rt))

  cat("Trials before RT exclusion:", nrow(rt_data), "\n")

  # Apply RT exclusions
  rt_data <- exclude_rt(rt_data)

  cat("Trials after RT exclusion:", nrow(rt_data), "\n")

  # Z-score and log transform
  rt_data <- rt_data %>%
    group_by(subject_id) %>%
    mutate(
      chosen_value_z = scale(chosen_value)[,1],
      chosen_PC1_z = scale(chosen_PC1)[,1],
      chosen_PC2_z = scale(chosen_PC2)[,1],
      logtotal_rt = log(total_rt)
    ) %>%
    ungroup() %>%
    mutate(across(ends_with("_z"), ~ifelse(is.na(.), 0, .)))

  return(rt_data)
}

# Process each Thomas set size
thomas_ss9_rt <- process_thomas_rt(9)
thomas_ss16_rt <- process_thomas_rt(16)
thomas_ss25_rt <- process_thomas_rt(25)
thomas_ss36_rt <- process_thomas_rt(36)

# Fit models
cat("\nFitting Thomas SS9 RT model...\n")
m_thomas_ss9_rt <- fit_rt_model(thomas_ss9_rt, "item_level_thomas_ss9_rt")
print(summary(m_thomas_ss9_rt))
thomas_ss9_rt_results <- save_rt_results(m_thomas_ss9_rt, "item_level_thomas_ss9_rt_results.csv")
print(thomas_ss9_rt_results)

cat("\nFitting Thomas SS16 RT model...\n")
m_thomas_ss16_rt <- fit_rt_model(thomas_ss16_rt, "item_level_thomas_ss16_rt")
print(summary(m_thomas_ss16_rt))
thomas_ss16_rt_results <- save_rt_results(m_thomas_ss16_rt, "item_level_thomas_ss16_rt_results.csv")
print(thomas_ss16_rt_results)

cat("\nFitting Thomas SS25 RT model...\n")
m_thomas_ss25_rt <- fit_rt_model(thomas_ss25_rt, "item_level_thomas_ss25_rt")
print(summary(m_thomas_ss25_rt))
thomas_ss25_rt_results <- save_rt_results(m_thomas_ss25_rt, "item_level_thomas_ss25_rt_results.csv")
print(thomas_ss25_rt_results)

cat("\nFitting Thomas SS36 RT model...\n")
m_thomas_ss36_rt <- fit_rt_model(thomas_ss36_rt, "item_level_thomas_ss36_rt")
print(summary(m_thomas_ss36_rt))
thomas_ss36_rt_results <- save_rt_results(m_thomas_ss36_rt, "item_level_thomas_ss36_rt_results.csv")
print(thomas_ss36_rt_results)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("===========================================================\n")
cat("RT ANALYSIS COMPLETE\n")
cat("===========================================================\n")

cat("\nResults files created:\n")
cat("  - results/item_level_leng_rt_results.csv\n")
cat("  - results/item_level_fernandez_exp1_rt_results.csv\n")
cat("  - results/item_level_fernandez_exp2_ss4_rt_results.csv\n")
cat("  - results/item_level_fernandez_exp2_ss8_rt_results.csv\n")
cat("  - results/item_level_fernandez_exp2_ss12_rt_results.csv\n")
cat("  - results/item_level_thomas_ss9_rt_results.csv\n")
cat("  - results/item_level_thomas_ss16_rt_results.csv\n")
cat("  - results/item_level_thomas_ss25_rt_results.csv\n")
cat("  - results/item_level_thomas_ss36_rt_results.csv\n")

cat("\nModel formula used:\n")
cat("  logtotal_rt ~ chosen_value_z + chosen_PC1_z + chosen_PC2_z +\n")
cat("                chosen_value_z:chosen_PC1_z + chosen_value_z:chosen_PC2_z +\n")
cat("                (1 | subject_id)\n")
