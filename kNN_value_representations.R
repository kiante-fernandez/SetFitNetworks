# kNN_value_representations.R -
#
# Copyright (C) 2024 Kianté Fernandez, <kiantefernan@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Record of Revisions
#
# Date            Programmers                         Descriptions of Change
# ====         ================                       ======================
# 24/02/27      Kianté  Fernandez                       wrote code
# 24/02/28      Kianté  Fernandez                      added loop over kappa 

# Load necessary libraries
library(stats)
library(readxl)
library(patchwork)

weightedRowMeansSimilarity <- function(df, similarities) {

  # Adjust similarities to include the initial 1 for the first value's self-similarity
  similarities <- c(1, similarities)

  # Check if the number of columns in df matches the length of similarities
  if (ncol(df) != length(similarities)) {
    stop("The number of columns in the dataframe must match the number of provided similarity measures.")
  }

  # Function to calculate weighted average for a single row
  calculateRowWeightedAverage <- function(row) {
    total_weight <- sum(similarities)
    weighted_sum <- sum(row * similarities, na.rm = TRUE)
    weighted_average <- weighted_sum / total_weight
    return(weighted_average)
  }

  # Apply the function to each row
  row_weighted_averages <- apply(df, 1, calculateRowWeightedAverage)

  return(row_weighted_averages)
}

# # Custom function to calculate weighted row means
# weightedRowMeans <- function(df, weights, na.rm = TRUE) {
#   # Ensure weights length matches the number of columns in df
#   if(length(weights) != ncol(df)) {
#     stop("Length of weights must match the number of columns in df")
#   }
#
#   # Apply weights, handle NA values if na.rm is TRUE
#   weightedSums <- apply(df, 1, function(x) sum(x * weights, na.rm = na.rm))
#
#   # Calculate total weights applied to each row, adjusting for NAs if necessary
#   totalWeights <- apply(!is.na(df), 1, function(x) sum(weights[x]))
#
#   # Compute the weighted means
#   weightedMeans <- weightedSums / totalWeights
#
#   return(weightedMeans)
# }
#

# Define the function to find K nearest neighbors
findKNearestNeighbors <- function(weighted_adj_matrix, K) {
  num_nodes <- nrow(weighted_adj_matrix)
  neighbors <- vector("list", num_nodes)

  for (i in 1:num_nodes) {
    distances <- numeric(num_nodes)

    for (j in 1:num_nodes) {
      if (i != j) {
        distances[j] <- dist(rbind(weighted_adj_matrix[i, ], weighted_adj_matrix[j, ]))
      } else {
        distances[j] <- Inf # Set distance to itself as Infinity
      }
    }
    # Get indices of the K smallest distances
    nearest_indices <- order(distances, decreasing = FALSE)[1:K]
    # Store the indices of the K nearest neighbors
    neighbors[[i]] <- nearest_indices
  }
  return(neighbors)
}

# Function to calculate row-wise average including the first column for all neighbors
calculate_averages_for_all <- function(data, neighbors_list, similarities, kappa = 2) {
  # Initialize an empty data frame to store the final averaged values
  averaged_df <- data.frame(matrix(ncol = length(neighbors_list), nrow = nrow(data)))

  for (i in 1:length(neighbors_list)) {
    # Select the first column and the columns for the current set of neighbors
    cols_to_include <- c(i, neighbors_list[[i]])
    df <- data[, cols_to_include]
    # Calculate the row-wise average including the first column
    # average_col <- rowMeans(df, na.rm = TRUE)
    # Instead calculate an weighed average based on sim
    weights <- similarities[[i]] * kappa
    average_col <- weightedRowMeansSimilarity(df, weights)

    # Store the averaged values in the final data frame
    averaged_df[, i] <- average_col
  }

  # Set the column names of the final data frame to match the first column's name across all sets of neighbors
  colnames(averaged_df) <- colnames(data)

  return(averaged_df)
}

# Function to map item names to their average values for a specific subject
apply_averaged_values <- function(choice_data, averaged_values_df, net_degree, subn) {
  choice_data %>%
    mutate(rt = rt * 1000) %>%
    rowwise() %>%
    mutate(
      name_left = net_degree$Name[net_degree$Image == item_number_left],
      name_right = net_degree$Name[net_degree$Image == item_number_right],
      choice = if_else(choice == 1, 0, 1), # Reverse choice coding
      item_value_left_k = averaged_values_df[subn, colnames(averaged_values_df) %in% name_left],
      item_value_right_k = averaged_values_df[subn, colnames(averaged_values_df) %in% name_right],
      vd_k = item_value_left_k - item_value_right_k
    ) %>%
    ungroup() # To ensure subsequent operations are not performed in a rowwise manner
}
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
    filter(rt > 250 & rt < 9000) # Apply response time cutoffs
}

# Example usage
# Lee_Holyoak_word2vec <- read_excel("data/Lee_Holyoak_word2vec.xlsx")
# Lee_Holyoak_word2vec <- Lee_Holyoak_word2vec[,-1]
# neighbors <- findKNearestNeighbors(Lee_Holyoak_word2vec, K)
# names(neighbors) <- colnames(Lee_Holyoak_word2vec)
# print(neighbors)

source("exploratory_graph_analysis.R")
source(here::here("src", "utils.R"))
# Calculate network statistics
net_degree <- calculate_net_stats(g)
A <- ega_res[["typicalGraph"]][["graph"]]
# Lee_Holyoak_GPT3 <- read_csv("data/Lee_Holyoak_GPT3.csv")
# A <- Lee_Holyoak_GPT3

Lee_Holyoak_2021_choice_data_exp2_5 <- read_csv("data/lee_2021_exp2_5v2.csv")
FoodNames <- readxl::read_excel(here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))
lee_2021_rating1 <- readr::read_csv(here("data", "lee_2021_rating_SubIDs.csv"), col_names = FALSE, show_col_types = F)
names(lee_2021_rating1) <- c("subject_id", FoodNames$Name)
lee_2021_rating1 <- lee_2021_rating1[, -1]

K <- 25 # Number of neighbors
# kappa <- 1 # controls the magnitude of influence from the present correlation
# gamma #general boost in the use of average from other options (value zero to 1)

kappa_values <- 1:5 # Define kappa values to iterate over

for (kappa in kappa_values) {
  res_k <- vector(mode = "list", length = K)
  res_k_diff <- vector(mode = "list", length = K)
  names(res_k) <- paste0("kNN_", seq_along(res_k))
  names(res_k_diff) <- paste0("kNN_", seq_along(res_k))

  # kk = 1
  for (kk in 1:length(res_k)) {
    neighbors <- findKNearestNeighbors(A, kk)
    names(neighbors) <- colnames(A)
    similarities <- vector("list", length(neighbors))

    for (i in 1:length(neighbors)) {
      cols_to_include <- c(i, neighbors[[i]])
      similarities[i] <- list(A[cols_to_include[1], cols_to_include[-1]])
    }

    averaged_values_df <- calculate_averages_for_all(lee_2021_rating1, neighbors, similarities, kappa = kappa)

    # Initialize an empty data frame to store all subjects' data
    all_subjects_data <- data.frame()

    for (ii in seq_along(unique(Lee_Holyoak_2021_choice_data_exp2_5$subject_id))) {
      sub_idx <- unique(Lee_Holyoak_2021_choice_data_exp2_5$subject_id)[ii]

      specific_subject_data <- Lee_Holyoak_2021_choice_data_exp2_5 %>%
        filter(subject_id == sub_idx)

      sub_dat <- apply_averaged_values(specific_subject_data, averaged_values_df, net_degree, ii)

      # Append this subject's data to the overall dataset
      all_subjects_data <- bind_rows(all_subjects_data, sub_dat)
    }

    test <- all_subjects_data %>%
      exclusions() %>%
      mutate(
        correct = if_else(
          (item_value_left > item_value_right & choice == 1) |
            (item_value_right > item_value_left & choice == 0),
          1, # Mark as correct
          0 # Mark as incorrect
        ),
        correct_k = if_else(
          (item_value_left_k > item_value_right_k & choice == 1) |
            (item_value_right_k > item_value_left_k & choice == 0),
          1, # Mark as correct
          0 # Mark as incorrect
        )
      )

    avtest <- test %>%
      group_by(subject_id) %>%
      summarise(
        acc = mean(correct),
        acc_k = mean(correct_k),
        diff_acc_k = acc - acc_k
      )

    res_k_diff[[kk]] <- avtest$diff_acc_k
    res_k[[kk]] <- avtest$acc_k
  }

  my_dataframe <- data.frame(res_k)
  my_dataframe$acc <- avtest$acc
  my_dataframe$subject_id <- unique(Lee_Holyoak_2021_choice_data_exp2_5$subject_id)

  write_csv(my_dataframe, paste0("data/kNN25_Lee_Holyoak_2021_kappa_", kappa, ".csv"))
  
  long_data <- my_dataframe %>%
    pivot_longer(cols = c(acc, starts_with("kNN")), names_to = "metric", values_to = "accuracy")
  long_data$metric <- factor(long_data$metric, levels = unique(long_data$metric[order(nchar(long_data$metric), long_data$metric)]))

  p1 <- ggplot(long_data, aes(x = metric, y = accuracy, fill = metric)) +
    geom_line(aes(group = subject_id, color = subject_id), alpha = 0.1) +
    geom_jitter(width = 0.3, alpha = 0.5) + # Overlay points with jitter to reduce overlap
    geom_violin(trim = FALSE) + # Draw violin plots. 'trim=FALSE' includes the tails in the plot
    labs(
      title = "",
      x = "KNN",
      y = "Accuracy"
    ) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") + # Add mean points
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", size = .4) +
    guides(color = "none", size = "none", fill = "none") +
    scale_fill_manual(values = c(
      "#3182bd", "#3182bd", "#6baed6", "#9ecae1", "#9ecae1",
      "#c6dbef", "#e6550d", "#e6550d", "#fd8d3c", "#fdae6b",
      "#fdae6b", "#fdd0a2", "#31a354", "#31a354", "#74c476",
      "#a1d99b", "#c7e9c0", "#c7e9c0", "#756bb1", "#9e9ac8",
      "#9e9ac8", "#bcbddc", "#dadaeb", "#dadaeb", "#636363",
      "#969696", "#969696", "#bdbdbd", "#d9d9d9", "#d9d9d9"
    ))
  
  my_dataframe <- data.frame(res_k_diff)
  my_dataframe$acc <- avtest$acc
  my_dataframe$subject_id <- unique(Lee_Holyoak_2021_choice_data_exp2_5$subject_id)

  write_csv(my_dataframe, paste0("data/kNN25_DIFF_Lee_Holyoak_2021_kappa_", kappa, ".csv"))
  
  # # First, gather the accuracy metrics into a long format
  long_data <- my_dataframe %>%
    pivot_longer(cols = starts_with("kNN"), names_to = "metric", values_to = "accuracy")
  long_data$metric <- factor(long_data$metric, levels = unique(long_data$metric[order(nchar(long_data$metric), long_data$metric)]))
  long_data$accuracy <- long_data$accuracy * -1 # recoe to make more accurate positive
  
  p2 <- ggplot(long_data, aes(x = metric, y = accuracy, fill = metric)) +
    geom_line(aes(group = subject_id, color = subject_id), alpha = 0.15) +
    # geom_jitter(width = 0.3,height = 0.05, alpha = 0.4) + # Overlay points with jitter to reduce overlap
    geom_point(alpha = 0.4) + # Overlay points with jitter to reduce overlap
    geom_violin(trim = TRUE) + # Draw violin plots. 'trim=FALSE' includes the tails in the plot
    labs(
      title = "",
      x = "KNN",
      y = expression(Delta * " Accuracy")
    ) + # Use expression to add Delta symbol
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") + # Add mean points
    # scale_fill_brewer(palette = "Set1") + # Use a color palette for differentiation
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
    guides(color = "none", size = "none", fill = "none") +
    scale_fill_manual(values = c(
      "#3182bd", "#3182bd", "#6baed6", "#9ecae1", "#9ecae1",
      "#c6dbef", "#e6550d", "#e6550d", "#fd8d3c", "#fdae6b",
      "#fdae6b", "#fdd0a2", "#31a354", "#31a354", "#74c476",
      "#a1d99b", "#c7e9c0", "#c7e9c0", "#756bb1", "#9e9ac8",
      "#9e9ac8", "#bcbddc", "#dadaeb", "#dadaeb", "#636363",
      "#969696", "#969696", "#bdbdbd", "#d9d9d9", "#d9d9d9"
    ))

  p_fin <- p1 / p2 + plot_annotation(title = paste0("kappa = ", kappa))
  print(p_fin)
  ggsave(paste0("/Users/kiantefernandez/Downloads/kNN_accuracy_kappa_", kappa, ".png"), p_fin)
  
}
