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

# Load necessary libraries
library(stats)
library(readxl)

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
calculate_averages_for_all <- function(data, neighbors_list) {
  # Initialize an empty data frame to store the final averaged values
  averaged_df <- data.frame(matrix(ncol = length(neighbors_list), nrow = nrow(data)))

  for (i in 1:length(neighbors_list)) {
    # Select the first column and the columns for the current set of neighbors
    cols_to_include <- c(i, neighbors_list[[i]])
    df <- data[, cols_to_include]

    # Calculate the row-wise average including the first column
    average_col <- rowMeans(df, na.rm = TRUE)

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
      name_left = net_degree$Name[net_degree$Image == item_name_left],
      name_right = net_degree$Name[net_degree$Image == item_name_right],
      choice = if_else(response == 1, 0, 1), # Reverse choice coding
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

Lee_Holyoak_2021_choice_data_exp2_5 <- read_csv("data/lee_2021_exp2_5.csv")
FoodNames <- readxl::read_excel(here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))
lee_2021_rating1 <- readr::read_csv(here("data", "lee_2021_rating1.csv"), col_names = FALSE, show_col_types = F)
names(lee_2021_rating1) <- FoodNames$Name

res_k <- vector(mode = "list", length = 3)
names(res_k) <- paste0("kNN_", seq_along(res_k))

# K <- 3 # Number of neighbors
# kk = 2
for (kk in 1:length(res_k)){
  neighbors <- findKNearestNeighbors(A, kk)
  names(neighbors) <- colnames(A)
  # print(neighbors)
  
  averaged_values_df <- calculate_averages_for_all(lee_2021_rating1, neighbors)
  
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
    summarise(acc = mean(correct),
              acc_k = mean(correct_k),
              diff_acc_k = acc -acc_k)
  
  # res_k[[kk]] <- avtest$diff_acc_k
  res_k[[kk]] <- avtest$acc_k
  
}

my_dataframe <- data.frame(res_k)
my_dataframe$acc <- avtest$acc
my_dataframe$subject_id <- unique(Lee_Holyoak_2021_choice_data_exp2_5$subject_id)

# write_csv(my_dataframe, "data/kNN30_Lee_Holyoak_2021.csv")
# my_dataframe <- readr::read_csv("data/kNN30_Lee_Holyoak_2021.csv")

long_data <- my_dataframe %>%
  pivot_longer(cols = c(acc, starts_with("kNN")), names_to = "metric", values_to = "accuracy")
long_data$metric <- factor(long_data$metric, levels = unique(long_data$metric[order(nchar(long_data$metric), long_data$metric)]))

ggplot(long_data, aes(x = metric, y = accuracy, fill = metric)) +
  geom_line(aes(group = subject_id, color= subject_id), alpha = 0.1)+
  geom_jitter(width = 0.3, alpha = 0.5) + # Overlay points with jitter to reduce overlap
  geom_violin(trim = FALSE) + # Draw violin plots. 'trim=FALSE' includes the tails in the plot
  labs(title = "", 
       x = "KNN", 
       y = "Accuracy") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black") + # Add mean points
  # scale_color_brewer(palette = "Set1") + # Use a color palette for differentiation
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_hline(yintercept = 0.5, linetype = "dashed", size = .4)+
  guides(color = "none", size = "none", fill = "none")

# # First, gather the accuracy metrics into a long format
# long_data <- my_dataframe %>%
#   pivot_longer(cols = starts_with("kNN"), names_to = "metric", values_to = "accuracy")
# long_data$metric <- factor(long_data$metric, levels = unique(long_data$metric[order(nchar(long_data$metric), long_data$metric)]))
# 
# ggplot(long_data, aes(x = metric, y = accuracy, fill = metric)) +
#   geom_line(aes(group = subject_id, color= subject_id), alpha = 0.1)+
#   geom_jitter(width = 0.3, alpha = 0.5) + # Overlay points with jitter to reduce overlap
#   geom_violin(trim = FALSE) + # Draw violin plots. 'trim=FALSE' includes the tails in the plot
#   labs(title = "", 
#        x = "KNN", 
#        y = "Accuracy Difference") +
#   stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black") + # Add mean points
#   # scale_color_brewer(palette = "Set1") + # Use a color palette for differentiation
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   guides(color = "none", size = "none", fill = "none")

