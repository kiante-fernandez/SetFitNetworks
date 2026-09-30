# exp_3_similarity_ratings - conducts correlation analysis for part one
# Copyright (C) 2024 Kianté Fernandez, <kiantefernan@gmail.com>
#
# Record of Revisions
#
# Date            Programmers                         Descriptions of Change
# ====         ================                       ======================
# 2024/01/17      Kianté  Fernandez                       
#------------------------------------------------------------------------------
# Setup and Data Loading
#------------------------------------------------------------------------------

# Required packages
required_packages <- c(
  "igraph", "purrr", "tidyverse", "here", "BayesFactor",
  "bayestestR", "see", "ggcorrplot","patchwork"
)

# Load packages with error handling
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed"))
  }
}

# Load source files and data
source(here::here("src", "utils.R"))
source(here::here("src", "exploratory_graph_analysis.R"))
load(file = here::here("data", "average_strength_100_6.RData"))

# Set up file paths
temp_files <- list.files(
  path = here::here("data", "exp_3", "drive-20230627"), 
  pattern = ".json", 
  full.names = TRUE
)

#------------------------------------------------------------------------------
# Network Initialization
#------------------------------------------------------------------------------

# Set up network
net_degree <- calculate_net_stats(g)
V(g)$name <- net_degree$Name

# Initialize weighted graph
G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
mem <- membership(cluster_leading_eigen(G))

#------------------------------------------------------------------------------
# Network Metrics Functions
#------------------------------------------------------------------------------

# Calculate network metrics for each subgraph
calculate_metrics <- function(subgraphs, g, net_degree) {
  list(
    modularity = map(subgraphs, function(x) {
      temp <- igraph::induced_subgraph(g, V(x)$name)
      as.numeric(igraph::modularity(temp, V(temp)$snack_type))
    }) %>% do.call(rbind, .),
    
    edge_density = map(subgraphs, ~edge_density(
      igraph::induced_subgraph(g, V(.)$name)
    )) %>% do.call(rbind, .),
    
    eigen_centrality = map(subgraphs, function(x) {
      temp <- igraph::induced_subgraph(g, V(x)$name)
      E(temp)$weight <- 2**((E(temp)$weight - min(E(temp)$weight)) / 
                              diff(range(E(temp)$weight)))
      mean(eigen_centrality(temp)$vector)
    }) %>% do.call(rbind, .),
    
    strength = map(subgraphs, ~mean(
      strength(igraph::induced_subgraph(g, V(.)$name))
    )) %>% do.call(rbind, .),
    
    degree = map(subgraphs, ~mean(
      degree(igraph::induced_subgraph(g, V(.)$name), normalized = TRUE)
    )) %>% do.call(rbind, .),
    
    pca = list(
      pca1 = map(subgraphs, ~mean(
        net_degree[net_degree$Name %in% V(.)$name, ]$PCA1
      )) %>% do.call(rbind, .),
      pca2 = map(subgraphs, ~mean(
        net_degree[net_degree$Name %in% V(.)$name, ]$PCA2
      )) %>% do.call(rbind, .),
      pca3 = map(subgraphs, ~mean(
        net_degree[net_degree$Name %in% V(.)$name, ]$PCA3
      )) %>% do.call(rbind, .)
    ),
    
    conductance = map(subgraphs, function(x) {
      x <- igraph::induced_subgraph(g, V(x)$name)
      mem[names(mem)] = 1
      mem[names(mem) %in% V(x)$name] = 2
      unlist(clustAnalytics::conductance(g, mem)[2])
    })
  )
}

#------------------------------------------------------------------------------
# Data Processing Functions
#------------------------------------------------------------------------------

#' Process similarity ratings from JSON data
#' @param data Path to JSON data file
#' @param metrics List of pre-calculated network metrics
#' @return Processed data frame with similarity ratings and metrics
similarity_ratings <- function(data, metrics) {
  # Initialize storage
  set_values_temp <- vector(mode = "numeric", length = 100)
  
  # Load subject data
  subject_temp <- jsonlite::parse_json(jsonlite::read_json(data), 
                                       simplifyVector = TRUE)
  
  # Process similarity ratings
  subject_rating_temp <- subject_temp %>%
    filter(screen_id == "similarity") %>%
    select(stimulus, response) %>%
    mutate(
      stimulus = str_remove(stimulus, 
                            "../../img/grid_stimuli/grid_6_average_strength_") %>%
        str_remove(".jpg") %>%
        as.numeric()
    ) %>%
    unnest(response) %>%
    mutate(
      # Normalize ratings
      responsenormalized = (response - min(response)) / diff(range(response)),
      # Add metrics
      modularity = as.vector(metrics$modularity),
      edge_densi = as.vector(metrics$edge_density),
      degree_res = as.vector(metrics$degree),
      strength_res = as.vector(metrics$strength),
      eigen_cen = as.vector(metrics$eigen_centrality),
      con_cen = as.vector(unlist(metrics$conductance)),
      pca1 = as.vector(metrics$pca$pca1),
      pca2 = as.vector(metrics$pca$pca2),
      pca3 = as.vector(metrics$pca$pca3),
      subject_id = unique(subject_temp$subject_id)
    )
}

#------------------------------------------------------------------------------
# Analysis
#------------------------------------------------------------------------------

# Calculate all metrics
metrics <- calculate_metrics(subgraphs, g, net_degree)

# Process all files
res <- map_df(temp_files, ~similarity_ratings(., metrics))

# Calculate summary statistics
compares <- res %>%
  group_by(stimulus) %>%
  summarise(
    subgraph_mean = mean(responsenormalized),  # ratings normalized within participant, as in Set-Choice Study 2
    se = sqrt(var(responsenormalized) / length(responsenormalized)),
    subgraph_sd = sd(responsenormalized)
  ) %>%
  mutate(
    mod = metrics$modularity[,1],
    ed = metrics$edge_density[,1],
    st = metrics$strength[,1],
    d = metrics$degree[,1],
    ec = metrics$eigen_centrality[,1],
    c = unlist(metrics$conductance),
    pca1 = metrics$pca$pca1[,1],
    pca2 = metrics$pca$pca2[,1],
    pca3 = metrics$pca$pca3[,1],
    experiment = 3
  )

write_csv(compares, here::here("results", "similarity_sets_exp3.csv"))  # per-set data for Fig. 2

#------------------------------------------------------------------------------
# Statistical Analysis Functions
#------------------------------------------------------------------------------

#' Run correlation analyses for all metrics
#' @param data Data frame containing variables to analyze
#' @return List of correlation results
run_correlations <- function(data) {
  # List of variables to correlate with subgraph_mean
  vars <- c("st", "ed", "mod", "ec", "c", "pca1", "pca2", "pca3", "d")
  
  # Run Bayesian correlations
  bayesian_results <- map(vars, function(var) {
    result <- correlationBF(data$subgraph_mean, data[[var]])
    list(
      correlation = result,
      posterior = describe_posterior(result, test = "p_direction"),
      bf_models = bayesfactor_models(result)
    )
  }) %>% setNames(vars)
  
  # Run frequentist correlations
  freq_results <- map(vars, function(var) {
    cor.test(data$subgraph_mean, data[[var]], method = "spearman")
  }) %>% setNames(vars)
  
  list(
    bayesian = bayesian_results,
    frequentist = freq_results
  )
}

# Run correlations
correlation_results <- run_correlations(compares)
correlation_results

#------------------------------------------------------------------------------
# Visualization Functions
#------------------------------------------------------------------------------

#' Create a scatter plot with error bars and regression line
#' @param data Data frame containing the data
#' @param x_var Name of x variable
#' @param x_label Label for x axis
#' @return ggplot object
create_scatter_plot <- function(data, x_var, x_label) {
  ggplot(data, aes(!!sym(x_var), subgraph_mean)) +
    geom_point() +
    theme_classic() +
    geom_pointrange(
      aes(ymin = subgraph_mean - se, 
          ymax = subgraph_mean + se), 
      size = .7, 
      color = "red"
    ) +
    geom_smooth(
      method = "lm", 
      se = TRUE, 
      size = 1.8, 
      color = "black"
    ) +
    labs(x = x_label, y = "Similarity") +
    theme(
      axis.text = element_text(face = "bold"),
      text = element_text(size = 15),
      axis.title = element_text(face = "bold")
    )
}

#' Create correlation matrix plot
#' @param data Data frame containing variables for correlation
#' @return ggplot object
create_correlation_matrix <- function(data) {
  data %>%
    dplyr::select(subgraph_mean, mod, ed, st, c) %>%
    cor() %>%
    ggcorrplot::ggcorrplot(
      type = "upper",
      lab = TRUE
    ) +
    theme_classic() +
    labs(x = "", y = "") +
    theme(
      axis.text = element_text(face = "bold"),
      text = element_text(size = 15),
      axis.title = element_text(face = "bold")
    )
}

#------------------------------------------------------------------------------
# Generate Plots
#------------------------------------------------------------------------------

# Define plot specifications
plot_specs <- list(
  eigen_centrality = list(var = "ec", label = "Eigen Centrality"),
  edge_density = list(var = "ed", label = "Edge Density"),
  strength = list(var = "st", label = "Strength"),
  degree = list(var = "d", label = "Degree"),
  conductance = list(var = "c", label = "Conductance"),
  pca1 = list(var = "pca1", label = "PCA1"),
  pca2 = list(var = "pca2", label = "PCA2"),
  pca3 = list(var = "pca3", label = "PCA3")
)

# Generate all scatter plots
scatter_plots <- map(plot_specs, ~create_scatter_plot(
  compares, 
  .x$var, 
  .x$label
))

# Generate correlation matrix
correlation_matrix <- create_correlation_matrix(compares)

#------------------------------------------------------------------------------
# Function to Arrange
#------------------------------------------------------------------------------

#' Arrange multiple plots in a grid
#' @param plot_list List of ggplot objects
#' @param ncol Number of columns in grid
#' @return Arranged plot grid
arrange_plots <- function(plot_list, ncol = 3) {
  patchwork::wrap_plots(plot_list, ncol = ncol)
}

# Arrange all scatter plots in a grid
scatter_grid <- arrange_plots(scatter_plots, ncol = 3)
scatter_grid
correlation_matrix

