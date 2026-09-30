# exp_2_similarity_ratings - conducts correlation analysis for part one
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

# Load required data and utilities
source(here::here("src", "utils.R"))
source(here::here("src", "exploratory_graph_analysis.R"))
load(file = here::here("data", "modularity_100_6.RData"))
temp_files <- list.files(path = here::here("data", "exp_2"), pattern = ".json", full.names = T)

#------------------------------------------------------------------------------
# Network Initialization
#------------------------------------------------------------------------------

# Initialize network
net_degree <- calculate_net_stats(g)
V(g)$name <- net_degree$Name

# Prepare weighted graph
G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
mem <- membership(cluster_leading_eigen(G))

#------------------------------------------------------------------------------
# Calculate Network Metrics
#------------------------------------------------------------------------------

# Calculate precision for each subgraph
set_precision <- map(subgraphs, function(x) {
  mean(net_degree[net_degree$Name %in% V(x)$name, ]$precision)
}) %>% do.call(rbind, .)

# Calculate standard deviation for each subgraph
set_sd <- map(subgraphs, function(x) {
  mean(net_degree[net_degree$Name %in% V(x)$name, ]$sd)
}) %>% 
  do.call(rbind, .) %>%
  magrittr::multiply_by(100) %>%
  round()

# Calculate modularity for each subgraph
mod_res <- map(subgraphs, function(x) {
  temp <- igraph::induced_subgraph(g, V(x)$name)
  as.numeric(igraph::modularity(temp, V(temp)$snack_type))
}) %>% do.call(rbind, .)

# Calculate edge density for each subgraph
edge_dens <- map(subgraphs, function(x) {
  edge_density(igraph::induced_subgraph(g, V(x)$name))
}) %>% do.call(rbind, .)

# Calculate eigenvector centrality for each subgraph
eigen_cen <- map(subgraphs, function(x) {
  temp <- igraph::induced_subgraph(g, V(x)$name)
  E(temp)$weight <- 2**((E(temp)$weight - min(E(temp)$weight)) / 
                          diff(range(E(temp)$weight)))
  mean(eigen_centrality(temp)$vector)
}) %>% do.call(rbind, .)

# Calculate strength for each subgraph
strength_res <- map(subgraphs, function(x) {
  mean(strength(igraph::induced_subgraph(g, V(x)$name)))
}) %>% do.call(rbind, .)

# Calculate normalized degree for each subgraph
degree_res <- map(subgraphs, function(x) {
  mean(degree(igraph::induced_subgraph(g, V(x)$name), normalized = TRUE))
}) %>% do.call(rbind, .)

# Calculate PCA components for each subgraph
pca1 <- map(subgraphs, function(x) {
  temp <- igraph::induced_subgraph(g, V(x)$name)
  mean(net_degree[net_degree$Name %in% V(temp)$name, ]$PCA1)
}) %>% do.call(rbind, .)

pca2 <- map(subgraphs, function(x) {
  temp <- igraph::induced_subgraph(g, V(x)$name)
  mean(net_degree[net_degree$Name %in% V(temp)$name, ]$PCA2)
}) %>% do.call(rbind, .)

pca3 <- map(subgraphs, function(x) {
  temp <- igraph::induced_subgraph(g, V(x)$name)
  mean(net_degree[net_degree$Name %in% V(temp)$name, ]$PCA3)
}) %>% do.call(rbind, .)

# Calculate conductance for each subgraph
con_res <- map(subgraphs, function(x) {
  x <- igraph::induced_subgraph(g, V(x)$name)
  mem[names(mem)] <- 1
  mem[names(mem) %in% V(x)$name] <- 2
  conductance_temp <- clustAnalytics::conductance(g, mem)[2]
  unlist(conductance_temp)
})

#------------------------------------------------------------------------------
# Data Processing Functions
#------------------------------------------------------------------------------

#' Process similarity ratings from JSON data
#' @param data Path to JSON data file
#' @return Processed data frame with similarity ratings and metrics
similarity_ratings <- function(data) {
  # Initialize storage for set values
  set_values_temp <- vector(mode = "numeric", length = 100)
  
  # Load and parse subject data
  subject_temp <- jsonlite::parse_json(jsonlite::read_json(data), simplifyVector = TRUE)
  
  # Process similarity ratings
  subject_rating_temp <- subject_temp %>%
    filter(screen_id == "similarity") %>%
    select(stimulus, response) %>%
    mutate(
      stimulus = stringr::str_remove(stimulus, pattern = "../../img/grid_stimuli/grid_6_modularity_") %>%
        stringr::str_remove(pattern = ".jpg") %>%
        as.numeric()
    ) %>%
    unnest(response)
  
  # Process individual ratings
  subject_value_temp <- subject_temp %>%
    filter(screen_id == "ratings") %>%
    select(stimulus, response) %>%
    mutate(
      Image = stringr::str_remove(stimulus, pattern = "../../img/60Foods/item") %>%
        stringr::str_remove(pattern = ".jpg") %>%
        as.numeric()
    ) %>%
    dplyr::left_join(load_food_names()$foods_in_image, by = "Image")
  
  # Calculate set values
  for (foo in 1:100) {
    set_values_temp[[foo]] <- sum(do.call(rbind, 
                                          subject_value_temp[subject_value_temp$Name %in% res[[foo]], ]$response))
  }
  
  # Normalize ratings and add metrics
  subject_rating_temp %>%
    mutate(
      responsenormalized = (response - min(response)) / diff(range(response)),
      modularity = as.vector(mod_res),
      edge_densi = as.vector(edge_dens),
      degree_res = as.vector(degree_res),
      strength_res = as.vector(strength_res),
      eigen_cen = as.vector(eigen_cen),
      con_cen = as.vector(unlist(con_res)),
      pca1 = as.vector(pca1),
      pca2 = as.vector(pca2),
      pca3 = as.vector(pca3),
      ratings = set_values_temp,
      set_precision = as.vector(set_precision),
      subject_id = unique(subject_temp$subject_id)
    )
}
#------------------------------------------------------------------------------
# Visualization Functions
#------------------------------------------------------------------------------

#' Create a standardized histogram plot
#' @param data Data frame containing the data
#' @param var Variable to plot
#' @param x_label X-axis label
#' @param bins Number of bins
#' @param show_mean Whether to show mean line
create_histogram <- function(data, var, x_label, bins = 50, show_mean = TRUE) {
  p <- ggplot(data, aes({{var}})) +
    geom_histogram(color = "black", 
                   fill = "dodgerblue1", 
                   alpha = .8, 
                   bins = bins) +
    theme_classic() +
    labs(x = x_label, y = "Count") +
    theme(
      axis.text = element_text(face = "bold"),
      text = element_text(size = 15),
      axis.title = element_text(face = "bold")
    )
  
  if (show_mean) {
    p <- p + geom_vline(xintercept = mean(pull(data, {{var}})), 
                        linetype = "dashed", 
                        size = .7)
  }
  
  return(p)
}

#------------------------------------------------------------------------------
# Data Processing Pipeline
#------------------------------------------------------------------------------

# Process all files
res_list <- map(temp_files, similarity_ratings)
res <- map_df(temp_files, similarity_ratings)

#------------------------------------------------------------------------------
# Generate Visualizations
#------------------------------------------------------------------------------

# Create histograms for different metrics
plot_list <- list(
  ratings = create_histogram(res, ratings, "Sum ratings"),
  similarity = create_histogram(res, response, "Similarity Judgment"),
  normalized = create_histogram(res, responsenormalized, "Similarity Judgment (normalized)"),
  modularity = create_histogram(res, modularity, "Q", bins = 11),
  edge_density = create_histogram(res, edge_densi, "Edge Density", bins = 9),
  conductance = create_histogram(res, con_cen, "Conductance", bins = 8),
  pca1 = create_histogram(res, pca1, "PCA1", bins = 8),
  pca2 = create_histogram(res, pca2, "PCA2", bins = 8),
  precision = create_histogram(res, set_precision, "Precision", bins = 8)
)
plot_list
#------------------------------------------------------------------------------
# Summary Statistics Calculation
#------------------------------------------------------------------------------

# Calculate summary statistics by stimulus
compares <- res %>%
  group_by(stimulus) %>%
  summarise(
    subgraph_mean = mean(responsenormalized),
    se = sqrt(var(responsenormalized) / length(responsenormalized)),
    subgraph_sd = sd(responsenormalized),
    .groups = 'drop'
  ) %>%
  mutate(
    # Add network metrics
    mod = mod_res[,1],
    ed = edge_dens[,1],
    st = strength_res[,1],
    d = degree_res[,1],
    ec = eigen_cen[,1],
    c = unlist(con_res),
    # Add PCA components
    pca1 = pca1[,1],
    pca2 = pca2[,1],
    pca3 = pca3[,1],
    # Add additional metrics
    set_precision = set_precision[,1],
    experiment = 2
  )

# Save results
write_csv(compares, here::here("results", "similarity_sets_exp2.csv"))  # per-set data for Fig. 2

#------------------------------------------------------------------------------
# Correlation Analysis Functions
#------------------------------------------------------------------------------

#' Perform Bayesian correlation analysis
#' @param data Data frame containing variables
#' @param var Variable to correlate with subgraph_mean
#' @param scale_vars Whether to scale variables before correlation
#' @return List containing correlation results
run_bayesian_correlation <- function(data, var, scale_vars = FALSE) {
  if (scale_vars) {
    x <- scale(data$subgraph_mean)
    y <- scale(data[[var]])
  } else {
    x <- data$subgraph_mean
    y <- data[[var]]
  }
  
  result <- correlationBF(x, y)
  posterior <- describe_posterior(result, test = "p_direction")
  bf_models <- bayesfactor_models(result)
  
  list(
    correlation = result,
    posterior = posterior,
    bf_models = bf_models
  )
}

#' Perform frequentist correlation analysis
#' @param data Data frame containing variables
#' @param var Variable to correlate with subgraph_mean
#' @param method Correlation method (spearman or kendall)
#' @param scale_vars Whether to scale variables before correlation
#' @return Correlation test results
run_correlation_test <- function(data, var, method = "spearman", scale_vars = FALSE) {
  if (scale_vars) {
    x <- scale(data$subgraph_mean)
    y <- scale(data[[var]])
  } else {
    x <- data$subgraph_mean
    y <- data[[var]]
  }
  
  cor.test(x, y, method = method)
}

#------------------------------------------------------------------------------
# Run Analyses
#------------------------------------------------------------------------------

# Bayesian correlations
bayesian_results <- list(
  strength = run_bayesian_correlation(compares, "st"),
  edge_density = run_bayesian_correlation(compares, "ed"),
  modularity = run_bayesian_correlation(compares, "mod"),
  eigen_centrality = run_bayesian_correlation(compares, "ec"),
  conductance = run_bayesian_correlation(compares, "c"),
  pca2 = run_bayesian_correlation(compares, "pca2", scale_vars = TRUE)
)
bayesian_results
# Frequentist correlations
correlation_results <- list(
  conductance = run_correlation_test(compares, "c"),
  strength = run_correlation_test(compares, "st"),
  edge_density = run_correlation_test(compares, "ed"),
  eigen_centrality = run_correlation_test(compares, "ec"),
  pca1 = run_correlation_test(compares, "pca1", method = "kendall", scale_vars = TRUE),
  pca2 = run_correlation_test(compares, "pca2"),
  degree = run_correlation_test(compares, "d"),
  pca3 = run_correlation_test(compares, "pca3")
)
correlation_results
#------------------------------------------------------------------------------
# Generate Reports
#------------------------------------------------------------------------------

# Function to generate correlation reports
generate_correlation_reports <- function(results) {
  map(results, report::report)
}

# Generate reports for all correlations
correlation_reports <- generate_correlation_reports(correlation_results)
correlation_reports
#------------------------------------------------------------------------------
# Visualization Functions
#------------------------------------------------------------------------------

#' Plot Bayes factor models
#' @param result Bayesian correlation result
plot_bf_models <- function(result) {
  plot(bayesfactor_models(result)) +
    scale_fill_pizza()
}
# Generate plots
bf_plots <- map(bayesian_results, ~plot_bf_models(.x$correlation))
bf_plots
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
  set_precision = list(var = "set_precision", label = "Set Precision"),
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

