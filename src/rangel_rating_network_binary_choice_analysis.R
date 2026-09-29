
library(tidyverse)
library(EGAnet)
library(parallel)

load(here::here("data", "smith_krajbich_2018", "ACADchoiceandeyedata.RData"))

image_files <- readr::read_csv(here::here("data", "smith_krajbich_2018", "food_stimuli.csv"), show_col_types = FALSE)$filename
namesimages <- as_tibble(image_files) %>%
  rename(filename = value) %>%
  mutate(
    Picture = row_number(),
    image_name = str_remove(filename, "\\.[^.]+$"),
    image_name = str_remove(image_name, "img_"),
    image_name = str_to_lower(image_name)
  )

# Create the name columns by joining with the namesimages lookup table
twofoodchoicedata$FoodLeftName <- namesimages$image_name[match(twofoodchoicedata$FoodLeft, namesimages$Picture)]
twofoodchoicedata$FoodRightName <- namesimages$image_name[match(twofoodchoicedata$FoodRight, namesimages$Picture)]
twofoodchoicedata$choice <- if_else(twofoodchoicedata$LeftRight == 1, 1, 0)
# if_else(twofoodchoicedata$LeftRight == 1, 1, 0)
# ============================================================================
# SECTION 1: LOAD ALL DATASETS AND IDENTIFY TARGET ITEMS
# ============================================================================

# Load all datasets
rangel_final_df <- readr::read_csv(here::here("data", "liking_initiative", "rangel_final_database.csv"))
yoo_2024_df <- readr::read_csv(here::here("data", "liking_initiative", "EA_fMRI_value_rating.csv"))
smith_2025_df <- readr::read_csv(here::here("data", "liking_initiative", "Steph2025_ratings_combined_both_studies.csv"))

# ============================================================================
# SECTION 2: STANDARDIZE ALL DATASETS
# ============================================================================

cat("\n### Standardizing all datasets...\n")

# Standardize Yoo 2024
yoo_2024_standardized <- yoo_2024_df %>%
  mutate(
    dataset_subjectid = paste0("yoo2024_", subj_id),
    item_name = str_to_lower(food_name) %>%
      str_replace_all("'", "") %>%
      str_replace_all("\\s+", "") %>%
      str_replace_all("-", ""),
    rating = rating,
    rating_scale = "0_to_10"
  ) %>%
  select(dataset_subjectid, item_name, rating, rating_scale)

# Standardize Smith 2025
smith_2025_standardized <- smith_2025_df %>%
  mutate(
    dataset_subjectid = paste0("smith2025_s", study, "_", subject),
    item_name = str_to_lower(image) %>%
      str_replace_all("'", "") %>%
      str_replace_all("\\s+", "") %>%
      str_replace_all("-", ""),
    rating = rating,
    rating_scale = "0.01_to_4"
  ) %>%
  select(dataset_subjectid, item_name, rating, rating_scale)

# Rangel is already formatted
rangel_standardized <- rangel_final_df

# Combine all
combined_df <- bind_rows(
  rangel_standardized,
  yoo_2024_standardized,
  smith_2025_standardized
)

# Create a new column for the standardized names
combined_df$item_name_std <- combined_df$item_name

sort(unique(twofoodchoicedata$FoodLeftName))
# combined_df[combined_df$item_name == "layssweetbbq","item_name"] <- "lays_sweetbbq"
combined_df$item_name[combined_df$item_name == "layssweetbbq"] <- "lays_sweetbbq"

# Get the final count
final_unique_names <- unique(combined_df$item_name_std)
cat("Number of unique items reduced from", 
    length(unique(combined_df$item_name)), 
    "to", length(final_unique_names), "\n")

# Print the final, sorted list for review
print(sort(final_unique_names))

# Get the 146 items of interest from smikrab2018
target_items <- combined_df %>%
  filter(str_detect(dataset_subjectid, "smikrab2018")) %>%
  pull(item_name_std) %>%
  unique()

sort(target_items)

# target_items <- namesimages %>% 
#   pull(image_name) %>%
#   unique()
  

unique(combined_df[stringr::str_detect(combined_df$dataset_subjectid, "smikrab2018"), "dataset_subjectid"])

# ============================================================================
# SECTION 4: FILTER FOR TARGET ITEMS ONLY
# ============================================================================
combined_df$item_name_std
# Filter to only include target items
combined_filtered <- combined_df %>%
  filter(item_name_std %in% target_items)

cat("Before filtering: ", n_distinct(combined_df$item_name_std), "unique items\n")
cat("After filtering: ", n_distinct(combined_filtered$item_name_std), "unique items\n")
cat("Rows retained: ", nrow(combined_filtered), "\n")

# Check coverage: which items are present in each dataset
item_coverage <- combined_filtered %>%
  mutate(dataset = case_when(
    str_detect(dataset_subjectid, "^yoo2024") ~ "Yoo 2024",
    str_detect(dataset_subjectid, "^smith2025") ~ "Smith 2025",
    TRUE ~ "Rangel"
  )) %>%
  group_by(dataset, item_name_std) %>%
  summarise(n_ratings = n(), .groups = "drop") %>%
  pivot_wider(names_from = dataset, values_from = n_ratings, values_fill = 0)

# Handle duplicates
combined_clean <- combined_filtered %>%
  group_by(dataset_subjectid, item_name_std) %>%
  summarise(
    rating = mean(rating, na.rm = TRUE),
    rating_scale = first(rating_scale),
    .groups = "drop"
  )

# More general dataset extraction
combined_normalized <- combined_clean %>%
  mutate(
    dataset = case_when(
      # Handle hasdes specifically (remove _X.X pattern)  
      str_detect(dataset_subjectid, "^hasdes_") ~ "hasdes",
      # Handle other patterns - remove trailing numbers (integers or decimals) and subject IDs
      TRUE ~ str_replace(dataset_subjectid, "_[0-9]+(?:\\.[0-9]+)?(?:_.*)?$", "")
    )
  )

cat("\nUnique datasets found:\n")
unique_datasets <- unique(combined_normalized$dataset)
print(sort(unique_datasets))
cat("Total unique datasets:", length(unique_datasets), "\n")

combined_normalized <- combined_normalized %>%
  group_by(dataset) %>%
  mutate(
    rating_min = min(rating, na.rm = TRUE),
    rating_max = max(rating, na.rm = TRUE),
    rating_range = rating_max - rating_min,
    rating_normalized = if_else(rating_range == 0, 
                                0.5,  
                                (rating - rating_min) / rating_range)
  ) %>%
  ungroup()

# Summary by dataset
normalization_summary <- combined_normalized %>%
  group_by(dataset) %>%
  summarise(
    n_subjects = n_distinct(dataset_subjectid),
    original_min = min(rating, na.rm = TRUE),
    original_max = max(rating, na.rm = TRUE), 
    normalized_min = min(rating_normalized, na.rm = TRUE),
    normalized_max = max(rating_normalized, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dataset)

print(normalization_summary)

# ============================================================================
# SECTION 6: CREATE WIDE FORMAT
# ============================================================================

cat("\n### Creating wide format...\n")
names(combined_normalized)
rangel_wide <- combined_normalized %>%
  select(dataset_subjectid, item_name_std, rating_normalized) %>%
  pivot_wider(
    names_from = item_name_std,
    values_from = rating_normalized,
    values_fill = NA
  )

cat("Wide format dimensions: ", nrow(rangel_wide), "subjects x", 
    ncol(rangel_wide) - 1, "items\n")


cat("\n### Filtering items with poor coverage...\n")

# Calculate missing percentages
missing_percent <- rangel_wide %>%
  select(-dataset_subjectid) %>%
  summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "percent_missing")

items_to_keep <- missing_percent %>%
  filter(percent_missing < 99) %>%
  pull(item)

rangel_wide_filtered <- rangel_wide %>%
  select(dataset_subjectid, all_of(items_to_keep))

cat("Items retained after coverage filter: ", length(items_to_keep), "\n")

# Show items that were dropped
dropped_items <- setdiff(names(rangel_wide)[-1], items_to_keep)
if (length(dropped_items) > 0) {
  cat("\nItems dropped due to >90% missing:\n")
  cat(paste("-", dropped_items, collapse = "\n"), "\n")
}

# ============================================================================
# SECTION 8: PREPARE FOR NETWORK ANALYSIS
# ============================================================================

cat("\n### Preparing final matrix...\n")

rangel_for_network <- rangel_wide_filtered %>%
  select(-dataset_subjectid)

# Impute missing values
if (any(is.na(rangel_for_network))) {
  cat("Imputing missing values...\n")
  rangel_for_network_final <- rangel_for_network %>%
    mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
} else {
  rangel_for_network_final <- rangel_for_network
}

cat("Final dimensions: ", dim(rangel_for_network_final), "\n")

# ============================================================================
# SECTION 9: SAVE FILTERED DATA
# ============================================================================
# 
# cat("\n### Saving filtered data...\n")
# 
# write_csv(rangel_wide_filtered, "smikrab2018_items_wide_with_ids.csv")
# write_csv(rangel_for_network_final, "smikrab2018_items_for_network.csv")
# saveRDS(rangel_for_network_final, "smikrab2018_items_network_data.rds")
# 
# # Save the list of items actually used
# items_used <- data.frame(
#   item_name = names(rangel_for_network_final),
#   original_target = names(rangel_for_network_final) %in% target_items_standardized
# )
# write_csv(items_used, "smikrab2018_items_used.csv")

# ============================================================================
# SECTION 10: RUN EGA ON FILTERED DATA
# ============================================================================

cat("\n### Running EGA on filtered dataset...\n")

# Run EGA
# ega_results <- EGA(
#   data = rangel_for_network_final,
#   model = "glasso",
#   algorithm = "walktrap",
#   ncores = detectCores() - 1
# )
# 
if (!file.exists(here("data", "rangel_rating_network_graph.RData"))) {
  set.seed(2025)
  
  # # Bootstrap for stability
  boot_ega_results <- bootEGA(
    data = rangel_for_network_final,
    iter = 1000,
    typicalStructure = TRUE,
    model = "glasso",
    algorithm = "walktrap",
    type = "parametric",
    ncores = detectCores() - 1
  )
  # get adjacency matrix
  A <- boot_ega_results[["typicalGraph"]][["graph"]]
  # get clusters
  dimattributes <- boot_ega_results[["typicalGraph"]][["wc"]]
  # create igraph object
  g <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
  # add decorate attributes
  V(g)$product_type <- dimattributes
  
  save(boot_ega_results, g, file = here("data", "rangel_rating_network_graph.RData"))
  
}else {
  load(here::here("data", "rangel_rating_network_graph.RData"))
}

# 
# 
# # Stability Analysis ----------------------------------------------------------
# cat("\n", rep("=", 60), "\n", sep = "")
# cat("STABILITY ANALYSIS\n")
# cat(rep("=", 60), "\n\n", sep = "")
# 
# cat("Calculating dimension stability...\n")
# rangel.dimstab <- dimensionStability(boot_ega_results)
# 
# # Display stability metrics
# cat("\nDimension Stability Metrics:\n")
# cat("- Structural Consistency:", 
#     round(rangel.dimstab$dimension.stability$structural.consistency, 3), "\n")
# cat("- Average Item Stability:", 
#     round(rangel.dimstab$dimension.stability$average.item.stability, 3), "\n")
# 
# # Plot item stability
# stability_plot <- rangel.dimstab$item.stability$plot +
#   ggplot2::scale_color_brewer(palette = "Set3") 
# 
# print(stability_plot)
# 
# # Get stable items (stability > 0.5)
# test <- rangel.dimstab$item.stability
# stable_items <- names(test$item.stability$empirical.dimensions[
#   test[["item.stability"]][["empirical.dimensions"]] > .8])
# 
# cat("\n- Number of stable items (>7):", length(stable_items), "\n")
# cat("- Proportion of stable items:", 
#     round(length(stable_items)/ncol(rangel_for_network_final), 3), "\n")

# boot_ega_resultsV2 <- bootEGA(
#   data = rangel_for_network_final[,stable_items],
#   iter = 100,
#   typicalStructure = TRUE,
#   model = "glasso",
#   algorithm = "walktrap",
#   type = "parametric",
#   ncores = detectCores() - 1
# )


# Helper function for network statistics -------------------------------------
calculate_net_stats <- function(g) {
  # Calculate various network measures for a graph
  G <- g
  E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
  path_lengths <- distances(G)
  diag(path_lengths) <- NA # path length to oneself is zero
  
  adj_temp <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")
  
  # Calculate a range of metrics on the graph
  net_degree <- data.frame(
    degree = degree(g, normalized = TRUE),
    strength = strength(g),
    eigen = igraph::eigen_centrality(G)$vector,
    weighted_transitivity = transitivity(g, type = "weighted"),
    closeness = igraph::closeness.estimate(G, normalized = TRUE, cutoff = -1),
    betweenness = betweenness(G, normalized = TRUE)
  ) %>% tibble::rownames_to_column("Name")
  
  net_degree$product_type <- V(g)$product_type
  
  return(net_degree)
}



library(igraph)

# ============================================================================
# SECTION 1: CREATE NETWORK FROM EGA RESULTS
# ============================================================================

# Extract the network from EGA results
network_matrix <- ega_results$network
colnames(network_matrix) <- names(rangel_for_network_final)
rownames(network_matrix) <- names(rangel_for_network_final)

# Create igraph object
g <- igraph::graph_from_adjacency_matrix(
  network_matrix, 
  mode = "undirected", 
  weighted = TRUE, 
  diag = FALSE
)

# Set vertex names
V(g)$name <- names(rangel_for_network_final)

# ============================================================================
# SECTION 2: COMPUTE CENTRALITY MEASURES
# ============================================================================

# Use your existing function to calculate network stats
net_degree <- calculate_net_stats(g)

# Add PCA scores if you want them (from your existing pipeline)
dat_pca <- net_degree[,c("degree","strength","eigen","weighted_transitivity","closeness","betweenness")]
rownames(dat_pca) <- net_degree$Name
pca_res <- prcomp(dat_pca, center = TRUE, scale. = TRUE)

print(pca_res)
summary(pca_res)
library("factoextra")
scree_p <- fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 70))+
  theme_classic()+
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )+labs(x = "PC")
pc_plt <- fviz_pca_var(pca_res, col.var = "black")+
  theme_classic()+
  theme(
    axis.text = element_text(face = "bold"),
    axis.text.x = element_text(face="bold", size=14),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  ) + labs(x = "PC1", y = "PC2", title = "")
# fviz_pca_var(pca_res, col.var = "contrib",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_contrib(pca_res, choice = "var", axes = 1, ylim = c(0, 40))+
  theme_classic()+
  theme(
  axis.text = element_text(face = "bold"),
  axis.text.x = element_text(face="bold", size=14),
  text = element_text(size = 15),
  axis.title = element_text(face = "bold")
)+
  scale_x_discrete(labels=c("weighted_transitivity" = "transitivity"))+
  labs(x = "centrality measure", title = "PC1")
fviz_contrib(pca_res, choice = "var", axes = 2, ylim = c(0, 40))+
  theme_classic()+
  theme(
    axis.text = element_text(face = "bold"),
    axis.text.x = element_text(face="bold", size=14),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )+
  scale_x_discrete(labels=c("weighted_transitivity" = "transitivity"))+
  labs(x = "centrality measure", title = "PC2")

print(summary(pca_res))
# net_degree$PCA1 <-  pca_res$x[,1] * -1 #change the scale w/ linear transformation
net_degree$PCA1 <-  pca_res$x[,1] * -1
net_degree$PCA2 <- pca_res$x[,2] * -1
net_degree$PCA3 <- pca_res$x[,3]
net_degree$PCA4 <-  pca_res$x[,4]
net_degree$PCA5 <- pca_res$x[,5]
net_degree$PCA6 <- pca_res$x[,6]

cor_p <- net_degree %>% 
  dplyr::select(degree,strength,eigen,weighted_transitivity,closeness,betweenness,PCA1, PCA2, PCA3, PCA4, PCA5, PCA6) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE)+
  theme_classic()+
  labs(x = "", y = "") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(cor_p)


net_degree$PCA1 <- pca_res$x[,1]
net_degree$PCA2 <- pca_res$x[,2] 
net_degree$PCA3 <- pca_res$x[,3]

# Display the network measures
cat("Network measures computed for", nrow(net_degree), "items\n")
print(head(net_degree))

# Network Visualization Preparation ----

A <- boot_ega_results[["typicalGraph"]][["graph"]]  # or ega_res2 if using stable items
dimattributes <- boot_ega_results[["typicalGraph"]][["wc"]]
g <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
V(g)$product_type <- dimattributes

# Calculate network statistics
G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
net_degree <- calculate_net_stats(g)

# Create layout
l <- layout_with_graphopt(g)
# l <- layout_nicely(G)
# l <- layout.spring(G)

# Add colors to network based on number of clusters
# ADJUST THE NUMBER OF CASES BELOW BASED ON YOUR ACTUAL NUMBER OF CLUSTERS
net_degree <- net_degree %>%
  mutate(colors = case_when(
    product_type == 1 ~ "#E64B35FF",
    product_type == 2 ~ "#4DBBD5FF",
    product_type == 3 ~ "#00A087FF",
    product_type == 4 ~ "#3C5488FF",
    product_type == 5 ~ "#F39B7FFF",
    product_type == 6 ~ "#8491B4FF",
    product_type == 7 ~ "#91D1C2FF",
    product_type == 8 ~ "#DC0000FF",
    product_type == 9 ~ "#7E6148FF",
    product_type == 10 ~ "#B09C85FF"
  ))

V(g)$color <- net_degree$colors
E(g)$color[E(g)$weight > 0] <- "forestgreen"
E(g)$color[E(g)$weight < 0] <- "red2"

# Plot Network Visualizations ----

# Plot 1: Without labels (showing vertices)
plot(g,
     layout = l,
     margin = .0,
     vertex.label = NA,
     vertex.label.color = "black",
     label.font = 2,
     vertex.frame.color = adjustcolor(net_degree$colors, alpha.f = .1),
     vertex.size = 9,
     vertex.label.family = "Times",
     edge.width = E(g)$weight * 4.7)

# ADJUST LEGEND LABELS BASED ON YOUR ACTUAL CLUSTER NAMES
legend(x = 1.3,
       y = .6,
       c("Chocolate", "Savory", "Fruity", "Variety Sweet", "Hard Candies"),
       pch = 21,
       pt.bg = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"),
       pt.cex = 4,
       cex = 2,
       bty = "n",
       ncol = 1)

# Plot 2: With labels (text only, no vertices)
plot(g,
     layout = l,
     vertex.shape = "none",
     vertex.label.cex = .9,
     vertex.label = V(g)$name,
     vertex.label.font = 2,
     vertex.label.color = net_degree$colors,
     vertex.size = NULL,
     vertex.label.family = "Times",
     edge.width = E(g)$weight)

# ADJUST LEGEND LABELS TO MATCH ABOVE
legend(x = 1.3,
       y = .6,
       c("Chocolate", "Savory", "Fruity", "Variety Sweet", "Hard Candies"),
       pch = 21,
       pt.bg = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"),
       pt.cex = 2,
       cex = .8,
       bty = "n",
       ncol = 1)

# ============================================================================
# SECTION 3: ADD MEASURES TO CHOICE DATA
# ============================================================================

# Safe lookup function that returns NA if no match
safe_lookup <- function(lookup_name, reference_df, value_col) {
  match_idx <- which(reference_df$Name == lookup_name)
  if(length(match_idx) == 1) {
    return(reference_df[[value_col]][match_idx])
  } else {
    return(NA_real_)
  }
}

# Method 1: Using safe lookup function
twofoodchoicedata_enhanced <- twofoodchoicedata %>%
  rowwise() %>%
  mutate(
    # Left item measures
    left_degree = safe_lookup(FoodLeftName, net_degree, "degree"),
    left_strength = safe_lookup(FoodLeftName, net_degree, "strength"),
    left_eigen = safe_lookup(FoodLeftName, net_degree, "eigen"),
    left_transitivity = safe_lookup(FoodLeftName, net_degree, "weighted_transitivity"),
    left_closeness = safe_lookup(FoodLeftName, net_degree, "closeness"),
    left_betweenness = safe_lookup(FoodLeftName, net_degree, "betweenness"),
    left_pca1 = safe_lookup(FoodLeftName, net_degree, "PCA1"),
    left_pca2 = safe_lookup(FoodLeftName, net_degree, "PCA2"),
    
    # Right item measures  
    right_degree = safe_lookup(FoodRightName, net_degree, "degree"),
    right_strength = safe_lookup(FoodRightName, net_degree, "strength"),
    right_eigen = safe_lookup(FoodRightName, net_degree, "eigen"),
    right_transitivity = safe_lookup(FoodRightName, net_degree, "weighted_transitivity"),
    right_closeness = safe_lookup(FoodRightName, net_degree, "closeness"),
    right_betweenness = safe_lookup(FoodRightName, net_degree, "betweenness"),
    right_pca1 = safe_lookup(FoodRightName, net_degree, "PCA1"),
    right_pca2 = safe_lookup(FoodRightName, net_degree, "PCA2")
  ) %>%
  ungroup()

# Alternative Method 2: Using joins (often more efficient)
# Prepare network data for joining
net_left <- net_degree %>%
  select(Name, degree, strength, eigen, weighted_transitivity, closeness, betweenness, PCA1, PCA2) %>%
  rename_with(~paste0("left_", .), -Name) %>%
  rename(FoodLeftName = Name)

net_right <- net_degree %>%
  select(Name, degree, strength, eigen, weighted_transitivity, closeness, betweenness, PCA1, PCA2) %>%
  rename_with(~paste0("right_", .), -Name) %>%
  rename(FoodRightName = Name)

# Join method
twofoodchoicedata_enhanced_v2 <- twofoodchoicedata %>%
  left_join(net_left, by = "FoodLeftName") %>%
  left_join(net_right, by = "FoodRightName")

# Check coverage
missing_left <- sum(is.na(twofoodchoicedata_enhanced$left_degree))
missing_right <- sum(is.na(twofoodchoicedata_enhanced$right_degree))
total_rows <- nrow(twofoodchoicedata_enhanced)

cat("Coverage check:\n")
cat("Total choice trials:", total_rows, "\n")
cat("Missing left network measures:", missing_left, "(", round(missing_left/total_rows*100, 1), "%)\n")
cat("Missing right network measures:", missing_right, "(", round(missing_right/total_rows*100, 1), "%)\n")

# Show unmatched items
unmatched_left <- setdiff(unique(twofoodchoicedata$FoodLeftName), net_degree$Name)

if(length(unmatched_left) > 0) {
  cat("\nLeft items not in network data:\n")
  print(unmatched_left)
}


test_fdf <- na.omit(twofoodchoicedata_enhanced)
twofoodchoicedata_enhanced[is.na(twofoodchoicedata_enhanced$left_degree),]


standardized = TRUE
library(broom.mixed)
library(dplyr)
library(lme4)
library(lmerTest)

# Define all network measures to test
network_measures <- c("degree", "strength", "eigen", "transitivity", 
                      "closeness", "betweenness", "pca1", "pca2")

# Initialize results storage
choice_results <- list()
rt_results <- list()

# Loop through each network measure
for(measure in network_measures) {
  cat("\n=== Testing network measure:", measure, "===\n")
  
  # Prepare data with current network measure
  for_model_temp <- test_fdf %>%
    group_by(SubjectNumber) %>%
    mutate(
      zleft_rating = scale(ValueLeft, center = standardized, scale = standardized),
      zright_rating = scale(ValueRight, center = standardized, scale = standardized),
      zleft_net = scale(get(paste0("left_", measure)), center = standardized, scale = standardized),
      zright_net = scale(get(paste0("right_", measure)), center = standardized, scale = standardized),
      nd = scale(abs(get(paste0("left_", measure)) - get(paste0("right_", measure))), center = standardized, scale = standardized),
      vd = scale(abs(ValueLeft - ValueRight), center = standardized, scale = standardized),
      ov = scale(ValueLeft + ValueRight, center = standardized, scale = standardized)
    ) %>%
    ungroup() %>%
    select(SubjectNumber, choice, RT, zleft_rating, zright_rating, zleft_net, zright_net, vd, nd, ov) %>%
    filter(!is.na(zleft_net) & !is.na(zright_net))  # Remove rows with missing network data
  
  # Skip if insufficient data
  if(nrow(for_model_temp) < 100) {
    cat("Skipping", measure, "- insufficient data (n =", nrow(for_model_temp), ")\n")
    next
  }
  
  # CHOICE MODEL
  tryCatch({
    choice_model <- glmer(
      choice ~ (zleft_rating * zleft_net + zright_rating * zright_net) +
        (zleft_rating * zleft_net + zright_rating * zright_net | SubjectNumber),
      data = for_model_temp,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7))
    )
    
    # Extract key coefficients
    choice_coefs <- tidy(choice_model, effects = "fixed") %>%
      filter(term %in% c("zleft_rating", "zright_rating", "zleft_net", "zright_net",
                         "zleft_rating:zleft_net", "zright_rating:zright_net")) %>%
      mutate(network_measure = measure, model_type = "choice")
    
    choice_results[[measure]] <- choice_coefs
    
    cat("Choice model for", measure, "- converged successfully\n")
    
  }, error = function(e) {
    cat("Choice model for", measure, "failed:", e$message, "\n")
    choice_results[[measure]] <- data.frame(
      term = "failed_to_converge",
      estimate = NA, std.error = NA, statistic = NA, p.value = NA,
      network_measure = measure, model_type = "choice"
    )
  })
  
  # RT MODEL  
  tryCatch({
    rt_model <- lmer(
      log(RT) ~ vd + ov + nd + (vd + ov + nd | SubjectNumber),
      data = for_model_temp,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
    
    # Extract key coefficients
    rt_coefs <- tidy(rt_model, effects = "fixed") %>%
      filter(term %in% c("vd", "ov", "nd")) %>%
      mutate(network_measure = measure, model_type = "rt")
    
    rt_results[[measure]] <- rt_coefs
    
    cat("RT model for", measure, "- converged successfully\n")
    
  }, error = function(e) {
    cat("RT model for", measure, "failed:", e$message, "\n")
    rt_results[[measure]] <- data.frame(
      term = "failed_to_converge", 
      estimate = NA, std.error = NA, statistic = NA, p.value = NA,
      network_measure = measure, model_type = "rt"
    )
  })
}

# Combine all results
all_choice_results <- bind_rows(choice_results)
all_rt_results <- bind_rows(rt_results)
all_results <- bind_rows(all_choice_results, all_rt_results)
options(scipen = 999)

# Create summary tables
cat("\n=== CHOICE MODEL RESULTS ===\n")
choice_summary <- all_choice_results %>%
  filter(term != "failed_to_converge") %>%
  select(network_measure, term, estimate, std.error, p.value) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = round(p.value, 4),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**", 
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  arrange(network_measure, term)

print(choice_summary)

choice_summary %>% filter(term == "zleft_net" | term == "zright_net"|
                          term =="zright_rating:zright_net"|
                          term =="zleft_rating:zleft_net")


cat("\n=== RT MODEL RESULTS ===\n")
rt_summary <- all_rt_results %>%
  filter(term != "failed_to_converge") %>%
  select(network_measure, term, estimate, std.error, p.value) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3), 
    p.value = round(p.value, 4),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*", 
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  arrange(network_measure, term)

print(rt_summary)

# rt_summary %>% filter(term == "nd") %>%
  
# Create effect size comparison
cat("\n=== EFFECT SIZE COMPARISON ===\n")
effect_comparison <- all_results %>%
  filter(term != "failed_to_converge" & !is.na(estimate)) %>%
  group_by(model_type, term) %>%
  summarise(
    max_effect = max(abs(estimate), na.rm = TRUE),
    best_measure = network_measure[which.max(abs(estimate))],
    min_p = min(p.value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(model_type, desc(max_effect))

print(effect_comparison)

# Save results
# write_csv(all_results, "network_measures_systematic_test.csv")
library(brms)

for_model <- test_fdf %>%
  group_by(SubjectNumber) %>%
  mutate(
    zleft_rating = scale(ValueLeft, center = standardized, scale = standardized),
    zright_rating = scale(ValueRight, center = standardized, scale = standardized),
    zleft_net1 = scale(get(paste0("left_", "pca1")), center = standardized, scale = standardized),
    zright_net1 = scale(get(paste0("right_", "pca1")), center = standardized, scale = standardized),
    zleft_net2 = scale(get(paste0("left_", "pca2")), center = standardized, scale = standardized),
    zright_net2 = scale(get(paste0("right_", "pca2")), center = standardized, scale = standardized),
    nd1 = scale(abs(get(paste0("left_", "pca1")) - get(paste0("right_", "pca1"))), center = standardized, scale = standardized),
    nd2 = scale(abs(get(paste0("left_", "pca2")) - get(paste0("right_", "pca2"))), center = standardized, scale = standardized),
    vd = scale(abs(ValueLeft - ValueRight), center = standardized, scale = standardized),
    ov = scale(ValueLeft + ValueRight, center = standardized, scale = standardized)
  ) %>%
  ungroup() %>%
  select(SubjectNumber, choice, RT, zleft_rating, zright_rating, zleft_net1, zleft_net2, zright_net1, zright_net2, vd, nd1, nd2, ov)

models_choice<- brm(choice ~ zleft_rating*(zleft_net1 + zleft_net2) + zright_rating*(zright_net1 + zright_net2) +
                        (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 +  zleft_net2 + zright_net2 | SubjectNumber), 
                      data = for_model, family = "bernoulli", iter = 10000, 
                      chains = 4, cores = 4, backend = "cmdstanr",
                      file = here::here("fits", "PCA_Smith_Krajbich_2018_choice_data_fit_choice"))

models_rt <- brm(log(RT) ~ vd + ov + nd1 + nd2 +
                    (vd + ov + nd1 + nd2 | SubjectNumber), 
                  data = for_model, iter = 10000, 
                  chains = 4, cores = 4, backend = "cmdstanr",
                  file = here::here("fits", "PCA_Smith_Krajbich_2018_choice_data_fit_rt"))



