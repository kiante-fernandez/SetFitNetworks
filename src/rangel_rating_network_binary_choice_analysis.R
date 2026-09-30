# rangel_rating_network_binary_choice_analysis.R - Rating Study 3 network and Binary-Choice Study 3.
# Ratings: the items rated in Smith & Krajbich (2018; smikrab2018 in the Liking Initiative), pooled across the Liking
# Initiative datasets, Yoo et al. (2024) and Smith et al. (2025); min-max normalized within dataset, items missing for
# >= 99% of raters dropped, remaining missing ratings mean-imputed.
# Rating Study 3 network (SI; plotted in Fig. 1a by figures.R): bootstrap EGA, saved to data/rangel_rating_network_graph.RData.
# Binary-Choice Study 3: item centralities from an EGA network on the same ratings, PC1/PC2 from a PCA of those
# centralities (net1/nd1 = PC1, net2/nd2 = PC2); choice and RT models as in the other binary-choice studies.

library(here)
library(tidyverse)
library(EGAnet)
library(igraph)
library(parallel)
library(brms)
library(cmdstanr)

# Binary-Choice Study 3 choices, with item names from the stimulus file names
load(here("data", "smith_krajbich_2018", "ACADchoiceandeyedata.RData"))
stimuli <- read_csv(here("data", "smith_krajbich_2018", "food_stimuli.csv"), show_col_types = FALSE)
item_names <- str_to_lower(str_remove(str_remove(stimuli$filename, "\\.[^.]+$"), "img_"))
twofoodchoicedata <- twofoodchoicedata %>%
  mutate(FoodLeftName = item_names[FoodLeft], FoodRightName = item_names[FoodRight], choice = if_else(LeftRight == 1, 1, 0))

# Ratings ----------------------------------------------------------------------------------------------------------
li <- function(f) read_csv(here("data", "liking_initiative", f), show_col_types = FALSE)
clean_name <- function(x) str_to_lower(x) %>% str_replace_all("'", "") %>% str_replace_all("\\s+", "") %>% str_replace_all("-", "")
ratings <- bind_rows(
  li("rangel_final_database.csv"),
  li("EA_fMRI_value_rating.csv") %>%
    transmute(dataset_subjectid = paste0("yoo2024_", subj_id), item_name = clean_name(food_name), rating, rating_scale = "0_to_10"),
  li("Steph2025_ratings_combined_both_studies.csv") %>%
    transmute(dataset_subjectid = paste0("smith2025_s", study, "_", subject), item_name = clean_name(image), rating,
              rating_scale = "0.01_to_4"))
target_items <- unique(ratings$item_name[str_detect(ratings$dataset_subjectid, "smikrab2018")])

rangel_wide <- ratings %>%
  filter(item_name %in% target_items) %>%
  group_by(dataset_subjectid, item_name) %>%
  summarise(rating = mean(rating, na.rm = TRUE), .groups = "drop") %>%
  mutate(dataset = if_else(str_detect(dataset_subjectid, "^hasdes_"), "hasdes",
                           str_replace(dataset_subjectid, "_[0-9]+(?:\\.[0-9]+)?(?:_.*)?$", ""))) %>%
  group_by(dataset) %>%
  mutate(rating_min = min(rating, na.rm = TRUE), rating_range = max(rating, na.rm = TRUE) - rating_min,
         rating = if_else(rating_range == 0, 0.5, (rating - rating_min) / rating_range)) %>%
  ungroup() %>%
  select(dataset_subjectid, item_name, rating) %>%
  pivot_wider(names_from = item_name, values_from = rating)

rangel_for_network_final <- rangel_wide %>%
  select(-dataset_subjectid) %>%
  select(where(~ mean(is.na(.)) * 100 < 99)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Local version: returns the raw centrality measures (and communities, when present) without the PCA in utils.R
calculate_net_stats <- function(g) {
  G <- g
  E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
  net_degree <- data.frame(
    degree = degree(g, normalized = TRUE),
    strength = strength(g),
    eigen = igraph::eigen_centrality(G)$vector,
    weighted_transitivity = transitivity(g, type = "weighted"),
    closeness = igraph::closeness.estimate(G, normalized = TRUE, cutoff = -1),
    betweenness = betweenness(G, normalized = TRUE)
  ) %>% tibble::rownames_to_column("Name")
  net_degree$product_type <- V(g)$product_type
  net_degree
}

# Rating Study 3 network (bootstrap EGA) -----------------------------------------------------------------------------
if (!file.exists(here("data", "rangel_rating_network_graph.RData"))) {
  set.seed(2025)
  boot_ega_results <- bootEGA(data = rangel_for_network_final, iter = 1000, typicalStructure = TRUE, model = "glasso",
                              algorithm = "walktrap", type = "parametric", ncores = detectCores() - 1)
  g <- graph_from_adjacency_matrix(boot_ega_results[["typicalGraph"]][["graph"]], "undirected", weighted = TRUE)
  V(g)$product_type <- boot_ega_results[["typicalGraph"]][["wc"]]
  save(boot_ega_results, g, file = here("data", "rangel_rating_network_graph.RData"))
} else {
  load(here("data", "rangel_rating_network_graph.RData"))
}

# Binary-Choice Study 3 ------------------------------------------------------------------------------------------------
ega <- EGA(rangel_for_network_final, model = "glasso", algorithm = "walktrap", plot.EGA = FALSE)
network <- ega$network
dimnames(network) <- list(names(rangel_for_network_final), names(rangel_for_network_final))
net <- calculate_net_stats(graph_from_adjacency_matrix(network, mode = "undirected", weighted = TRUE, diag = FALSE))
pcs <- prcomp(net[, c("degree", "strength", "eigen", "weighted_transitivity", "closeness", "betweenness")],
              center = TRUE, scale. = TRUE)$x
pc <- function(item, k) pcs[match(item, net$Name), k]

for_model <- twofoodchoicedata %>%
  mutate(left_pca1 = pc(FoodLeftName, 1), right_pca1 = pc(FoodRightName, 1),
         left_pca2 = pc(FoodLeftName, 2), right_pca2 = pc(FoodRightName, 2)) %>%
  na.omit() %>%  # trials with an item outside the network
  group_by(SubjectNumber) %>%
  mutate(zleft_rating = scale(ValueLeft), zright_rating = scale(ValueRight),
         zleft_net1 = scale(left_pca1), zright_net1 = scale(right_pca1), zleft_net2 = scale(left_pca2), zright_net2 = scale(right_pca2),
         nd1 = scale(abs(left_pca1 - right_pca1)), nd2 = scale(abs(left_pca2 - right_pca2)),
         vd = scale(abs(ValueLeft - ValueRight)), ov = scale(ValueLeft + ValueRight)) %>%
  ungroup()

models_choice <- brm(choice ~ zleft_rating * (zleft_net1 + zleft_net2) + zright_rating * (zright_net1 + zright_net2) +
                       (1 + zleft_rating + zright_rating + zleft_net1 + zright_net1 + zleft_net2 + zright_net2 | SubjectNumber),
                     data = for_model, family = "bernoulli", iter = 10000, chains = 4, cores = 4, backend = "cmdstanr",
                     file = here("fits", "PCA_Smith_Krajbich_2018_choice_data_fit_choice"))
models_rt <- brm(log(RT) ~ vd + ov + nd1 + nd2 + (vd + ov + nd1 + nd2 | SubjectNumber),
                 data = for_model, iter = 10000, chains = 4, cores = 4, backend = "cmdstanr",
                 file = here("fits", "PCA_Smith_Krajbich_2018_choice_data_fit_rt"))
