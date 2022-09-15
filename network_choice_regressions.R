# network_choice_regressions.R - looks at the effect of network stats on choice
#
# Copyright (C) 2022 Kianté Fernandez, <kiantefernan@gmail.com>
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
# 11/09/22      Kianté  Fernandez                       wrote code

library(here)
library(igraph)
library(assortnet)
# load the data
lee_2021_rating1 <- readr::read_csv(here("data", "lee_2021_rating1.csv"), col_names = FALSE, show_col_types = F)
# load the names of the foods
FoodNames <- readxl::read_excel(here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))
# create cleaned names
names(lee_2021_rating1) <- FoodNames$Name
#
item_means <- colMeans(lee_2021_rating1)

network_stats <- c("assortment", "edge_density", "weighted_clustering_coefficient")

for (network_stat_idx in seq_len(length(network_stats))) {
  load(file = here::here("data", paste0(network_stats[[network_stat_idx]], "_", 30, "_", 6, ".RData")))

  statistics_res <- matrix(, ncol(res), 11)

    for (subgraphs_idx in seq_len(ncol(res))) {
    # get bunch of stats
    temp_stats <- psych::describe(item_means[names(item_means) %in% res[[subgraphs_idx]]])[3:12]
    statistics_res[subgraphs_idx, 2:11] <- as.numeric(temp_stats)
    
    # calculate network stats
    if (network_stats[[network_stat_idx]] == "assortment") {
      gt <- subgraphs[[subgraphs_idx]]
      adj_temp <- igraph::as_adjacency_matrix(gt, sparse = F, attr = "weight")
      statistics_res[[subgraphs_idx, 1]] <- assortment.discrete(adj_temp, V(gt)$snack_type, weighted = TRUE, SE = F)$r
    } else if (network_stats[[network_stat_idx]] == "edge_density") {
      statistics_res[[subgraphs_idx, 1]] <- igraph::edge_density(subgraphs[[subgraphs_idx]])
    } else if(network_stats[[network_stat_idx]] == "weighted_clustering_coefficient") {
      statistics_res[[subgraphs_idx, 1]] <- clustAnalytics::weighted_clustering_coefficient(subgraphs[[subgraphs_idx]], upper_bound = 1)
    }
  }
  colnames(statistics_res) <- c(network_stats[[network_stat_idx]], names(temp_stats))

  cor_res <- correlation::correlation(data.frame(statistics_res), p_adjust = "none")

  print(cor_res[cor_res$Parameter1 == network_stats[[network_stat_idx]], ])
}
