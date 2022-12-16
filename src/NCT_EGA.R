# NCT_EGA - conducts a bootstrap Exploratory Graph Analysis function for the
# estimator for a permutation test using network comparaisons
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
# 2022/12/14      Kianté  Fernandez                       wrote code

# Libraries
library(here)
library(readxl)
library(igraph)
library(EGAnet)
library(NetworkComparisonTest)
library(SemNeT)

# load the names of the foods
FoodNames <- readxl::read_excel(here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))
# load the data
lee_2021_rating1 <- readr::read_csv(here("data", "lee_2021_rating1.csv"), col_names =  FoodNames$Name, show_col_types = F)
lee_2021_rating2 <- readr::read_csv(here("data", "lee_2021_rating2.csv"), col_names = FoodNames$Name, show_col_types = F)

# define the EGA as the estimator of choice
ega_estimator <- function(data, iter = 100, ...) {
  # number of observations
  n <- nrow(data)
  # ega_res <- EGAnet::EGA(data, n = n, corr =  "pearson", model = "glasso", algorithm = "walktrap", plot.EGA = FALSE)
  ega_res <- suppressMessages(
    suppressWarnings(
      EGAnet::bootEGA(data,
        iter = iter,
        n = n,
        model = "glasso",
        algorithm = "walktrap",
        ncores = 10, typicalStructure = T,
        plot.typicalStructure = FALSE,
        progress = FALSE
      )
    )
  )
  return(ega_res$typicalGraph$graph)
}

NCT_res <- NCT(lee_2021_rating1, lee_2021_rating2, it = 1000, weighted = T, estimator = ega_estimator,paired = TRUE,
               test.edges = TRUE,
               test.centrality = TRUE,
               centrality = "strength",
               verbose = FALSE)


# Plot results of the network structure invariance test (not reliable with only 10 permutations!):
plot(NCT_res, what="network")
# Plot results of global strength invariance test (not reliable with only 10 permutations!):
plot(NCT_res, what="strength")
# Plot results of the edge invariance test (not reliable with only 10 permutations!):
# Note that two distributions are plotted
plot(NCT_res, what="edge")

plot(NCT_res, what="centrality")
#save the results (they are on the M1)
save(NCT_res, file = here("data", "lee_NCT.RData"))

