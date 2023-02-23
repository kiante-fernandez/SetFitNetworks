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
# lee_2021_rating2 <- readr::read_csv(here("data", "lee_2021_rating2.csv"), col_names = FoodNames$Name, show_col_types = F)

# load the data
fernandez_2022_rating1 <- readr::read_csv(here("data","fernandez_2022_rating_exp1.csv"), col_names = T, show_col_types = F)
fernandez_2022_rating2 <- readr::read_csv(here("data","fernandez_2022_rating_exp2.csv"), col_names = T, show_col_types = F)
#take both sets of rating data from study one and two and combine them
fernandez_2022_rating_combineded <- rbind(fernandez_2022_rating1,fernandez_2022_rating2)


# define the EGA as the estimator of choice
ega_estimator <- function(data, iter = 500, ...) {
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

NCT_res <- NCT(lee_2021_rating1, fernandez_2022_rating_combineded, it = 200, weighted = T, estimator = ega_estimator,
               paired = FALSE,
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
# plot(NCT_res, what="edge")
plot(NCT_res, what="centrality")
#save the results (they are on the M1)

# save(NCT_res, file = here("data", "lee_NCT.RData"))
save(NCT_res, file = here("data", "fernandez_lee_NCT_2.RData"))

#we found that the networks are similar to one another...

#TODO compare the first rating dataset to the second in our study.
library(aricode)

source("exploratory_graph_analysis.R")
cl1 <- ega_res$typicalGraph$typical.dim.variables
cl1 <- ega_res$EGA$wc
cl1 <- ega_res$boot.wc

source('fernandez_rating_network.R') #load the EGA from the new rating data
cl2 <- ega_res$typicalGraph$typical.dim.variables
cl2 <- ega_res$EGA$wc
cl2 <- ega_res$boot.wc
library(purrr)

test <- list(cl1, cl2)

# map(test, function (x){aricode::NMI(x[[1]], x[[2]])})

aricode::NMI(cl1$dimension, cl2$dimension)
aricode::NMI(cl1, cl2)

temp_res <- vector(mode = "numeric", length = 1000)
for (foo in 1:1000){
  temp_res[[foo]] <- aricode::NMI(test[[1]][[foo]], test[[2]][[foo]])
  print(temp_res)
}
hist(temp_res)
confint(temp_res)
psych::describe(temp_res)
model <- lm(temp_res ~ 1)
confint(model, level=0.95)

