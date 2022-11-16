# gandhi_rating_network.R - conducts a bootstrap Exploratory Graph Analysis for 
#
# Gandhi, N., Zou, W., Meyer, C., Bhatia, S., & Walasek, L. (2022). 
# Computational Methods for Predicting and Understanding Food Judgment. 
# Psychological Science, 33(4), 579–594. 
# https://doi.org/10.1177/09567976211043426
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
# 07/11/22      Kianté  Fernandez                       wrote code


##NOTES: 
#> The issue with this data set is that is has alot of items, but few ratings
#> thousands of items infact, but only a little over 120 observations per item
#> so the problem is not solved here. The problem being that we do not have enough
#> observations within a dataset
#> 
# Libraries
library(igraph) # Network Analysis and Visualization
library(qgraph) # Graph Plotting Methods, Psychometric Data Visualization and Graphical Model Estimation
library(EGAnet)
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(SemNeT)
library(here)
library(bootnet)
library(patchwork)

# helper functions for working with lists
list.do <- function(.data, fun, ...) {
  do.call(what = fun, args = as.list(.data), ...)
}
list.cbind <- function(.data) {
  list.do(.data, "cbind")
}

gandhi_2022_rating <- read_csv("/Users/kiantefernandez/OneDrive - The Ohio State University/food_standardization/Gandhi_etal_2022/data/Main Analysis/Main Analysis Data/1A_Ratings.csv")
names(gandhi_2022_rating) <- c("subject", "food_name", "rating")
rating_matrix <- gandhi_2022_rating %>% 
  mutate(food_name = factor(food_name)) %>% 
  select(food_name,rating) %>% 
  pivot_wider(names_from = food_name, values_from = rating) %>% 
  map(unlist) %>% 
  map(as.numeric)

gandhi_2022_rating %>% 
  mutate(food_name = factor(food_name)) %>% 
  group_by(food_name) %>% 
  na.omit() %>% 
  summarise(n= n()) %>% View
  

rating_matrix <- list.cbind(rating_matrix)
cor_snack_food <- SemNeT::similarity(na.omit(rating_matrix), method = "cor")
m <- as.matrix(cor_snack_food)

mDim <- length(m[1, ]) # determine size of one dim of the matrix, which we assume is identical to the other dim.
# calulate the correlations
ggcorrplot::ggcorrplot(m[mDim:1, ],
                       colors = c("red", "white", "green"),
                       ggtheme = ggplot2::theme_classic,
                       outline.color = "white",
                       show.diag = T,
                       hc.order = F
) + ggplot2::theme(
  axis.text.x = element_text(size = 4),
  axis.text.y = element_text(size = 4),
  axis.ticks = element_blank()
) + theme(legend.position = "left")

cor_x1 <- cor(na.omit(rating_matrix))
cor_x1 <- matrix(Matrix::nearPD(cor_x1, corr=TRUE, maxit = 500 )$mat, ncol = 172)
cor_x1 <- (cor_x1 + t(cor_x1)) / 2 # make symmetric
colnames(cor_x1) <- colnames(rating_matrix)

test_net <-EGAnet::EGA(cor_x1, n = 134, model = "glasso", algorithm = "walktrap",corr = "pearson")

boot_test <- EGAnet::bootEGA(cor_x1,
                             plot.type = "qgraph",
                             iter = 100,
                             type = "resampling",
                             corr = "pearson",
                             n = 134,
                             model = "glasso",
                             algorithm = "walktrap",
                             ncores = 8, typicalStructure = T,
                             model.args = list(lambda.min.ratio = 0.1,
                                               nlambda = 300)) # gamma = 0.05

boot_test[["plot.typical.ega"]][["layers"]][[6]] <- NULL
boot_test$plot.typical.ega
boot_test$frequency
boot_test$typicalGraph$typical.dim.variables

A <- boot_test[["typicalGraph"]][["graph"]]
# get clusters
dimattributes <- boot_test[["typicalGraph"]][["wc"]]
# create igraph object
g_2 <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE)
# add decorate attributes
V(g_2)$snack_type <- dimattributes
value_sample_data <- tibble(items = V(g_2)$name) %>%
  left_join(boot_test$typicalGraph$typical.dim.variables,
            by = "items"
  )
E(g_2)$color[E(g_2)$weight > 0] <- "forestgreen"
E(g_2)$color[E(g_2)$weight < 0] <- "red2"
l <- layout_with_lgl(g_2)
V(g_2)$color <- value_sample_data$dimension
plot(g_2,
     layout = l,
     margin = .0,
     # vertex.label = V(g_2)$name,
     vertex.label = NA,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.label.dist = 1,
     vertex.size = 6,
     vertex.label.family = "Times",
     main = "Gandhi et. al: N = 134",
     edge.width = abs(E(g_2)$weight) * 4,
)

