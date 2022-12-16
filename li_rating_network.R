# li_rating_network.R - conducts a bootstrap Exploratory Graph Analysis for 
#
# Li, X., Bainbridge, W., & Bakkour, A. (2022). 
# Memorable but not chosen: No effect of memorability on value-based decisions.
# PsyArXiv. https://doi.org/10.31234/osf.io/xqhk8
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
# 2022/12/06      Kianté  Fernandez                       wrote code

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

#load the dictionary for image files and corresponding words
wordlist = read.csv('/Users/kiantefernandez/OneDrive - The Ohio State University/food_standardization/Li_etal.2022/data/image_word.csv')

#Load memorability data
dat = read.csv("/Users/kiantefernandez/OneDrive - The Ohio State University/food_standardization/Li_etal.2022/data/Exp1A.csv")

#load and apply function for calculating memorability scores
source("/Users/kiantefernandez/OneDrive - The Ohio State University/food_standardization/Li_etal.2022/memorability_analysis.R", local = knitr::knit_global())

#use function calculate_mem() to calculate image memorability
img.values = calculate_mem(dat)
summary(img.values)

#combine it with image/word indexes
mem = merge(img.values, wordlist, by = "image")

li_2022_rating <- read_csv("/Users/kiantefernandez/OneDrive - The Ohio State University/food_standardization/Li_etal.2022/data/Exp1B.csv")

#select rating trials to get subjective values of images from each participant
rating = li_2022_rating %>% filter(ttype == 'rating_task') %>% 
  dplyr::group_by(ID) %>% mutate(z = scale(as.numeric(response)))

values = rating %>% dplyr::group_by(image) %>% 
  dplyr::summarise(value = mean(as.numeric(response)))%>% 
  mutate(item = substring(image, 54)) %>%
  dplyr::rename(url = image) %>%
  dplyr::rename(image = item) %>% 
  left_join(mem)


rating_matrix <- rating %>% 
  select(response, image) %>% 
  mutate(response = as.numeric(response),
         image = substring(image, 54)) %>% 
  pivot_wider(names_from = image, values_from = response) %>% 
  ungroup() %>% select(-ID)
  
  
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

test_net <-EGAnet::EGA(rating_matrix, n = 44, model = "TMFG", algorithm = "walktrap", corr = "spearman")


A <- test_net[["network"]]
# get clusters
dimattributes <- test_net[["wc"]]
# create igraph object
g_2 <- graph_from_adjacency_matrix(A, "undirected", weighted = TRUE, diag = F)
# add decorate attributes
V(g_2)$snack_type <- dimattributes
E(g_2)$color[E(g_2)$weight > 0] <- "forestgreen"
E(g_2)$color[E(g_2)$weight < 0] <- "red2"
l <- layout_with_lgl(g_2)
V(g_2)$color <- V(g_2)$snack_type
V(g_2)$mem <- round(values$Memorability * 7) 
V(g_2)$name <- stringr::str_remove_all(V(g_2)$name, ".jpg")
plot(g_2,
     layout = l,
     margin = .0,
     # vertex.label = V(g_2)$name,
     # vertex.label = NA,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.label.dist = 1,
     vertex.size = V(g_2)$mem,
     vertex.label.family = "Times",
     main = "Li et. al: N = 44",
     edge.width = abs(E(g_2)$weight) * 3,
)

centralitys <- centrality_auto(g_2)$node.centrality
centralitys$eign <- eigen_centrality(g_2)$vector

centralitys$mem <- values$Memorability
centralitys$hit <- values$Hit.count

# correlation::correlation(centralitys)

# correlogram
centralitys %>%
  ggstatsplot::ggcorrmat(
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

