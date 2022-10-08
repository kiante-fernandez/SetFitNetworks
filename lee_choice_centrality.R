# lee_choice_centrality.R - conducts an analysis looking at the effect of
# micro-scale measures of network organization on choice
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
# 22/09/22      Kianté  Fernandez                       wrote code

# Libraries
library(here)
library(readr)
library(dplyr)
library(igraph)
library(qgraph)
library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4
library(lmerTest) # Tests in Linear Mixed Effects Models

# load all the images to calculate the value for a group of foods
food_folder <- here::here("data", "snackitemnames_nicholas", "Lee_Holyoak_2021_images")
FoodNames <- readxl::read_excel(here::here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))

temp <- list.files(path = food_folder, pattern = "*.jpg", full.names = T)
foods_in_image <- stringr::str_extract(temp, "item\\d+")
foods_in_image <- stringr::str_extract(foods_in_image, "\\d+")
# get row idx for each of the image numbers
foods_in_image <- tibble::rowid_to_column(data.frame(Image = as.numeric(foods_in_image)))
foods_in_image <- dplyr::left_join(FoodNames, foods_in_image, "Image")

lee_2021_choice <- read_csv(here("data", "lee_2021_exp2_5.csv"), col_names = FALSE)

colnames(lee_2021_choice) <- c("subjectid", "experimentid", "itemL", "itemR", "ratingL", "ratingR", "rDiff", "choice", "rt")
length(unique(lee_2021_choice$subjectid)) # you should have 267 subjects

for (foo in seq_len(nrow(lee_2021_choice))) {
  lee_2021_choice$itemL[[foo]] <- foods_in_image[foods_in_image$Image == lee_2021_choice$itemL[[foo]], ]$Name
  lee_2021_choice$itemR[[foo]] <- foods_in_image[foods_in_image$Image == lee_2021_choice$itemR[[foo]], ]$Name
}
# load graph
source("exploratory_graph_analysis.R")

g
V(g)
E(g)

l <- layout.circle(g)
E(g)$color[E(g)$weight > 0] <- "forestgreen"
E(g)$color[E(g)$weight < 0] <- "red2"

plot(g,
  layout = l,
  margin = .0,
  vertex.label = V(g)$name,
  vertex.label.color = "black",
  vertex.label.cex = .7,
  vertex.size = 0,
  vertex.label.family = "Times",
  edge.curved = .15,
  edge.width = abs(E(g)$weight) * 5,
  main = "How much would you like this as a daily snack?",
  # mark.groups =  V(g)$snack_type
)

net_degree <- data.frame(degree(g)) %>%
  tibble::rownames_to_column("Name") %>%
  left_join(foods_in_image, "Name")

net_degree <- data.frame(clustcoef_auto(g)) %>%
  tibble::rownames_to_column("Name") %>%
  left_join(net_degree, "Name")


lee_2021_choice$degreeL <- NA
lee_2021_choice$degreeR <- NA
lee_2021_choice$clusterL <- NA
lee_2021_choice$clusterR <- NA

for (foo in seq_len(nrow(lee_2021_choice))) {
  lee_2021_choice$degreeL[[foo]] <- as.numeric(net_degree$degree.g.[lee_2021_choice$itemL[[foo]] == net_degree$Name])
  lee_2021_choice$degreeR[[foo]] <- as.numeric(net_degree$degree.g.[lee_2021_choice$itemR[[foo]] == net_degree$Name])
  lee_2021_choice$clusterL[[foo]] <- as.numeric(net_degree$clustWS[lee_2021_choice$itemL[[foo]] == net_degree$Name])
  lee_2021_choice$clusterR[[foo]] <- as.numeric(net_degree$clustWS[lee_2021_choice$itemR[[foo]] == net_degree$Name])
}
# relative diff
lee_2021_choice$rDegreeDiff <- lee_2021_choice$degreeR - lee_2021_choice$degreeL
lee_2021_choice$rClustDiff <- lee_2021_choice$clusterR - lee_2021_choice$clusterL

summary(lm(log(rt) ~ rClustDiff + rDegreeDiff + rDiff, data = lee_2021_choice))
summary(glm(choice ~ rClustDiff + rDegreeDiff + rDiff, data = lee_2021_choice, family = "binomial"))
#
# ssummary(lmer(log(rt) ~ rDiff  + rClustDiff + (1 | subjectid), data = lee_2021_choice))
# summary(glmer(choice ~ rDiff  + rClustDiff + (1 | subjectid), data = lee_2021_choice, family = "binomial"))

# sub_test <- lee_2021_choice[lee_2021_choice$subjectid == 104,]
# cor.test(sub_test$rDiff, sub_test$rDegreeDiff)
# cor.test(sub_test$rDiff, sub_test$rClustDiff)
#
# summary(lm(log(rt) ~ rDiff +  rClustDiff + rDegreeDiff + rDiff, data = sub_test))
# summary(glm(choice ~ rDiff +  rClustDiff + rDegreeDiff, data = sub_test, family = "binomial"))
