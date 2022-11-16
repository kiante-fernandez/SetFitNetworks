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
lee_2021_choice$correct <- as.numeric((lee_2021_choice$ratingR > lee_2021_choice$ratingL & lee_2021_choice$choice == 1) | (lee_2021_choice$ratingR < lee_2021_choice$ratingL & lee_2021_choice$choice == 0))


# load graph
source("exploratory_graph_analysis.R")

G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
path_lengths <- distances(G)
adj_temp <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")

#calculate a bunch of measures
net_degree <- data.frame(degree= degree(g), 
                         strength = strength(g),
                         eigen = igraph::eigen_centrality(G)$vector,
                         page_rank = page_rank(g)$vector, #weighted
                         weighted_transitivity = transitivity(g, type = "weighted"),
                         closeness = NetworkToolbox::closeness(adj_temp, weighted = TRUE),
                         closeness2 = closeness(G), 
                         betweenness = betweenness(G),
                         participation = NetworkToolbox::participation(adj_temp, comm = V(g)$snack_type)$overall,
                         clustWS = clustcoef_auto(g),
                         diversity = NetworkToolbox::diversity(adj_temp, comm = V(g)$snack_type)$positive) %>%
  tibble::rownames_to_column("Name") %>%
  left_join(foods_in_image, "Name")

lee_2021_choice$degreeL <- NA
lee_2021_choice$degreeR <- NA

for (foo in seq_len(nrow(lee_2021_choice))) {
  lee_2021_choice$degreeL[[foo]] <- as.numeric(net_degree$closeness2[lee_2021_choice$itemL[[foo]] == net_degree$Name])
  lee_2021_choice$degreeR[[foo]] <- as.numeric(net_degree$closeness2[lee_2021_choice$itemR[[foo]] == net_degree$Name])
}
# relative diff
lee_2021_choice$rDegreeDiff <- scale(lee_2021_choice$degreeR - lee_2021_choice$degreeL)
lee_2021_choice$rDiff <- scale(lee_2021_choice$ratingR - lee_2021_choice$ratingL)
#overall value
lee_2021_choice$VSum <- scale(lee_2021_choice$ratingR + lee_2021_choice$ratingL)
lee_2021_choice$DegreeSum <- scale(lee_2021_choice$degreeR + lee_2021_choice$degreeL)

# cor.test(lee_2021_choice$rDiff,lee_2021_choice$rDegreeDiff)
# pca_res <- prcomp(lee_2021_choice[, c("rDegreeDiff","rDiff")])

# summary(glmer(choice ~ rDiff + (rDiff | subjectid), data = lee_2021_choice, family = "binomial"))
summary(glmer(choice ~ DegreeSum + (rDegreeSum | subjectid), data = lee_2021_choice, family = "binomial"))
summary(glmer(choice ~ rDegreeDiff + (rDegreeDiff | subjectid), data = lee_2021_choice, family = "binomial"))
summary(glmer(choice ~ rDegreeDiff*rDiff + (rDegreeDiff*rDiff  | subjectid), data = lee_2021_choice, family = "binomial"))

lee_2021_choice$absrDegreeDiff <- scale(abs(lee_2021_choice$degreeR - lee_2021_choice$degreeL))
lee_2021_choice$absrDiff <- scale(abs(lee_2021_choice$ratingR - lee_2021_choice$ratingL))

#coded for correct models
# summary(glmer(correct ~ absrDiff + (absrDiff  | subjectid), data = lee_2021_choice, family = "binomial"))
summary(glmer(correct ~ absrDegreeDiff + (absrDegreeDiff  | subjectid), data = lee_2021_choice, family = "binomial"))
summary(glmer(correct ~ absrDegreeDiff + absrDiff (absrDegreeDiff + absrDiff | subjectid), data = lee_2021_choice, family = "binomial"))
#rt models
m_0 <- lmer(log(rt) ~ absrDiff+ absrDegreeDiff + DegreeSum +  VSum + (absrDiff + absrDegreeDiff+ DegreeSum+VSum | subjectid), data = lee_2021_choice)
summary(m_0)
# library(ggeffects)
# plot(ggpredict(m_0, terms = c("absrDiff","absrDegreeDiff[-1, 1]")))


#we find no effect of the network measures in the choice data.
#we can find small effects