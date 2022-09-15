# get_data.R - get the data from goolge drive
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
# 11/12/22      Kianté  Fernandez                       added plotting


library(googledrive) # An Interface to Google Drive
library(jsonlite) # A Simple and Robust JSON Parser and Generator for R
library(purrr) # Functional Programming Tools
library(tidyverse) # Easily Install and Load the 'Tidyverse'

library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4
library(lmerTest) # Tests in Linear Mixed Effects Models

library(gghalves) # Compose Half-Half Plots Using Your Favourite Geoms
library(ggforce) # Accelerating 'ggplot2'
library(ggdist) # Visualizations of Distributions and Uncertainty
library(patchwork) # The Composer of Plots

# helper functions for working with lists
list.do <- function(.data, fun, ...) {
  do.call(what = fun, args = as.list(.data), ...)
}
list.cbind <- function(.data) {
  list.do(.data, "cbind")
}

# drive_auth_configure(api_key = "AIzaSyBO6kKhgHGvepPphJw4Y6XtGBG98hcwaNs")
# drive_api_key()
# 
# # apply to a browser URL for, e.g., a Google Sheet
# my_url <- "https://drive.google.com/drive/folders/1PFHm5wI7hOz4eu1gRppwtgVFZkl5M_tk"
# 
# for (file_idx in seq_len(dim(drive_ls(drive_get(my_url)))[[1]])) {
#   temp <- drive_ls(drive_get(my_url))$drive_resource[[file_idx]]$originalFilename
#   drive_download(temp, here::here("data", "pilot_5", temp))
# }

temp_files <- list.files(path = here::here("data", "pilot_5"), pattern = ".json", full.names = T)

# load all the images to calculate the value for a group of foods
food_folder <- here::here("data", "snackitemnames_nicholas", "Lee_Holyoak_2021_images")
FoodNames <- readxl::read_excel(here::here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))
pilot_5_stimuli_sets <- read_csv("data/pilot_5/pilot_5_stimuli.csv", col_names = FALSE)

# NOTE NEXT TIME YOU WILL USE THIS FILE INSTEAD. THE 'RES' FILE (BC YOU DID THE NAMES RIGHT)
# network_stats <- c("assortment", "edge_density", "weighted_clustering_coefficient")
# load(file = here::here("data", paste0(network_stats[[1]], "_", 30, "_", 6, ".RData")))

# The 60 items we have in this data set.
images <- c(
  1, 5, 8, 12, 13, 16, 17, 18, 25, 26, 28, 33, 39, 51, 55, 60,
  61, 63, 65, 70, 77, 80, 93, 96, 98, 99, 100, 102, 103, 104, 105,
  106, 114, 115, 118, 119, 123, 124, 131, 142, 150, 153, 154, 155,
  158, 159, 160, 161, 164, 165, 168, 170, 172, 173, 174, 176, 184,
  187, 188, 200
)

temp <- list.files(path = food_folder, pattern = "*.jpg", full.names = T)
foods_in_image <- stringr::str_extract(temp, "item\\d+")
foods_in_image <- stringr::str_extract(foods_in_image, "\\d+")
# get row idx for each of the image numbers
foods_in_image <- tibble::rowid_to_column(data.frame(Image = as.numeric(foods_in_image)))
foods_in_image <- dplyr::left_join(FoodNames, foods_in_image, "Image")

file_idx <- 5
subject_df <- vector(mode = "list", length = file_idx)

assortment_levels <- data.frame(name = seq_len(30))
assortment_levels$value <- c(
  -0.9143, -0.9268, -0.8084, -0.7311, -0.7359, -0.6536, -0.5664,
  -0.4486, -0.4231, -0.3244, -0.2561, -0.2656, -0.162, -0.087,
  -0.0392, 0.0554, 0.1184, 0.1747, 0.2779, 0.2504, 0.3117, 0.4642,
  0.4548, 0.5435, 0.6053, 0.7219, 0.7867, 0.7875, 0.8841, 1
)

for (pp in seq_len(file_idx)) {
  subject_temp <- parse_json(read_json(temp_files[[pp]]), simplifyVector = T)

  # this gets the ratings in check
  subject_rating_temp <- subject_temp %>%
    filter(screen_id == "ratings") %>%
    select(stimulus, response) %>%
    mutate(
      Image = stringr::str_remove(stimulus, pattern = "../../img/60Foods/item"),
      Image = as.numeric(stringr::str_remove(Image, pattern = ".jpg"))
    ) %>%
    dplyr::left_join(foods_in_image, by = "Image")
  # we need to score each of the stimuli for a subject:
  # BUT somehow the networks are not the ones we saved as images,
  # so, we cannot do this until we generate an excel sheet of the images
  # sort of like the res file, but correct ,
  # this gets the choices in check use old fig temp todo this

  set_values_temp <- vector(mode = "numeric", length = 30)
  for (foo in 1:30) {
    set_values_temp[[foo]] <- sum(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% pilot_5_stimuli_sets[[foo]], ]$response))
    # set_values_temp[[foo]] <- mean(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% pilot_5_stimuli_sets[[foo]],]$response))
  }
  print(cor.test(set_values_temp, assortment_levels$value))

  task_temp <- subject_temp %>%
    filter(screen_id == "task") %>%
    select(subject_id, rt, options, key_press) %>%
    mutate(key_press = ifelse(key_press == "f", 1, 0))

  xxxx <- as.data.frame(do.call(rbind, task_temp$options)) %>% mutate(subject_id = pp)
  xxxx[, 1] <- as.numeric(str_remove(str_remove(xxxx[, 1], pattern = "../../img/grid_stimuli/grid_6_assortment_"), ".jpg"))
  xxxx[, 2] <- as.numeric(str_remove(str_remove(xxxx[, 2], pattern = "../../img/grid_stimuli/grid_6_assortment_"), ".jpg"))
  xxxx$rt <- task_temp$rt / 1000
  xxxx$choice <- task_temp$key_press
  names(xxxx) <- c("left", "right", "subject_id", "rt", "choice")

  xxxx$left_rating <- NULL
  xxxx$right_rating <- NULL
  for (foo in seq_len(nrow(xxxx))) {
    xxxx$left_rating[[foo]] <- as.numeric(set_values_temp[xxxx$left[[foo]]])
    xxxx$right_rating[[foo]] <- as.numeric(set_values_temp[xxxx$right[[foo]]])
  }

  subject_df[[pp]] <- xxxx
}

df <- as.data.frame(do.call(rbind, subject_df)) %>%
  unnest(cols = c(left_rating, right_rating))

df$left <- assortment_levels[df$left, "value"]
df$right <- assortment_levels[df$right, "value"]

remove <- round(length(df[df$subject_id == 1, ]$rt) * 0.1) # how many trials is 10%

df %>%
  group_by(subject_id) %>%
  arrange(desc(rt)) %>%
  slice(-(1:3)) %>%
  ggplot(aes(subject_id, rt, fill = factor(subject_id), group = factor(subject_id))) +
  ggdist::stat_halfeye(justification = -.3, point_colour = NA) +
  geom_boxplot(width = .1, outlier.shape = NA) +
  gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .5) +
  theme_classic() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "subjects", y = "RT(s)") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_wrap(~subject_id, scales = "free")


tune <- .5 # the amount of binning
p3 <- df %>%
  group_by(subject_id) %>%
  arrange(desc(rt)) %>%
  slice(-(1:3)) %>%
  ungroup() %>%
  mutate(vd = left_rating - right_rating) %>%
  group_by(subject_id) %>%
  mutate(Dif = scale(vd)) %>%
  ungroup() %>%
  mutate(
    binned_value_diff = tune * round(Dif / tune),
  ) %>%
  ungroup() %>%
  group_by(subject_id, binned_value_diff) %>%
  mutate(
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_value_diff, y = m_rt, group = subject_id)) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  theme_classic() +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "RT(s)",
    x = "Value Difference (L-R)"
  ) +
  facet_wrap(~subject_id, scales = "free")

p4 <- df %>%
  group_by(subject_id) %>%
  arrange(desc(rt)) %>%
  slice(-(1:3)) %>%
  ungroup() %>%
  mutate(nd = left - right) %>%
  group_by(subject_id) %>%
  mutate(Dif = scale(nd)) %>%
  ungroup() %>%
  mutate(
    binned_network_diff = tune * round(Dif / tune),
  ) %>%
  ungroup() %>%
  group_by(subject_id, binned_network_diff) %>%
  mutate(
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_network_diff, y = m_rt, group = subject_id)) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  theme_classic() +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "RT(s)",
    x = "Network Difference (L-R)"
  ) +
  facet_wrap(~subject_id, scales = "free")


tune <- .7 # the amount of binning

p1 <- df %>%
  group_by(subject_id) %>%
  arrange(desc(rt)) %>%
  slice(-(1:3)) %>%
  ungroup() %>%
  mutate(vd = left_rating - right_rating) %>%
  group_by(subject_id) %>%
  mutate(Dif = scale(vd)) %>%
  ungroup() %>%
  mutate(
    binned_value_diff = tune * round(Dif / tune),
  ) %>%
  ungroup() %>%
  group_by(subject_id, binned_value_diff) %>%
  mutate(
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_value_diff, y = m_left, group = subject_id)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  theme_classic() +
  geom_line(size = 1) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    y = "Probability of Choosing Left",
    x = "Value Difference (L-R)"
  ) +
  facet_wrap(~subject_id)


tune <- .7
p2 <- df %>%
  group_by(subject_id) %>%
  arrange(desc(rt)) %>%
  slice(-(1:3)) %>%
  ungroup() %>%
  mutate(nd = left - right) %>%
  group_by(subject_id) %>%
  mutate(Dif = scale(nd)) %>%
  ungroup() %>%
  mutate(
    binned_network_diff = tune * round(Dif / tune),
  ) %>%
  group_by(subject_id, binned_network_diff) %>%
  mutate(
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_network_diff, y = m_left, group = subject_id)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  theme_classic() +
  geom_line(size = 1) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    y = "Probability of Choosing Left",
    x = "Network Difference (L-R)"
  ) +
  facet_wrap(~subject_id)

p1 + p2
p3 + p4

model_dat <- df %>%
  group_by(subject_id) %>%
  arrange(desc(rt)) %>%
  slice(-(1:3)) %>%
  mutate(
    nd = scale(left - right),
    vd = scale(left_rating - right_rating)
  ) %>%
  ungroup()

mlm1 <- lmer(log(rt) ~ vd + nd + (1 | subject_id), data = model_dat)
summary(mlm1)

mlm2 <- glmer(choice ~ vd + nd +
  (1 | subject_id), data = model_dat, family = "binomial")
summary(mlm2)

# tune <- .8
# df %>%
#   group_by(subject_id) %>%
#   arrange(desc(rt)) %>%
#   slice(-(1:3)) %>%
#   ungroup() %>%
#   mutate(nd = left - right) %>%
#   group_by(subject_id) %>%
#   mutate(Dif = scale(nd)) %>%
#   ungroup() %>%
#   mutate(
#     binned_network_diff = tune * round(Dif / tune),
#   ) %>%
#   group_by(binned_network_diff) %>%
#   mutate(
#     m_left = mean(choice),
#     se = sqrt(var(choice) / length(choice))
#   ) %>%
#   ggplot(aes(x = binned_network_diff, y = m_left)) +
#   geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
#   theme_classic() +
#   geom_line(size = 1) +
#   geom_hline(yintercept = .5, linetype = "dashed") +
#   scale_color_brewer(palette = "Set1") +
#   scale_y_continuous(limits = c(0, 1.01)) +
#   labs(
#     y = "Probability of Choosing Left",
#     x = "Network Difference (L-R)"
#   )
# 
# df %>%
#   group_by(subject_id) %>%
#   arrange(desc(rt)) %>%
#   slice(-(1:3)) %>%
#   ungroup() %>%
#   mutate(vd = left_rating - right_rating) %>%
#   group_by(subject_id) %>%
#   mutate(Dif = scale(vd)) %>%
#   ungroup() %>%
#   mutate(
#     binned_value_diff = tune * round(Dif / tune),
#   ) %>%
#   ungroup() %>%
#   group_by(binned_value_diff) %>%
#   mutate(
#     m_left = mean(choice),
#     se = sqrt(var(choice) / length(choice))
#   ) %>%
#   ggplot(aes(x = binned_value_diff, y = m_left)) +
#   geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
#   theme_classic() +
#   geom_line(size = 1) +
#   geom_hline(yintercept = .5, linetype = "dashed") +
#   scale_color_brewer(palette = "Set1") +
#   scale_y_continuous(limits = c(0, 1.01)) +
#   labs(
#     y = "Probability of Choosing Left",
#     x = "Value Difference (L-R)"
#   )

