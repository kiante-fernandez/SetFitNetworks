library(grid)
library(gridExtra)
library(here)
library(jpeg)
library(progress)

generate_image_group <- function(myfiles, ncol, file_name, grid_idx, trial_set) {
  image_file_idx <- trial_set[[grid_idx]]
  # record which images are selected for later (need to add)

  # stim_grid <-  arrangeGrob(
  #  rasterGrob(myfiles[[image_file_idx[[1]]]]),
  #  rasterGrob(myfiles[[image_file_idx[[2]]]]),
  #  rasterGrob(myfiles[[image_file_idx[[3]]]]),
  #  rasterGrob(myfiles[[image_file_idx[[4]]]]),
  #  rasterGrob(myfiles[[image_file_idx[[5]]]]),
  #  rasterGrob(myfiles[[image_file_idx[[6]]]]),
  #  rasterGrob(myfiles[[image_file_idx[[7]]]]),
  #  rasterGrob(myfiles[[image_file_idx[[8]]]]),
  #  rasterGrob(myfiles[[image_file_idx[[9]]]]),
  #  ncol=3)
  stim_grid <- arrangeGrob(
    rasterGrob(myfiles[[image_file_idx[[1]]]]),
    rasterGrob(myfiles[[image_file_idx[[2]]]]),
    rasterGrob(myfiles[[image_file_idx[[3]]]]),
    rasterGrob(myfiles[[image_file_idx[[4]]]]),
    rasterGrob(myfiles[[image_file_idx[[5]]]]),
    rasterGrob(myfiles[[image_file_idx[[6]]]]),
    ncol = ncol
  )
  file_name <- paste0("grid_", 2 * ncol, "_", file_name, grid_idx, ".jpg")
  ggplot2::ggsave(here::here("img", file_name), stim_grid, width = 4, height = 3)
}

# load all the images
food_folder <- here("data", "snackitemnames_nicholas", "Lee_Holyoak_2021_images")
FoodNames <- readxl::read_excel(here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))

# The 60 items we have in this data set.
images <- c(
  1, 5, 8, 12, 13, 16, 17, 18, 25, 26, 28, 33, 39, 51, 55, 60,
  61, 63, 65, 70, 77, 80, 93, 96, 98, 99, 100, 102, 103, 104, 105,
  106, 114, 115, 118, 119, 123, 124, 131, 142, 150, 153, 154, 155,
  158, 159, 160, 161, 164, 165, 168, 170, 172, 173, 174, 176, 184,
  187, 188, 200
)

# how do we get data for all the subjects?
temp <- list.files(path = food_folder, pattern = "*.jpg", full.names = T)


foods_in_image <- stringr::str_extract(temp, "item\\d+")
foods_in_image <- stringr::str_extract(foods_in_image, "\\d+")
# get row idx for each of the image numbers
foods_in_image <- tibble::rowid_to_column(data.frame(Image = as.numeric(foods_in_image)))
foods_in_image <- dplyr::left_join(FoodNames, foods_in_image, "Image")

# subset such that we only get the foods in the data set
nodes_temp <- temp[foods_in_image$rowid]
dput(as.numeric(stringr::str_extract(stringr::str_extract(nodes_temp, "item\\d+"), "\\d+")))
newlocation <- "/Users/kiantefernandez/Documents/OSU/similarity_networks/similarity/public/img/60Foods"
file.copy(from=nodes_temp, to=newlocation, 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

# we might need a for loop here instead can we figure out how to use map here?'
myfiles <- list()
for (idx in 1:length(temp)) {
  myfiles[[idx]] <- readJPEG(temp[idx])
}

network_stats <- c("assortment", "edge_density", "weighted_clustering_coefficient")

for (network_stat_idx in 1:3) {
  print(paste0("GENERATING STIMULI FOR: ", network_stats[[network_stat_idx]]))
  # get the information for each trial from the sub graph selection output trial generator (here titled `res`)
  load(file = here::here("data", paste0(network_stats[[network_stat_idx]], "_", 30, "_", 6, ".RData")))

  trial_set <- vector(mode = "list", length = ncol(res))
  for (graph_idk in seq_len(ncol(res))) {
    trial_set[[graph_idk]] <- dplyr::filter(foods_in_image, foods_in_image$Name %in% res[, graph_idk])$rowid
  }
  pb <- progress_bar$new(
    format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
    total = ncol(res),
    complete = "=", # Completion bar character
    incomplete = "-", # Incomplete bar character
    current = ">", # Current bar character
    clear = FALSE, # If TRUE, clears the bar when finish
    width = 100
  ) # Width of the progress bar

  file_name <- paste0(network_stats[[network_stat_idx]],"_")
  ncol <- 3
  for (grid_idx in seq_len(ncol(res))) {
    # Updates the current state
    pb$tick()
    generate_image_group(myfiles, 3, file_name, grid_idx, trial_set)
  }
}

