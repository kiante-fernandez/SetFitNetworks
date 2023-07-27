# reward_procedure.r: checks and distributes rewards for bonus payment and
# randomly selects a trial and food from a set for additional payment
# the script returns a csv with all the information to proceed with payment
library(tidyverse)

temp_files <- list.files(path = here::here("data", "exp_3", "drive-20230627"), pattern = ".json", full.names = T)
network_stats <- "average_strength"
sim_img_pattern <- "../../img/grid_stimuli/grid_6_average_strength_"

# load subgraphs
load(file = here::here("data", "average_strength_100_6.RData"))

file_idx <- length(temp_files) # how many subjects data to preprocess
############################
## organize the data and calculate value of group of items and net stats for each subject
##
subject_df <- vector(mode = "list", length = file_idx)

check_reward <- function(df) {
  temp_df <- df %>% filter(screen_id == "bonus" | screen_id == "additional_payment")

  reward_temp <- data.frame(subject_id = subject_temp$subject_id[1], subject_email = temp_df$subject_email[1], subject_venmo = temp_df$subject_venmo[1], sim_bonus = NA, choice_bonus = NA, address = NA, food_to_ship = NA)

  # Check if string contains "Congrats!"
  if (grepl("Congrats!", temp_df$stimulus[1])) {
    print("subject gets $3 bonus")
    reward_temp$sim_bonus <- TRUE
  } else {
    print("subject gets no cash bonus")
    reward_temp$sim_bonus <- FALSE
  }
  # check for additional payment information
  if (length(temp_df$response[2][[1]]) == 0) {
    reward_temp$choice_bonus <- FALSE
    print("subject does not get a food")
  } else {
    reward_temp$choice_bonus <- TRUE

    print("subject gets a food")
    reward_temp$address <- paste(unlist(temp_df$response[2][[1]]), collapse = " ")
  }
  return(reward_temp)
}

get_random_food <- function(df) {
  # select random trial
  temp_df <- df %>%
    filter(screen_id == "task") %>%
    slice(sample(1:100, 1)) %>%
    mutate(key_press = ifelse(key_press == "f", 1, 2)) %>%
    select(response, key_press, options)
  # parse chosen set and find items in image
  temp_string <- temp_df$options[[1]][temp_df$key_press]
  foods_temp <- res[, as.numeric(str_remove(str_remove(temp_string, pattern = paste0("../../img/grid_stimuli/grid_6_", network_stats, "_")), ".jpg"))]
  # select random food
  return(foods_temp[sample(1:6, 1)])
}

reward_res <- vector(mode = "list", length = file_idx)

for (pp in seq_len(file_idx)) {
  # load the  subjects data
  print(paste0("######## subject: ", pp, " #######"))
  subject_temp <- jsonlite::parse_json(jsonlite::read_json(temp_files[[pp]]), simplifyVector = T)
  reward_res[[pp]] <- check_reward(subject_temp)
  

  if (reward_res[[pp]]$choice_bonus == TRUE) {
    reward_res[[pp]]$food_to_ship <- get_random_food(subject_temp)
  }
  #calculate hourly payment
  reward_res[[pp]]$payment_amount <- subject_temp %>%
    filter(trial_type == "fullscreen") %>%
    select(time_elapsed) %>%
    mutate(diff_row = lead(time_elapsed) - time_elapsed) %>%
    head(1) %>%
    select(diff_row) %>%
    mutate(payment_amount = (diff_row / 3600000) * 15) %>%
    select(payment_amount) %>% as.numeric()
  
}

reward_res <- do.call(rbind, reward_res)
reward_res$subject_number <- 1:nrow(reward_res)
reward_res$paidYN <- "no"
reward_res$payment_amount <- if_else(reward_res$choice_bonus == T, reward_res$payment_amount + 3, reward_res$payment_amount )
reward_res$payment_date <- Sys.Date()
#do not just save over copies
if (!file.exists(here::here("data", "simnet_payment_log.csv"))) {
  write_csv(reward_res, "data/simnet_payment_log.csv")
  # write_csv(reward_res, "data/simnet_payment_log_p2.csv")
  
} else
{
  temp_res <- readr::read_csv("data/simnet_payment_log.csv")
  subject_paid <- temp_res$subject_id[temp_res$paidYN == "yes"]
}
reward_res$paidYN <- if_else(reward_res$subject_id %in% subject_paid, "yes", "no")
reward_res <- reward_res %>% arrange(paidYN)

# write_csv(reward_res, "data/simnet_payment_log_p2.csv")




