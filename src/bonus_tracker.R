# bonus_tracker.R - get the data from goolge drive and check for bonus
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
# 2022/12/21      Kianté  Fernandez                       updated analysis

library(googledrive) # An Interface to Google Drive
library(jsonlite) # A Simple and Robust JSON Parser and Generator for R
library(purrr) # Functional Programming Tools
library(tidyverse) # Easily Install and Load the 'Tidyverse'

# drive_auth_configure(api_key = "AIzaSyBO6kKhgHGvepPphJw4Y6XtGBG98hcwaNs")
# drive_api_key()
#

# # apply to a browser URL for, e.g., a Google Sheet
# my_url <- "https://drive.google.com/drive/folders/1PFHm5wI7hOz4eu1gRppwtgVFZkl5M_tk"
# #
# drive_ls(drive_get(my_url)) %>% View
#   
#   
# for (file_idx in seq_len(dim(drive_ls(drive_get(my_url)))[[1]])) {
#   temp <- drive_ls(drive_get(my_url))$drive_resource[[file_idx]]$originalFilename
#   drive_download(temp, here::here("data", "exp_2", temp), overwrite = TRUE)
# }

temp_files <- list.files(path = here::here("data", "exp_2"), pattern = ".json", full.names = T)
temp_files
tracker_dat <- readr::read_csv(here::here("data", "prolific_bonus_tracker.csv"))

tracker_dat$bonus <- NA
#load the  subjects data
subject_temp <- parse_json(read_json(temp_files[[14]]), simplifyVector = T)

bonus_tracker <- function(x){
  subject_temp <- parse_json(read_json(x), simplifyVector = T)
  
  ans <- ifelse(stringr::str_detect(subject_temp$stimulus[subject_temp$screen_id == "bonus"][[6]],"Bonus two dollars"), 1,0)

  return(ans)
}

for (foo in 1:75){
  subject_temp <- parse_json(read_json(temp_files[[foo]]), simplifyVector = T)
  
  ans <- ifelse(stringr::str_detect(subject_temp$stimulus[subject_temp$screen_id == "bonus"][[6]],"Bonus two dollars"), 1,0)
  if (ans == 1){
    tracker_dat$bonus[which(unique(subject_temp$subject_id) == tracker_dat$`Participant id`)] = ",2"
  }else {
    tracker_dat$bonus[which(unique(subject_temp$subject_id) == tracker_dat$`Participant id`)] = 0
  }
}

to_pay <- tracker_dat %>% select(`Participant id`, bonus) %>% 
  filter(bonus == ",2")

write.csv(to_pay,here::here("data", "bonus_payment.csv"))

#map through and get each subjects bonus
sum(unlist(purrr::map(temp_files, bonus_tracker))) #we had 40 subjects get the bonus payment

