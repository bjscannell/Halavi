##################################################
#' Read in and create our dataset for YOY and JUV
#' guitarfish within Wajh lagoon
#################################################

library(readr)
library(stringr)
library(janitor)
library(dplyr)
library(lubridate)
library(vroom)


# Check we have all the files ---------------------------------------------
#' Read in the tag metadata, receiver metadata and all our csv files

HalaviTaggingMetadata <- read_csv("data/tags/HalaviTaggingMetadata.csv", 
                                  col_types = cols(UTC_RELEASE_DATE_TIME = col_datetime(format = "%m/%d/%y %H:%M"),
                                                   TAG_ACTIVATION_DATE = col_date(format = "%d-%b-%y"))) %>%  clean_names() %>% 
  mutate(year = year(utc_release_date_time),
         end_date = tag_activation_date+est_tag_life) %>% 
  mutate(new_class = case_when(
    length_cm < 40 ~ "YOY",
    length_cm >= 70 ~ "ADULT",
    .default = "JUV"
  )) 


HalaviArray <- read_csv("data/stations/HalaviArray.csv", 
                        col_types = cols(`DEPLOY_DATE_TIME (yyyy-mm-ddThh:mm:ss)` = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         `RECOVER_DATE_TIME (yyyy-mm-ddThh:mm:ss)` = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) 


files <- list.files("data/detections/halavi_files")

# Making sure we have all the files
names <- unique(c(str_extract(files[which(str_count(files, " ") == 3)],  "^[^.]*"),
                  str_extract(files[which(str_count(files, " ") == 4)],"^([^ ]* +){3}[^ ]*")))


test <- data.frame(names, x = seq(1, length(names)))

HalaviArray <- HalaviArray %>%
  filter(!is.na(FILENAME)) %>%
  mutate(names = if_else(str_count(FILENAME, " ") == 3,str_extract(FILENAME, "^[^.]*"),
                         str_extract(FILENAME, "^([^ ]* +){3}[^ ]*"))) %>%
  clean_names()

#HalaviArray %>% left_join(test, by = "names") %>% View()



# Join files with station and clip to dates -------------------------------
#' read in the raw csv files and deal with the weird new formating
#' filter the csv for the deploy and recovery date based off out
#' receiver metadata


files <- list.files("data/detections/halavi_files", full.names = T)

raw <- vroom(files, skip = 1, id = "source") %>% clean_names()

dets_raw <- raw %>% filter(record_type == "DET") %>% 
  dplyr::select(field_2, field_7, field_10, source) %>% 
  dplyr::rename(detection_timestamp = field_2,
         receiver = field_7,
         tag_id = field_10) %>% 
  filter(!str_detect(tag_id, "A69-1601")) %>% 
  mutate(id = if_else(str_detect(source,"VR2Tx-69"), 
                      str_extract(source,  "VR2Tx-69 .*?(?=\\.)"),
                      str_extract(source,  "VR2W-69 .*?(?=\\.)")))


dets <- dets_raw %>% left_join(HalaviArray, by = c("id" = "names")) %>%  
  mutate(detection_timestamp = ymd_hms(detection_timestamp, quiet = FALSE, tz = "UTC")) %>%
  group_by(id) %>% 
  filter(detection_timestamp > deploy_date_time_yyyy_mm_dd_thh_mm_ss &
           detection_timestamp < recover_date_time_yyyy_mm_dd_thh_mm_ss) %>% ungroup() %>% 
  dplyr::select(detection_timestamp, tag_id, receiver, otn_array,
                station_no, deploy_lat, deploy_long, bottom_depth)


# filter for false detections ---------------------------------------------

dets_g <- dets %>% 
  mutate(transmitter_codespace = str_extract(tag_id, "^[^-]*-[^-]*"),
         day = date(detection_timestamp)) %>% 
  dplyr::rename(detection_timestamp_utc = detection_timestamp,
         transmitter_id = tag_id,
         receiver_sn = receiver)

dets <-  glatos::false_detections(dets_g, tf = 3600) %>% filter(passed_filter == 1) %>% 
  filter(transmitter_id != "A69-1605-73")


# Join with metadata ------------------------------------------------------

dets <- dets %>% left_join(HalaviTaggingMetadata, by = "transmitter_id")

# remove the first line
# dets <- dets %>% 
#   filter(!str_detect(station_no, "GH")) %>% 
#   filter(transmitter_id %in% HalaviTaggingMetadata$transmitter_id)
  

# lets take just the quman tags that are dead
# tags <- HalaviTaggingMetadata %>% 
#   filter(capture_island == "Quman") %>% 
#   #mutate(end_date = tag_activation_date+est_tag_life) %>% 
#   #filter(end_date <= date("2025-01-01")) %>%
#   pull(transmitter_id)



#' Here were taking just receiver locations from the Quman array
#' that means we still use the data from our first array (GH), but 
#' only the receivers that we left in the same spots.  Akso filter our data
#' for JUST the guitarfish tags, data from before May 2025 and remove 
#' all adults

dets <- dets %>% ungroup() %>% 
  filter(str_detect(station_no,  paste(c("GH06", "GH14"), collapse = "|")) |
           !str_detect(station_no, "GH")) %>% 
  mutate(station_no = case_when(
    station_no == "GH06" ~ "Qu2",
    station_no == "GH14" ~ "Qu3",
    .default = station_no
  )) %>% 
  filter(transmitter_id %in% HalaviTaggingMetadata$transmitter_id) %>% 
  filter(year(utc_release_date_time) < year("2025-05-01 08:27:38")) %>% 
  filter(new_class != "ADULT") %>% 
  mutate(date = date(detection_timestamp_utc)) %>% 
  arrange(detection_timestamp_utc) %>% 
  filter(otn_array != "Turtle Bay") %>% 
  group_by(transmitter_id) %>%
  filter(n_distinct(date) > 2) %>% ungroup() 

# Adding time of day ------------------------------------------------------
library(suncalc)

dets_tz <- dets %>% 
  mutate(date_a = as.Date(with_tz(detection_timestamp_utc, tzone = "Asia/Riyadh")),
         date_times_a = with_tz(detection_timestamp_utc, tzone = "Asia/Riyadh")) 

SaudiSUN <- getSunlightTimes(distinct(dets_tz, date_a)$date_a,
                             lat = 25.551725,
                             lon = 36.845717,
                             keep = c("sunrise", "sunset"),
                             tz = "Asia/Riyadh")

dets_tz_tod <- dets_tz %>% 
  left_join(SaudiSUN, by = c("date_a" = "date")) %>%
  mutate(tod = ifelse(date_times_a >= sunrise & date_times_a < sunset, 'Day', 'Night')) %>% 
  dplyr::select(-c(lat, lon, sunrise, sunset))

