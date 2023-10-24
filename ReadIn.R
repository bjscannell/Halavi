
library(readr)
library(dplyr)
library(janitor)
library(lubridate)


# detection dataset setup -------------------------------------------------


dets_full <- read_csv("data/full-array_detections_1023.csv", 
                      col_types = cols(`Date and Time (UTC)` = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) %>% clean_names() %>% 
  filter(date_and_time_utc > mdy('04-27-2023'))

dets_line <- read_csv("data/first_deployment_1023.csv", 
                      col_types = cols(`Date and Time (UTC)` = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) %>% clean_names()

dets <- rbind(dets_line, dets_full)

dets <- dets %>% 
  mutate(transmitter_codespace = "A69-9005",
         date = date(date_and_time_utc)) %>% 
  rename(detection_timestamp_utc = date_and_time_utc,
         transmitter_id = transmitter,
         receiver_sn = receiver)


dets <- glatos::false_detections(dets, tf = 3600) %>% filter(passed_filter == 1) %>% 
  filter(transmitter_id != "A69-1605-73")



# tag metadata ------------------------------------------------------------


HalaviTaggingMetadata <- read_csv("data/HalaviTaggingMetadata.csv", 
                                  col_types = cols(UTC_RELEASE_DATE_TIME = col_datetime(format = "%m/%d/%y %H:%M"),
                                                   TAG_ACTIVATION_DATE = col_date(format = "%d-%b-%y"))) %>%  
  dplyr::select(1:38) %>% clean_names() %>% 
  mutate(year = year(utc_release_date_time))


dets <- dets %>% left_join(HalaviTaggingMetadata, by = "transmitter_id")
