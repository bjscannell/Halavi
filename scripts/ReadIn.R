
library(readr)
library(dplyr)
library(janitor)
library(stringr)
library(vroom)
library(lubridate)


# detection dataset setup -------------------------------------------------




dets_full <- read_csv("data/full_array_detections0224.csv", 
                      col_types = cols(`Date and Time (UTC)` = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) %>% clean_names()  %>% 
  filter(date_and_time_utc > mdy('04-27-2023'))

dets_line <- read_csv("data/first_deployment_1023.csv", 
                      col_types = cols(`Date and Time (UTC)` = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) %>% clean_names() %>% 
  filter(station_name == "Qu2" | station_name == "Qu3")

# alittle bit of fuckery with the station names for now
halavi_array <- read_csv("data/halavi_array.csv") %>% clean_names() %>% select(receiver, station_name, latitude, longitude)

dets <- rbind(dets_line, dets_full) %>% left_join(halavi_array, by = "receiver")

dets <- dets %>% select(-c(station_name.x, latitude.x, longitude.x)) %>% 
                          rename(station_name = station_name.y,
                                 latitude = latitude.y,
                                 longitude = longitude.y)


dets <- dets %>% 
  mutate(transmitter_codespace = str_extract(transmitter, "^[^-]*-[^-]*"),
         date = date(date_and_time_utc)) %>% 
  rename(detection_timestamp_utc = date_and_time_utc,
         transmitter_id = transmitter,
         receiver_sn = receiver)


dets <- glatos::false_detections(dets, tf = 3600) %>% filter(passed_filter == 1) %>% 
  filter(transmitter_id != "A69-1605-73") %>% 
  filter(transmitter_codespace != "A69-1601")



# tag metadata ------------------------------------------------------------


HalaviTaggingMetadata <- read_csv("data/HalaviTaggingMetadata.csv", 
                                  col_types = cols(UTC_RELEASE_DATE_TIME = col_datetime(format = "%m/%d/%y %H:%M"),
                                                   TAG_ACTIVATION_DATE = col_date(format = "%d-%b-%y"))) %>%  
  dplyr::select(1:38) %>% clean_names() %>% 
  mutate(year = year(utc_release_date_time))


dets <- dets %>% left_join(HalaviTaggingMetadata, by = "transmitter_id")

