
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(lubridate)
library(readr)
library(ggplot2)

dets <- read_csv("data/2deployment.csv", 
                 col_types = cols(`Date and Time (UTC)` = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) %>% clean_names()


dets <- dets %>% 
  mutate(transmitter_codespace = "A69-9005") %>% 
  rename(detection_timestamp_utc = date_and_time_utc,
         transmitter_id = transmitter,
         receiver_sn = receiver)


dets <- glatos::false_detections(dets, tf = 3600) %>% filter(passed_filter == 1) %>% 
  filter(transmitter_id != "A69-1605-73")

dets %>% mutate(date = date(detection_timestamp_utc)) %>% 
  group_by(date) %>% 
  summarise(count = n_distinct(transmitter_id)) %>% 
  ggplot(aes(x=date, y = count)) + ()



dets %>% 
  mutate(date = date(detection_timestamp_utc)) %>% 
  distinct(date, transmitter_id) %>%
  ggplot(aes(x = date, y = transmitter_id)) +
  geom_point(shape = 20) + theme_minimal() +
  scale_x_date(date_labels = "%b-%Y") +
  labs(
    x = "Date",
    y = "Halavi Guitarfish ID",
    title = "Presence of Halavi Guitarfish in Al Wajh",
    subtitle = "Detections for 17 tagged individuals by date from acoustic receivers at Quman Island"
  ) +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.title.position = "plot", 
        plot.caption.position =  "plot",
        plot.title = element_text(face = "bold", size = 16)) 

