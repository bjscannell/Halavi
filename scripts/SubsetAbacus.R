
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(lubridate)
library(readr)
library(ggplot2)

dets2 <- dets %>% 
  mutate(date = date(detection_timestamp_utc)) %>% 
  distinct(date, transmitter_id, .keep_all = T) %>% 
  filter(tag_activation_date < mdy('01-01-2023'))


x <- ggplot(dets2) +
  geom_point(aes(x = date, y = reorder(transmitter_id, tag_activation_date, decreasing = T)), shape = 20) +
  geom_point(aes(x=tag_activation_date, y = transmitter_id), shape = 4) +
  geom_point(data = filter(dets2, tag_model == "V9"), 
             aes(x = tag_activation_date + days(403), y = transmitter_id), 
             color = "red", shape = 4) +
  theme_minimal() +
  scale_x_date(date_labels = "%b-%Y") +
  labs(
    x = "Date",
    y = "Halavi Guitarfish ID",
    title = "Presence of Halavi Guitarfish in Al Wajh",
    subtitle = "Detections for 17 tagged individuals by date from acoustic receivers at Quman Island") +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.title.position = "plot", 
        plot.caption.position =  "plot",
        plot.title = element_text(face = "bold", size = 16)) 

ggsave("abacus.png",x)

dets2 %>% 
  ggplot(aes(x=detection_timestamp_utc, y = transmitter_id)) +
  geom_point() +
  facet_wrap(~station_name)
