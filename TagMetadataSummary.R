library(readr)
library(dplyr)
library(janitor)
library(lubridate)


HalaviTaggingMetadata <- read_csv("data/HalaviTaggingMetadata.csv", 
                                  col_types = cols(UTC_RELEASE_DATE_TIME = col_datetime(format = "%m/%d/%y %H:%M"))) %>%  
  select(1:37) %>% clean_names() %>% 
  mutate(year = year(utc_release_date_time))


# Count -------------------------------------------------------------------
HalaviTaggingMetadata %>% group_by(year) %>% 
  summarise(count = n())


# Sex Ratio ---------------------------------------------------------------
HalaviTaggingMetadata %>% 
  count(sex)


# Age Group ---------------------------------------------------------------
HalaviTaggingMetadata %>% 
  count(life_stage)


# Location ----------------------------------------------------------------
HalaviTaggingMetadata %>% 
  group_by(year) %>% 
  count(capture_location)


  
  