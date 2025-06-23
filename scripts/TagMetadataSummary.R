library(readr)
library(dplyr)
library(janitor)
library(lubridate)


# Count -------------------------------------------------------------------
HalaviTaggingMetadata %>% group_by(year) %>% 
  summarise(count = n())


# Sex Ratio ---------------------------------------------------------------
HalaviTaggingMetadata %>% 
  group_by(year) %>% 
  count(sex)


# Age Group ---------------------------------------------------------------
HalaviTaggingMetadata %>%
  group_by(year) %>% 
  count(life_stage)


# Location ----------------------------------------------------------------
HalaviTaggingMetadata %>% 
  group_by(year) %>% 
  count(capture_island)



# Size Metrics -------------------------------------------------------------------------
HalaviTaggingMetadata %>% 
  group_by(year) %>% 
  summarise(min_l = min(length_cm, na.rm = T),
            max_l = max(length_cm, na.rm = T),
            avg_l = mean(length_cm, na.rm = T),
            min_d = min(length2_cm, na.rm = T),
            max_d = max(length2_cm, na.rm = T),
            avg_d = mean(length2_cm, na.rm = T))

  
HalaviTaggingMetadata %>% 
  count(year,tag_model) 
