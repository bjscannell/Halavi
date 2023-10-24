library(readr)
library(dplyr)
library(janitor)
library(lubridate)


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
  count(release_location)


  
  