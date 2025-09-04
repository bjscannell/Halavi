##################################################
#' Caclulate all our individual metrics for the
#' guitarfish and overall tagging metrics
#################################################

# consecutive days

transmitter <- unique(dets$transmitter_id)

consec <- list()

for (i in 1:length(unique(dets$transmitter_id))) {
  consec[[i]] <- dets %>% filter(transmitter_id == transmitter[i]) %>%
    distinct(date, .keep_all = T) %>% 
    group_by(grp = cumsum(c(0, diff(date) > 1)), transmitter_id) %>% 
    mutate(ConsecutiveDays = row_number()) %>% ungroup(grp) %>% group_by(transmitter_id) %>% 
    summarise(transmitter_id = first(transmitter_id),
              consec = max(ConsecutiveDays))
}

consec <- dplyr::bind_rows(consec)


# full summary table -----------------------------------------------------------


res <- dets %>% group_by(transmitter_id) %>% 
  summarise(
    transmitter_id = first(transmitter_id),
    ID = first(animal_id_floy_tag_id_pit_tag_code_etc),
    Sex = first(sex),
    DW = first(length2_cm),
    TL = first(length_cm),
    Class = first(new_class),
    Tagging_Island = first(otn_array),
    Deployment_Date = first(tag_activation_date),
    Last_Detection = max(date),
    First_Detection = min(date),
    Total_No_Dets = n(),
    Days_Liberty = as.numeric(first(Last_Detection) - first(First_Detection)) + 1,
    Tag_on = first(tag_activation_date),
    Tag_off = first(tag_activation_date) + days(first(est_tag_life)),
    Days_Monitored = if_else(first(Last_Detection) > first(Tag_off),
                             first(Last_Detection) - first(Tag_on) + 1,
                             first(Tag_off) - first(Tag_on) +1),
    Days_Present = length(unique(date)),
    residency_max = as.numeric(Days_Present) / as.numeric(Days_Liberty),
    residency_min = as.numeric(Days_Present)/ as.numeric(Days_Monitored),
    res_ratio = as.numeric(Days_Liberty)/ as.numeric(Days_Monitored),
    # res_type = case_when(
    #   residency_min >= 0.65 & res_ratio > 0.70 ~ "Resident",
    #   residency_min < 0.65 & res_ratio > 0.40 ~ "Intermittent Resident",
    #   residency_min < 0.5 & res_ratio <= 0.5 ~ "Transient",
    #   TRUE ~ NA_character_),
    n_stations = n_distinct(station_no)) %>% 
  left_join(consec, by = "transmitter_id")


# Tagging efforts ---------------------------------------------------------
tagging_pool <- HalaviTaggingMetadata %>% 
  filter(new_class != "ADULT") %>% 
  filter(year != "2025") %>% 
  filter(capture_island != "Turtle Bay")

# Count -------------------------------------------------------------------
tagging_pool %>% group_by(year) %>% 
  summarise(count = n())


# Sex Ratio ---------------------------------------------------------------
tagging_pool %>% 
  group_by(year) %>% 
  count(sex)


# Age Group ---------------------------------------------------------------
HalaviTaggingMetadata %>%
  group_by(year) %>% 
  count(new_class)


# Location ----------------------------------------------------------------
tagging_pool %>% 
  group_by(year) %>% 
  count(capture_island)



# Size Metrics -------------------------------------------------------------------------
tagging_pool %>% 
  group_by(year) %>% 
  summarise(min_l = min(length_cm, na.rm = T),
            max_l = max(length_cm, na.rm = T),
            avg_l = mean(length_cm, na.rm = T),
            min_d = min(length2_cm, na.rm = T),
            max_d = max(length2_cm, na.rm = T),
            avg_d = mean(length2_cm, na.rm = T))


tagging_pool %>% 
  count(year,tag_model) 



# Detected Individuals ----------------------------------------------------

dets %>% distinct(transmitter_id, .keep_all = T) %>%  nrow()

dets %>% distinct(transmitter_id, .keep_all = T) %>% count(sex)

dets %>% distinct(transmitter_id, .keep_all = T) %>% count(new_class)

dets %>% distinct(transmitter_id, .keep_all = T) %>% count(otn_array)


dets %>% distinct(transmitter_id, .keep_all = T) %>% 
  group_by(sex) %>% 
  summarise(min_l = min(length_cm, na.rm = T),
            max_l = max(length_cm, na.rm = T),
            avg_l = mean(length_cm, na.rm = T),
            sd_l = sd(length_cm, na.rm = T)) %>% View()
  
res %>% summarise(
  residency_max_mean = mean(residency_max),
  residency_max_sd = sd(residency_max),
  res_ax_min = min(residency_max),
  res_ax_max = max(residency_max),
  residency_min_mean = mean(residency_min),
  residency_min_sd = sd(residency_min),
  res_min_min = min(residency_min),
  res_min_max = max(residency_min)
)


res %>% summarise(
  residency_max_mean = mean(RI),
  residency_max_sd = sd(residency_max),
  res_ax_min = min(residency_max),
  res_ax_max = max(residency_max))