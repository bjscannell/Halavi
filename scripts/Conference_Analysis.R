

# Constraining all the data to the first 17 tags --------------------------

dets_og <- dets %>% 
  filter(tag_activation_date < as.Date('01-27-2023',format ="%m-%d-%Y")) %>% 
  mutate(date = date(detection_timestamp_utc))



# TagMetadata  ------------------------------------------------------------

HalaviTaggingMetadata %>% 
  filter(utc_release_date_time < as.Date('01-27-2023',format ="%m-%d-%Y")) %>% 
  ggplot() +
  geom_point(aes(x = life_stage, y = length_m))


# Abacus plot -------------------------------------------------------------

dets_og %>% 
  ggplot() +
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



# Calculating individual metrics ---------------------------------------------

# consecutive days

transmitter <- unique(dets_og$transmitter_id)

consec <- list()

for (i in 1:length(unique(dets_og$transmitter_id))) {
  consec[[i]] <- dets_og %>% filter(transmitter_id == transmitter[i]) %>%
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
    DW = first(length2_m),
    Class = first(life_stage),
    Deployment_Date = first(tag_activation_date),
    Last_Detection = max(date),
    Total_No_Dets = n(),
    Days_Liberty = max(date) - min(date) + 1,
    Days_Monitored = as.numeric(mdy('02-15-2024') - first(tag_activation_date)) + 1, # change to pull from sheet
    Days_Present = length(unique(date)),
    residency_max = as.numeric(Days_Present) / as.numeric(Days_Liberty),
    residency_min = as.numeric(Days_Present)/ as.numeric(Days_Monitored),
    res_ratio = as.numeric(Days_Liberty)/ as.numeric(Days_Monitored),
    res_type = case_when(
      residency_min >= 0.5 ~ "Resident",
      residency_min < 0.5 & res_ratio > 0.5 ~ "Intermittent Resident",
      residency_min < 0.5 & res_ratio <= 0.5 ~ "Transient",
      TRUE ~ NA_character_),
    n_stations = n_distinct(station_name)) %>% 
  left_join(consec, by = "transmitter_id")


# residency plot ----------------------------------------------------------

res %>% 
  ggplot(aes(x=res_ratio, y = residency_min,
             color = res_type, shape = Class)) +
  geom_point()


# number of stations by size ----------------------------------------------

dets %>% group_by(transmitter_id) %>% 
  mutate(n_stations = n_distinct(station_name)) %>% 
  distinct(transmitter_id, .keep_all = T) %>% 
  ggplot(aes(x=n_stations, y=length_m)) +
  geom_point() +
  geom_label(aes(label =length_m)) + 
  geom_smooth(method = "lm", se = T)