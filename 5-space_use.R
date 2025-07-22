

# Create COA --------------------------------------------------------------

coa <- dets %>%
  mutate(time_bin = floor_date(detection_timestamp_utc, unit = "2 hours")) %>%
  group_by(transmitter_id, time_bin) %>%
  summarise(
    lat = mean(deploy_lat, na.rm = TRUE),
    lon = mean(deploy_long, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  group_by(transmitter_id) %>% 
  filter(n_distinct(lat,lon) >10) %>% ungroup()
