x <- HalaviArray %>% distinct(station_no, .keep_all = T) %>% select(station_no, deploy_lat, deploy_long) %>% 
  ggplot() +
  geom_jitter(aes(x=deploy_long, y=deploy_lat, color = station_no)) 


ggplotly(x)


# no youngings where detected anywhere outside the array they were tagged at
dets %>% group_by(transmitter_id) %>% 
  distinct(otn_array, .keep_all = T) %>% select(capture_island, otn_array) %>% 
  mutate(leave_array = otn_array==capture_island) %>% View()

day_dets <- dets %>% group_by(date,transmitter_id) %>% 
  mutate(daily_dets = n()) 



  ggplot(aes(x=date,y=transmitter_id, size=daily_dets)) +
  geom_point(shape = ".")
