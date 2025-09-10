stations <- HalaviArray %>% filter(otn_array == "QUMAN") %>%
  distinct(station_no, .keep_all = T)  %>% select(station_no, deploy_lat, deploy_long, ins_serial_no) %>% 
  mutate(ins_serial_no = as.character(ins_serial_no))

# verifying some temperature stuff ----------------------------------------


stations$station_no[stations$station_no == "Qu3"] <- "North"


temp <- raw %>% filter(record_type == "TEMP") %>% select(field_2, field_7, field_8) %>% 
  mutate(datetime = ymd_hms(field_2),
         receiver = field_7,
         temp = as.numeric(field_8),
         month = month(datetime), 
         date = date(datetime)) %>% 
  drop_na() %>% 
  filter(datetime > ymd_hms("2022-09-03 16:01:43 EDT")) %>% ungroup() %>%  
  left_join(stations, by = c("receiver" = "ins_serial_no")) %>% 
  filter(!str_detect(station_no, "GH")) %>% 
  mutate(site = case_when(
    receiver == "488909" ~ "east",
    receiver == "488919" ~ "east",
    receiver == "488908" ~ "north",
    receiver == "488910" ~ "north",
    receiver == "488921" ~ "north",
    TRUE ~ "other"
    
  ))

winter <- edmc_winter[[2]] %>% 
  filter(station != "0") %>% 
  mutate(perct = V1/sum(V1)*100, 
         szn = "winter") 

summer <- edmc_summer[[2]] %>% 
  filter(station != "0") %>% 
  mutate(perct = V1/sum(V1)*100,
         szn = "summer") 

ed_wide <- left_join(summer, winter, by = "station") %>%
  mutate(diff = perct.x-perct.y) %>% 
  left_join(stations, by = c("station" = "station_no"))

ed <- rbind(winter, summer) 


ed_plot <-ggplot() +
  geom_point(data = ed, 
             aes(x = szn, y = perct, color = station),
             size = 3) +
  geom_segment(data = ed_wide,
               aes(x = szn.x, y = perct.x, xend = szn.y, yend = perct.y, colour = station),
               size = 1.5) +
  theme_bw()


ggplotly(ed_plot)


ggplot(temp %>% filter(site != "other"), aes(x=datetime, y = temp, color = site)) +
  geom_smooth() 



                  



summer <- ggplot() +
  geom_sf(data = na_quman_wgs) +
  geom_point(data = ed_wide, aes(deploy_long, deploy_lat, color = perct.x, size = perct.x)) +
  scale_color_continuous(type = "viridis", limits = c(0,60)) +
  scale_size_continuous(limits = c(0,60)) +
  labs(color = "Transition Probability", size = " Residency Probability") +
  ggtitle("Summer") +
  theme_void()


winter <- ggplot() +
  geom_sf(data = na_quman_wgs) +
  geom_point(data = ed_wide, aes(deploy_long, deploy_lat, color = perct.y, size =5)) +
  scale_color_continuous(type = "viridis", limits = c(0,60)) +
  scale_size_continuous(limits = c(0,60)) +
  ggtitle("Winter") +
  labs(color = "Transition Probability", size = " Residency Probability") +
  theme_void()

summer + winter

ggplot() +
  geom_sf(data = na_quman_wgs) +
  geom_point(data = ed_wide, aes(deploy_long, deploy_lat, color = diff, size = 10)) +
  colorspace::scale_color_continuous_diverging() +
  #scale_color_continuous(type = "viridis", trans = 'reverse') +
  #scale_size_continuous(trans = 'reverse') +
  ggtitle("difference") +
  theme_void()



temp_plot <- temp %>% filter(station_no == "Qu15" | station_no == "North") %>% 
  group_by(date, station_no) %>% 
  dplyr::mutate(daily_avg = mean(temp),
                min_temp = min(temp),
                max_temp = max(temp)) %>% ungroup() %>% 
  distinct(date, station_no, .keep_all = T) #%>% 
  #pivot_wider(names_from = station_no, values_from = max_temp)


max_temp <- temp_plot %>% 
  group_by(date) %>%
  mutate(
    max_value_on_date = max(max_temp),
    hottest_site = ifelse(max_temp == max_value_on_date, station_no, NA_character_)
  ) %>% ungroup() %>% filter(!is.na(hottest_site)) %>% arrange(date) 


ggplot(max_temp, aes(x=date, y = max_value_on_date, color = station_no)) +
         geom_point()

       