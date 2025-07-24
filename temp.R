temp_rec <- raw %>% filter(record_type == "TEMP") %>% select(field_2,field_7, field_8) %>% 
  mutate(datetime = ymd_hms(field_2),
         receiver = field_7,
         temp = as.numeric(field_8)) %>% 
  drop_na()
# over the daily quantiles??

  
  
  temp_rec %>% filter(temp >)


ggplot(temp_rec %>% filter(receiver == "488907") %>% 
         filter(date("2023-07-24 10:11:33 EDT") == date(datetime))) +
  aes(datetime, temp) + geom_point()


