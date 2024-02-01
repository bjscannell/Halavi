date_hour <- seq(ymd_h('2022-11-28 00'),ymd_h('2023-10-7 00'),by='hour')
df <- as.data.frame(date_hour)
df <- df %>% mutate(date_hour = as.character(date_hour))
fish_str <- fish %>% mutate(date_hour = paste0(year(detection_timestamp_utc), "-",
                                         month(detection_timestamp_utc), "-", 
                                         day(detection_timestamp_utc), " ",
                                         hour(detection_timestamp_utc), ":00:00")) %>% 
  select(date_hour, latitude, longitude) 



fish_full <- left_join(df,fish_str, by="date_hour") %>% 
  mutate(latitude = tidyr::replace_na(latitude, 25.551777),
         longitude = tidyr::replace_na(longitude, 36.845606),
         detection_timestamp_utc = ymd_hms(date_hour))


library(sf)
library(ggplot2)
library(gganimate)


data_sf <- st_as_sf(fish_full, coords = c("longitude", "latitude"), crs = 4326)
data_utm <- st_transform(data_sf, crs = 32637)


shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp")


gg <- ggplot() +
  geom_sf(data = shape.data) +
  geom_sf(data = data_utm, size = 2) +  # Plot your data points
  labs(title = "Quman Detections") +
  xlim(st_bbox(data_utm)[1],
       st_bbox(data_utm)[3]) +
  ylim(c(st_bbox(data_utm)[2],
         st_bbox(data_utm)[4]))

anim = gg + 
  transition_time(detection_timestamp_utc) +
  ease_aes('linear')+
  ggtitle("Date: {frame_along}")

animate(anim, nframes = 8455, fps = 100)
