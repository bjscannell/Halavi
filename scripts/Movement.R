library(ggplot2)
library(stringr)
library(sf)
library(gganimate)
library(forcats)

df_r <- dets %>%
  filter(date > as.Date('04-27-2023',format ="%m-%d-%Y"))

sum <- dets %>%
  filter(date > as.Date('04-27-2023',format ="%m-%d-%Y")) %>% 
  distinct(transmitter_id, station_name, .keep_all = T) %>%
  group_by(transmitter_id) %>%
  summarize(Detected_Stations = paste(station_name, collapse = ', '),
            length = first(length_m),
            class = first(life_stage)) %>% 
  mutate(num_stations = str_count(Detected_Stations, ",")+1) 




# one animated gf ---------------------------------------------------------

library(sf)

fish <- dets %>% filter(transmitter_id == "A69-1605-60")

data_sf <- st_as_sf(fish, coords = c("longitude", "latitude"), crs = 4326)
data_utm <- st_transform(your_data_sf, crs = 32637)


shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp")


gg <- ggplot() +
  geom_sf(data = shape.data) +
  geom_sf(data = data_utm, size = 2) +  # Plot your data points
  labs(title = "Quman Detections") +
  xlim(st_bbox(your_data_utm)[1],
       st_bbox(your_data_utm)[3]) +
  ylim(c(st_bbox(your_data_utm)[2],
         st_bbox(your_data_utm)[4]))

anim = gg + 
  transition_time(detection_timestamp_utc) +
  ease_aes('linear')+
  ggtitle("Date: {frame_along}")

animate(anim, nframes = 365, fps = 10)



# everyones locations ---------------------------------------------------------------

# Load your shapefile
shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp")

# Replace "dets" with your data frame containing latitude, longitude, and transmitter_id
data_sf <- st_as_sf(df_r, coords = c("longitude", "latitude"), crs = 4326)

# Transform to UTM
data_utm <- st_transform(data_sf, crs = 32637)

stations <- df_r %>% distinct(station_name, .keep_all = T) %>% 
  select(station_name, latitude, longitude) %>%
  st_as_sf( coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(crs = 32637)

# Create a ggplot object
gg <- ggplot() +
  geom_sf(data = shape.data) +  # Plot your shapefile
  geom_sf(data = data_utm, aes(color = sex), size = 1) +  # Plot data points with color by transmitter_id
  geom_sf(data = stations, shape = 1, alpha = .5) +
  labs(title = "Quman Detections") +
  xlim(st_bbox(data_utm)[1],
       st_bbox(data_utm)[3]) +
  ylim(c(st_bbox(data_utm)[2],
         st_bbox(data_utm)[4])) +
  facet_wrap(~ transmitter_id, nrow = 5)  

gg


# Length vs. Number of Stations -------------------------------------------

dets %>%
  filter(date > as.Date('04-27-2023',format ="%m-%d-%Y")) %>% 
  distinct(transmitter_id, station_name, .keep_all = T) %>%
  group_by(transmitter_id) %>%
  summarize(Detected_Stations = paste(station_name, collapse = ', '),
            length = first(length_m),
            class = first(life_stage),
            sex = first(sex)) %>% 
  mutate(num_stations = str_count(Detected_Stations, ",")+1) %>%
  ggplot(aes(x = length,
             y = num_stations,
             color = sex)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  facet_wrap(~sex) +
  theme_bw()



