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
library(ggan)

fish <- dets %>% filter(transmitter_id == "A69-1605-60") %>% 
  mutate(run = cumsum(receiver_sn != lag(receiver_sn, default = first(receiver_sn)))) %>%
  group_by(run) %>%
  slice(1) %>%
  ungroup() %>% 
  filter(date > as.Date('04-27-2023',format ="%m-%d-%Y"))

data_sf <- st_as_sf(fish, coords = c("longitude", "latitude"), crs = 4326)
data_utm <- st_transform(data_sf, crs = 32637)


shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp") %>% st_transform(32637)

path <- fish %>% dplyr::select(longitude, latitude)
path <- SpatialPoints(path, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

path <-  st_as_sf(path)  %>% st_transform(32637)


ggplot() + 
  ggspatial::annotation_spatial(shape.data, fill = "cornsilk3", size = 0) +
  geom_point(data = path, aes(x=unlist(map(geometry,1)), y=unlist(map(geometry,2)))) +
  geom_path(data = path, aes(x=unlist(map(geometry,1)), y=unlist(map(geometry,2))))  +
  theme_void() +
  xlim(st_bbox(data_utm)[1],
       st_bbox(data_utm)[3]) +
  ylim(c(st_bbox(data_utm)[2],
         st_bbox(data_utm)[4]))

plot_path <- path %>% summarise(do_union = FALSE) %>% st_cast('LINESTRING')

track_pts <- st_sample(plot_path, size = 10000, type = "regular")

vis_graph <- prt_visgraph(shape.data, buffer = 100)

track_pts_fix <- prt_reroute(track_pts, shape.data, vis_graph, blend = TRUE)

track_pts_fix <- prt_update_points(track_pts_fix, track_pts)

pathroutrplot <- ggplot() + 
  ggspatial::annotation_spatial(shape.data, fill = "cornsilk3", size = 0) +
  geom_point(data = track_pts_fix, aes(x=unlist(map(geometry,1)), y=unlist(map(geometry,2)))) +
  geom_path(data = track_pts_fix, aes(x=unlist(map(geometry,1)), y=unlist(map(geometry,2))))  +
  theme_void() +
  xlim(st_bbox(data_utm)[1],
       st_bbox(data_utm)[3]) +
  ylim(c(st_bbox(data_utm)[2],
         st_bbox(data_utm)[4]))

pathroutrplot.animation <-
  pathroutrplot +
  transition_reveal(fid) +
  shadow_mark(past = TRUE, future = FALSE)

gganimate::animate(pathroutrplot.animation, nframes=100, detail=2)



gg <- ggplot() +
  geom_sf(data = shape.data) +
  geom_sf(data = data_utm, size = 2) +  
  labs(title = "Quman Detections") +
  xlim(st_bbox(data_utm)[1],
       st_bbox(data_utm)[3]) +
  ylim(c(st_bbox(data_utm)[2],
         st_bbox(data_utm)[4]))

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


# animate plot ------------------------------------------------------------

anim = gg + 
  transition_time(detection_timestamp_utc) +
  ease_aes('linear')+
  ggtitle("Date: {frame_along}")

animate(anim, nframes = 8455, fps = 100)

# ---------------------------------


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




# Length vs. Number of Stations -------------------------------------------
## By sex

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



write_csv(dets, "dets.csv")


dets %>% 
  group_by(transmitter_id) %>% 
  mutate(location_change = cumsum(station_name != lag(station_name, def = first(station_name)))) %>% ungroup() %>% 
  group_by(transmitter_id,location_change) %>% 
  slice(1) %>% ungroup() %>% filter(transmitter_id == "A69-1605-52") %>%  write_csv("dets.csv")
  
# Load required libraries
library(raster)
library(gdistance)

# Read in the raster of the river map
water <- raster("Halavi/Reclass_Feat1.tif")

# Convert to categorical data
rfac <- asFactor(water < 100)

# Compute the transition matrix
rfactr <- transition(rfac, "areas", 8)

# Define the points
x <- c(36.8452, 36.847133)
y <- c(25.572833, 25.535283)

pts <- data.frame(x, y)

start <- matrix(c(pts[1, 1], pts[1, 2]), ncol = 2)
end <-  matrix(c(pts[2, 1], pts[2, 2]), ncol = 2)

from_point_sf <- st_sfc(st_point(start), crs = 4326)
end_point_sf <- st_sfc(st_point(end), crs = 4326)

# Transform the sf data frame to EPSG 32637
from_point_proj <- st_transform(from_point_sf, crs = 32637)
end_point_proj <- st_transform(end_point_sf, crs = 32637)


from_point_sp <- st_as_sf(from_point_proj)
end_point_sp <- st_as_sf(end_point_proj)


path = shortestPath(rfactr, from_point_sp, end_point_sp, output="SpatialLines")


utmcrs <-  CRS("+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs")

plot(water)
plot(from_point_proj, add = TRUE, pch = 16, col = "red")
plot(end_point_proj, add = TRUE, pch = 16, col = "red")


