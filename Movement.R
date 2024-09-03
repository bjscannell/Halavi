library(pathroutr)
library(dplyr)
library(ggplot2)
library(sf)
library(ggspatial)

shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp") %>% st_transform(32637) %>% filter(IslandName == "Quman")

fish <- dets %>% filter(transmitter_id == "A69-1605-60") %>% 
  mutate(run = cumsum(receiver_sn != lag(receiver_sn, default = first(receiver_sn)))) %>%
  group_by(run) %>%
  slice(1) %>%
  ungroup() %>% 
  filter(date > as.Date('04-27-2023',format ="%m-%d-%Y")) %>% select(latitude,longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(crs = 32637) %>% st_cast("MULTIPOINT")

receivers <-dets %>% distinct(station_name, .keep_all = T) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(crs = 32637)

#test
ggplot() + 
  ggspatial::annotation_spatial(data = shape.data, 
                                fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(data = fish) +
  theme_void() 

# look at the first 15 points and how they connect
l_pts_m <- fish %>%  slice_sample(n=20)
path_m <- l_pts_m %>% summarise(do_union = FALSE) %>% st_cast('LINESTRING')

ggplot() + 
  ggspatial::annotation_spatial(shape.data, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(fish) +
  ggspatial::layer_spatial(path_m, color = "deepskyblue3") +
  ggspatial::layer_spatial(l_pts_m[1,], color = "darkgreen", size = 4) +
  #ggspatial::layer_spatial(l_pts_m[15,], color = "darkred", size = 4) +
  theme_void()

# sample 10000 points along this string to represent simulated path
track_pts_m <- st_sample(path_m, size = 10000, type = "regular")

ggplot() + 
  ggspatial::annotation_spatial(shape.data, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(path_m, color = "deepskyblue3") +
  ggspatial::layer_spatial(track_pts_m) +
  theme_void()

# identify all the consecutive track points that intersect with land. Out put
segs_tbl_m <- get_barrier_segments(track_pts_m,shape.data)
segs_tbl_m

# visgraph
vis_graph_m <- prt_visgraph(shape.data, buffer = 150)

# check the linstrings
segs_tbl_m <- segs_tbl_m %>% prt_shortpath(vis_graph_m, blend = TRUE)

ggplot() + 
  ggspatial::annotation_spatial(shape.data, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(segs_tbl_m$geometry, color = "deepskyblue3") +
  theme_void()

# update points 
track_pts_fix_m <- prt_reroute(track_pts_m, shape.data, vis_graph_m, blend = TRUE)

track_pts_fix_m <- prt_update_points(track_pts_fix_m, track_pts_m)

# Check final plot
track_pts_m <- track_pts_m %>% st_cast('LINESTRING')
track_line_fixed_m <- track_pts_fix_m %>% summarise(do_union = FALSE) %>% st_cast('LINESTRING')


ggplot() + 
  ggspatial::annotation_spatial(shape.data, fill = "cornsilk3", size = 0) +
  #ggspatial::layer_spatial(track_pts_m, color = "red3") +
  ggspatial::layer_spatial(segs_tbl_m$geometry, color = "deepskyblue3", size = 2) +
  ggspatial::layer_spatial(track_line_fixed_m) +
  ggspatial::layer_spatial(l_pts_m[1,], color = "darkgreen", size = 4) +
  ggspatial::layer_spatial(l_pts_m[15,], color = "darkred", size = 4) +
  ggspatial::layer_spatial(receivers, color = "pink") +
  theme_void()


pathroutrplot <- ggplot() + 
  ggspatial::annotation_spatial(shape.data, fill = "cornsilk3", size = 0) +
  geom_point(data = track_pts_fix_m, aes(x=unlist(map(geometry,1)), y=unlist(map(geometry,2)))) +
  geom_path(data = track_pts_fix_m, aes(x=unlist(map(geometry,1)), y=unlist(map(geometry,2))))  +
  theme_void()


pathroutrplot.animation <-
  pathroutrplot +
  transition_reveal(fid) +
  shadow_mark(past = TRUE, future = FALSE)

gganimate::animate(pathroutrplot.animation, nframes=100, detail=2)
