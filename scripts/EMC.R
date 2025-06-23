# Create sf object of receivers
# receivers <- dets %>%
#   distinct(station_no, deploy_lat, deploy_long) %>%
#   st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>%
#   st_transform(32636) 

# # Cluster receivers within 1 km
# receiver_clusters <- st_join(receivers, st_buffer(receivers, 1000), join = st_intersects) %>%
#   rename(station_no = station_no.x)
#   group_by(station_no) %>%
#   #summarise(geometry = st_union(geometry)) %>%
#   st_centroid() %>%
#   mutate(cluster_id = row_number())
# 
# # Join cluster IDs back to the detection data
# detections <- dets %>%
#   left_join(st_drop_geometry(receiver_clusters), by = "station_no")

data <- dets %>% 
  #mutate(bin = round(detection_timestamp_utc, units = "days")) %>% 
  group_by(transmitter_id, date) %>% slice(1) %>% ungroup() %>% 
  mutate(id = as.factor(transmitter_id),
         time = detection_timestamp_utc,
         state = station_no)
  

em <- sharkov(data)


dets %>% group_by(transmitter_id) %>% filter(station_no != lag(station_no)) %>% View()
