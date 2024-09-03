


dets2 <- dets %>% 
  mutate(date = date(detection_timestamp_utc)) %>% 
  distinct(date, transmitter_id, .keep_all = T) %>% 
  filter(tag_activation_date < mdy('01-01-2023'))

  
  ggplot(dets2) +
  geom_point(aes(x = date, y = reorder(transmitter_id, tag_activation_date, decreasing = T)), shape = 20) +
  geom_point(aes(x=tag_activation_date, y = transmitter_id), shape = 4) +
  geom_point(data = filter(dets2, tag_model == "V9"), 
             aes(x = tag_activation_date + days(403), y = transmitter_id), 
             color = "red", shape = 4)
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
  
  
  dets2 %>% 
    ggplot(aes(x=detection_timestamp_utc, y = transmitter_id)) +
    geom_point() +
    facet_wrap(~station_name)

# roll --------------------------------------------------------------------
library(ggplot2)
library(sf)
library(rgdal)
library(rgeos)

  
land_shape <- st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp")  %>% st_transform(crs = 32637)
  
movement <- st_as_sf(df_r, coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(crs = 32637)

intersects_land <- st_intersects(movement, land_shape)

land_points <- movement[unlist(lapply(intersects_land, any)), ]

# Buffer the land shape slightly to simulate a water boundary
# Negative buffer if you want to shrink the land area, positive to expand
# Here, we assume a very small buffer just to create a distinct boundary
land_shape_buffered <- st_buffer(land_shape, dist = -0.001) # Adjust dist based on your CRS and scale

# For each land point, find the nearest point on the buffered land shape
nearest_water_points <- st_nearest_points(land_points, land_shape_buffered)

# Assuming nearest_water_points is a collection of LINESTRING objects
# where each LINESTRING connects a point on land to its nearest water point

# Initialize an empty sf object to store adjusted points
adjusted_points <- st_sfc(crs = st_crs(movement))

# Loop through each LINESTRING to extract the second point
for (i in seq_len(length(nearest_water_points))) {
  line <- nearest_water_points[1, ] # Extract the i-th LINESTRING
  points <- st_cast(line, "POINT") # Cast the LINESTRING to POINTs
  adjusted_point <- points[2, ] # Select the second POINT (nearest water point)
  adjusted_points <- st_sfc(adjusted_points, adjusted_point, crs = st_crs(movement)) # Append to the collection
}

# This step involves replacing the original land points with their adjusted versions
# Ensure you keep track of indices to correctly update the original dataset
movement[unlist(lapply(intersects_land, any)), ] <- adjusted_points


 ggplot() +
  geom_sf(data = land_shape, fill = "gray80", color = "black") +
  geom_sf(data = filter(movement,transmitter_id == "A69-1605-52"), color = "blue", size = 2, alpha = 0.7) +
   xlim(st_bbox(movement)[1],
        st_bbox(movement)[3]) +
   ylim(c(st_bbox(movement)[2],
          st_bbox(movement)[4])) +
  theme_minimal() +
  labs(title = "Adjusted Animal Movements with Land Overlay",
       x = "Longitude", y = "Latitude")

# Assuming 'land_shape' and 'movements' are already prepared sf objects
ggplot() +
  geom_sf(data = land_shape, fill = "gray80", color = "black", size = 0.25) + # Land
  geom_sf(data = movements, color = "blue", size = 1, alpha = 0.7) + # Adjusted Movements
  theme_minimal() +
  labs(title = "Adjusted Animal Movements with Land Overlay",
       x = "Longitude", y = "Latitude")

movement %>% 
filter(transmitter_id == "A69-1605-52") %>% 
  mutate(lag = detection_timestamp_utc - lag(detection_timestamp_utc), default = 0) %>% 
  ggplot(aes(x=lag)) + geom_histogram() +
  scale_y_log10()
