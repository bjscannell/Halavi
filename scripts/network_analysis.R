fish <- dets %>% filter(transmitter_id == "A69-1605-52")

library(dplyr)
library(lubridate)
library(sf)
library(sfnetworks)
library(igraph)

# Convert detection timestamps to hourly intervals
detections <- fish %>%
  mutate(hour_bin = floor_date(detection_timestamp_utc, unit = "hour"))


# Create sf object of receivers
receivers <- detections %>%
  distinct(station_no, deploy_lat, deploy_long) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>%
  st_transform(32636)  # Use your appropriate UTM zone

# Cluster receivers within 1 km
receiver_clusters <- st_join(receivers, st_buffer(receivers, 1000), join = st_intersects) %>%
  rename(station_no = station_no.x) %>% 
  group_by(station_no) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_centroid() %>%
  mutate(cluster_id = row_number())

# Join cluster IDs back to the detection data
detections <- detections %>%
  left_join(st_drop_geometry(receiver_clusters), by = "station_no")

# One-hot presence per hour per node
presence_matrix <- detections %>%
  group_by(transmitter_id, hour_bin, cluster_id) %>%
  summarise(present = 1, .groups = "drop") %>%
  pivot_wider(names_from = cluster_id, values_from = present, values_fill = 0)


# Arrange data for each shark
transitions <- detections %>%
  arrange(transmitter_id, hour_bin) %>%
  group_by(transmitter_id) %>%
  mutate(next_node = lead(cluster_id),
         next_time = lead(hour_bin),
         delta = as.numeric(difftime(next_time, hour_bin, units = "hours"))) %>%
  filter(delta == 1)  # Only keep 1-hour transitions


transition_counts <- transitions %>%
  count(cluster_id, next_node) %>%
  filter(!is.na(cluster_id) & !is.na(next_node))

# Convert to matrix format
transition_matrix <- transition_counts %>%
  pivot_wider(names_from = next_node, values_from = n, values_fill = 0) %>%
  column_to_rownames("cluster_id") %>% dplyr::select(-transmitter_id) %>% 
  as.matrix() 


# Normalize to transition probabilities (row stochastic)
P <- transition_matrix / rowSums(transition_matrix)

# Dominant eigenvector
eigen_vec <- eigen(t(P))$vectors[,1]
stationary_dist <- Re(eigen_vec / sum(eigen_vec))  # Normalize

dom_eig <- cbind(receiver_clusters, stationary_dist)


library(sfnetworks)

# Create edge list from transitions
edges <- transitions %>%
  count(cluster_id, next_node, name = "weight")

nodes_sf <- receiver_clusters %>%
  mutate(cluster_id = as.character(cluster_id))  # Ensure match with edge list

# Create network
net <- as_sfnetwork(nodes_sf, directed = TRUE) %>%
  activate("edges") %>%
  left_join(edges, by = c("from" = "cluster_id", "to" = "next_node"))

# Convert to igraph for centrality
g <- as.igraph(net)

# Eigenvector centrality
centrality <- eigen_centrality(g, weights = E(g)$weight)$vector
V(g)$centrality <- centrality


net_sf <- net %>% 
  st_as_sf()

g_df <- igraph::as_data_frame(g)
g_df$centrality <- centrality


plot(net, cex = 2, lwd = 2, main = "Original geometries")

net <- net %>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness(weights = weight, directed = FALSE))


net %>%
  activate("edges") %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf()


net = net %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness())

shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp")

ggplot() +
  #geom_sf(data = shape.data %>% filter(IslandName == "Quman")) +
  geom_sf(data = st_as_sf(net, "edges"), aes(color = weight)) +
  geom_sf(data = st_as_sf(net, "nodes"), aes(size = bc)) +
  ggtitle("Betweenness centrality in Münster Roxel") 


# gh ----------------------------------------------------------------------

detections <- dets %>%
  mutate(hour_bin = floor_date(detection_timestamp_utc, unit = "hour"))


# Create sf object of receivers
receivers <- detections %>%
  distinct(station_no, deploy_lat, deploy_long) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>%
  st_transform(32636)  # Use your appropriate UTM zone

# Cluster receivers within 1 km
receiver_clusters <- st_join(receivers, st_buffer(receivers, 1000), join = st_intersects) %>%
  rename(station_no = station_no.x) %>% 
  group_by(station_no) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_centroid() %>%
  mutate(cluster_id = row_number())

# Join cluster IDs back to the detection data
detections <- detections %>%
  left_join(st_drop_geometry(receiver_clusters), by = "station_no")

# One-hot presence per hour per node
presence_matrix <- detections %>%
  group_by(transmitter_id, hour_bin, cluster_id) %>%
  summarise(present = 1, .groups = "drop") %>%
  pivot_wider(names_from = cluster_id, values_from = present, values_fill = 0)

transitions_grouped <- detections %>%
  arrange(transmitter_id, hour_bin) %>%
  group_by(transmitter_id) %>%
  mutate(next_node = lead(cluster_id),
         next_time = lead(hour_bin),
         delta = as.numeric(difftime(next_time, hour_bin, units = "hours"))) %>%
  filter(delta == 1) %>%
  left_join(detections %>% select(transmitter_id, sex, life_stage) %>% distinct(),
            by = "transmitter_id") %>%
  filter(!is.na(sex.x), !is.na(life_stage.x))


library(sfnetworks)
library(purrr)

# Create edges for each group
edges_by_group <- transitions_grouped %>%
  count(sex.x, life_stage.x, cluster_id, next_node, name = "weight")

# Function to build sfnetwork for a group
build_network <- function(group_edges, node_data) {
  net <- as_sfnetwork(node_data, directed = TRUE) %>%
    activate("edges") %>%
    left_join(group_edges, by = c("from" = "cluster_id", "to" = "next_node"))
  
  return(net)
}

# Create list of grouped edges
group_nets <- edges_by_group %>%
  group_split(sex.x, life_stage.x) %>%
  map(~ build_network(.x, receiver_clusters %>% mutate(cluster_id = as.character(cluster_id))))


group_labels <- edges_by_group %>%
  distinct(sex.x, life_stage.x) %>%
  mutate(group_id = row_number())


library(ggraph)

# Pick a group to visualize
net <- group_nets[[1]]  # e.g., Males - YOY
label <- group_labels[1, ]

# Convert node positions
node_coords <- st_coordinates(receiver_clusters)
V(as.igraph(net))$x <- node_coords[, 1]
V(as.igraph(net))$y <- node_coords[, 2]

ggraph(net, layout = "manual", x = V(net)$x, y = V(net)$y) +
  geom_edge_link(aes(width = weight), alpha = 0.5, color = "gray40") +
  geom_node_point(size = 4, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  labs(title = paste("Movement Network -", label$sex, label$life_stage)) +
  theme_void()

library(dplyr)
library(sfnetworks)
library(purrr)
library(ggraph)

# Transitions without time filtering (just consecutive detections)
transitions_grouped <- dets %>%
  arrange(transmitter_id, detection_timestamp_utc) %>%
  group_by(transmitter_id) %>%
  mutate(next_node = lead(station_no)) %>%
  filter(!is.na(next_node)) 
# Count movements (edges) by sex and life stage
edges_by_group <- transitions_grouped %>%
  count(sex, life_stage, station_no, next_node, name = "weight")

# Build network function using deploy lat/long from receiver_clusters as node coords
build_network <- function(group_edges, node_data) {
  # Prepare nodes sf object with cluster_id and geometry (from deploy lat/long)
  nodes <- node_data %>%
    distinct(station_no, deploy_long, deploy_lat) %>%
    mutate(cluster_id = as.character(cluster_id)) %>%
    sf::st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326)
  
  net <- sfnetworks::sfnetwork(nodes, group_edges, directed = TRUE) %>%
    activate("edges") %>%
    mutate(weight = coalesce(weight, 0))
  
  return(net)
}

# Create list of grouped edges and corresponding networks
group_nets <- edges_by_group %>%
  group_split(sex, life_stage) %>%
  map(~ build_network(.x, receiver_clusters))

group_labels <- edges_by_group %>%
  distinct(sex, life_stage) %>%
  mutate(group_id = row_number())

# Example: Visualize the first group network
net <- group_nets[[1]]
label <- group_labels[1, ]

# Extract node coordinates for plotting
node_coords <- sf::st_coordinates(sfnetwork::activate(net, "nodes") %>% sf::st_as_sf())

igraph_net <- sfnetwork::as.igraph(net)
V(igraph_net)$x <- node_coords[,1]
V(igraph_net)$y <- node_coords[,2]

ggraph(igraph_net, layout = "manual", x = V(igraph_net)$x, y = V(igraph_net)$y) +
  geom_edge_link(aes(width = weight), alpha = 0.5, color = "gray40") +
  geom_node_point(size = 4, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  labs(title = paste("Movement Network -", label$sex, label$life_stage)) +
  theme_void()


