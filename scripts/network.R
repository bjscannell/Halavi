
daily_detect <- dets %>% 
  mutate(date = date(detection_timestamp_utc)) %>% 
  group_by(date, transmitter_id, station_no) %>% 
  summarise(n_det = n())


library(igraph)
library(purrr)
library(dplyr)
library(tidyr)

# Step 1: Get all unique stations and ensure square matrices
all_stations <- unique(daily_detect$station_no)

tr_mat_fixed <- map(tr_mat, function(mat) {
  mat_fixed <- mat %>%
    as_tibble(rownames = "from") %>%
    pivot_longer(-from, names_to = "to", values_to = "n") %>%
    complete(from = all_stations, to = all_stations, fill = list(n = 0)) %>%
    pivot_wider(names_from = to, values_from = n) %>%
    column_to_rownames(var = "from") %>%
    as.matrix()
  return(mat_fixed)
})

# Step 2: Add station coordinates as node attributes
station_coords <- dets %>%
  select(station_no, deploy_long, deploy_lat) %>%
  distinct()

networks <- map(tr_mat_fixed, function(mat) {
  g <- graph_from_adjacency_matrix(mat, mode = "directed", weighted = TRUE)
  
  # Add coordinates as node attributes
  V(g)$station_no <- rownames(mat)  # Add station names to vertices
  V(g)$long <- station_coords$deploy_long[match(rownames(mat), station_coords$station_no)]
  V(g)$lat  <- station_coords$deploy_lat[match(rownames(mat), station_coords$station_no)]
  
  return(g)
})

# Step 3: Example Plot with Spatial Layout
g1 <- networks[[5]]  # First fish network

# Spatial layout using longitude and latitude
layout_coords <- as.matrix(data.frame(V(g1)$long, V(g1)$lat))

plot(g1, 
     layout = layout_coords,  # Use geographic coordinates as layout
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.size = 15,
     edge.width = E(g1)$weight,  # Scale edges by weight
     edge.arrow.size = 0.5,
     main = "Spatial Network for Fish 1")



# combine -----------------------------------------------------------------

library(igraph)
library(purrr)
library(dplyr)
library(tidyr)

# Step 1: Combine all transition matrices into one global matrix
global_tr_mat <- reduce(tr_mat_fixed, `+`)  # Element-wise sum of matrices

# Step 2: Add station coordinates
station_coords <- dets %>%
  select(station_no, deploy_long, deploy_lat) %>%
  distinct()

# Step 3: Convert the global transition matrix into an igraph network
global_network <- graph_from_adjacency_matrix(global_tr_mat, mode = "directed", weighted = TRUE)

# Add station coordinates as node attributes
V(global_network)$station_no <- rownames(global_tr_mat)  # Station names
V(global_network)$long <- station_coords$deploy_long[match(rownames(global_tr_mat), station_coords$station_no)]
V(global_network)$lat  <- station_coords$deploy_lat[match(rownames(global_tr_mat), station_coords$station_no)]

# Step 4: Spatial Plot of the Global Network
layout_coords <- as.matrix(data.frame(V(global_network)$long, V(global_network)$lat))

net <- simplify(global_network, remove.multiple = F, remove.loops = T) 

plot(net, 
     layout = layout_coords,   # Use geographic coordinates for layout
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.size = 15,
     edge.width = E(global_network)$weight / max(E(global_network)$weight) * 5,  # Scale edge thickness
     edge.arrow.size = 0.5,
     main = "Global Network of Fish Transitions")

# heatmap
netm <- as_adjacency_matrix(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$station_no
rownames(netm) <- V(net)$media

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm, Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )
library(igraph)
library(ggraph)
library(tidyverse)

# Step 1: Combine all transition matrices into a single global matrix
global_tr_mat <- reduce(tr_mat_fixed, `+`)  # Combine matrices

# Step 2: Create a graph from the global transition matrix
global_network <- graph_from_adjacency_matrix(global_tr_mat, mode = "directed", weighted = TRUE)

# Add node attributes for geographic coordinates
V(global_network)$long <- station_coords$deploy_long[match(V(global_network)$name, station_coords$station_no)]
V(global_network)$lat <- station_coords$deploy_lat[match(V(global_network)$name, station_coords$station_no)]

# Step 3: Convert nodes into a tibble for coloring or customization
V(global_network)$color <- "skyblue"  # Node color example
E(global_network)$width <- E(global_network)$weight / max(E(global_network)$weight) * 2  # Scale edge width

# Step 4: Plot the graph using ggraph
for (i in 1:length(networks)) {
  ggraph(networks[[i]], layout = "manual", x = V(networks[[i]])$long, y = V(networks[[i]])$lat) +
    # Edges (connections)
    geom_edge_fan(aes(width = weight), color = "gray50", alpha = 0.5) +
    # Nodes (stations)
    geom_node_point(aes(color = "skyblue"), size = 8) +
    # Labels for stations
    geom_node_text(aes(label = name), size = 3, repel = TRUE) +
    # Customize
    scale_edge_width(range = c(0.5, 2), name = "Transition Count") +
    scale_color_manual(values = c("skyblue")) +
    theme_void() +
    labs(title = names(networks[i]),
         subtitle = "Spatial Representation with Station Locations")
}


ggraph(global_network, layout = "manual", x = V(global_network)$long, y = V(global_network)$lat) +
  # Edges (connections)
  geom_edge_fan(aes(width = weight), color = "gray50", alpha = 0.5) +
  # Nodes (stations)
  geom_node_point(aes(color = color), size = 8) +
  # Labels for stations
  geom_node_text(aes(label = name), size = 3, repel = TRUE) +
  # Customize
  scale_edge_width(range = c(0.5, 2), name = "Transition Count") +
  scale_color_manual(values = c("skyblue")) +
  theme_void() +
  labs(title = "Global Transition Network of Fish",
       subtitle = "Spatial Representation with Station Locations")

