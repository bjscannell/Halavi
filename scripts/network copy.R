df <- dets %>% 
  mutate(station_no = if_else(station_no == "Qu3" | station_no == "Qu2", 
                              "north beach",
                               station_no)) %>% 
  dplyr::count(station_no, transmitter_id) %>% 
  spread(station_no, n) %>% replace(is.na(.), 0) %>% 
  tibble::column_to_rownames(var = "transmitter_id") %>% 
  as.matrix()



library(igraph)

bg = igraph::graph_from_biadjacency_matrix(df)
bg



# See the vertex attributes 
V(bg)$type 
V(bg)$name

# Plot the network
shape = ifelse(V(bg)$type, "circle", "square") # assign shape by node type
col = ifelse(V(bg)$type, "red", "yellow") # assign color by node type


c1 <- cluster_fast_greedy(
  bg,
  merges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  weights = NULL
)


B = modularity_matrix(bg, membership(c1))
round(B[1,],2)

mem <- membership(c1)
data <- data.frame(names(mem), as.numeric(mem))



plot(c1, bg)

plot_dendrogram(c1)


x <- left_join(data, HalaviTaggingMetadata, by = c("names.mem." = "transmitter_id"))


y <-  dets %>% 
  mutate(station_no = if_else(station_no == "Qu3" | station_no == "Qu2", 
                              "north beach",
                              station_no)) %>% ungroup() %>% 
  distinct(station_no, .keep_all = T) %>% 
  left_join(data,  by = c("station_no" ="names.mem."))

ggplot(x) +
  geom_point(aes(names.mem., life_stage, color =as.factor(as.numeric.mem.)))

ggplot(y) +
  geom_point(aes(deploy_long, deploy_lat, color =as.factor(as.numeric.mem.)))
