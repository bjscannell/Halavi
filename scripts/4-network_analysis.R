

# net ---------------------------------------------------------------------

library(dplyr)
library(lubridate)

# Order detections by animal and time
dets_n <- dets %>%
  arrange(transmitter_id, detection_timestamp_utc) %>%
  group_by(transmitter_id) %>%
  mutate(next_station = lead(station_no),
         next_time = lead(detection_timestamp_utc)) %>%
  filter(station_no != next_station) %>%
  ungroup()

edge_list <- dets_n %>%
  filter(!is.na(next_station)) %>%
  select(transmitter_id, station_no, next_station, sex, life_stage) %>%
  rename(from = station_no, to = next_station)

library(tidygraph)
library(ggraph)

# Create graph object
graph <- as_tbl_graph(edge_list, directed = TRUE)

# Add node metadata (station coordinates)
nodes_df <- dets %>%
  select(station_no, deploy_lat, deploy_long) %>%
  distinct() %>%
  rename(name = station_no)

graph <- graph %>%
  activate(nodes) %>%
  left_join(nodes_df, by = "name")


graph <- graph %>%
  activate(nodes) %>%
  mutate(
    degree_centrality = centrality_degree(mode = "all"),
    betweenness_centrality = centrality_betweenness(),
    eigen_centrality = centrality_eigen()
  ) 



as_tibble(graph, active = "nodes") %>%
  ggplot(aes(x = deploy_long, y = deploy_lat)) +
  geom_point(aes(size = degree_centrality, color = betweenness_centrality), alpha = 0.8) +
  scale_color_viridis_c(option = "D", name = "Betweenness") +
  scale_size(range = c(2, 8), name = "Degree") +
  labs(title = "Map of Node Centrality",
       x = "Longitude", y = "Latitude") +
  theme_minimal()



# random
# Your directed graph with coordinates
graph_obs <- as_tbl_graph(edge_list, directed = TRUE) %>%
  activate(nodes) %>%
  left_join(nodes_df, by = "name") %>%
  mutate(degree = centrality_degree(mode = "all"),
         betweenness = centrality_betweenness(),
         eigen = centrality_eigen())

# Extract observed centrality metrics
df_obs <- as_tibble(graph_obs, active = "nodes") %>%
  select(name, degree, betweenness, eigen) %>%
  mutate(type = "observed")

library(future)
library(furrr)
plan(multisession)  # Or use plan(multicore) on Linux/Mac

# Convert observed graph to igraph
igraph_obs <- graph_obs

# Function to randomize and calculate centralities
randomize_graph_metrics <- function(i) {
  library(dplyr)
  library(tidygraph)
  library(ggraph)
  set.seed(123)
  ig_rand <- rewire(igraph_obs, keeping_degseq(niter = gsize(igraph_obs) * 10))
  tbl_rand <- as_tbl_graph(ig_rand) %>%
    activate(nodes) %>%
    left_join(nodes_df, by = "name") %>%
    mutate(degree = centrality_degree(mode = "all"),
           betweenness = centrality_betweenness(),
           eigen = centrality_eigen()) %>%
    as_tibble() %>%
    select(name, degree, betweenness, eigen) %>%
    mutate(type = paste0("rand_", i))
  
  return(tbl_rand)
}

# Run in parallel with furrr
n_iter <- 100  # Number of randomizations

rand_results <- future_map(1:n_iter, randomize_graph_metrics, .progress = TRUE)
df_rand <- bind_rows(rand_results) %>%
  mutate(type = "random")


df_compare <- bind_rows(df_obs, df_rand)

library(ggplot2)
ggplot(df_compare, aes(x = degree, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Degree Centrality: Observed vs Randomized",
       x = "Degree", y = "Density") +
  theme_minimal()

ggplot(df_compare, aes(x = betweenness, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Betweenness Centrality: Observed vs Randomized",
       x = "Betweenness", y = "Density") +
  theme_minimal()


# Compare mean observed to mean of randomized metrics
obs_mean <- df_obs %>% summarise(across(degree:eigen, mean))
rand_mean <- df_rand %>% summarise(across(degree:eigen, mean))

# Optionally run Wilcoxon or permutation tests
wilcox.test(df_obs$degree, df_rand$degree)
wilcox.test(df_obs$betweenness, df_rand$betweenness)
wilcox.test(df_obs$eigen, df_rand$eigen)

rand_summ <- df_rand %>%
  group_by(name) %>%
  summarise(across(c(degree, betweenness, eigen), list(mean = mean, sd = sd), .names = "{.col}_{.fn}"))

obs_vs_rand <- left_join(df_obs, rand_summ, by = "name") %>%
  mutate(
    degree_z = (degree - degree_mean) / degree_sd,
    betweenness_z = (betweenness - betweenness_mean) / betweenness_sd,
    eigen_z = (eigen - eigen_mean) / eigen_sd
  )



# emdg --------------------------------------------------------------------



transitions <- dets %>%
  arrange(transmitter_id, detection_timestamp_utc) %>%
  group_by(transmitter_id) %>%
  mutate(next_station = lead(station_no)) %>%
  filter(!is.na(next_station) & station_no != next_station) %>%
  ungroup()

transition_counts <- transitions %>%
  count(station_no, next_station, name = "n_transitions")

transition_probs <- transition_counts %>%
  group_by(station_no) %>%
  mutate(prob = n_transitions / sum(n_transitions)) %>%
  ungroup()

library(ggplot2)

ggplot(transition_probs, aes(x = station_no, y = next_station, fill = prob)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Pij") +
  labs(title = "Global Markov Chain: Transition Probabilities",
       x = "From Station", y = "To Station") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Grouping with networks --------------------------------------------------

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





# Seasonal Movements? -----------------------------------------------------

library(lubridate)
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

df <- dets %>% 
  mutate(detection_time = ymd_hms(detection_timestamp_utc),
         year = year(detection_time),
         month = month(detection_time, label = FALSE),
         season  = getSeason(detection_timestamp_utc),
         location = case_when(
           station_no %in% c("Qu3", "Qu2", "Qu1") ~ 'North',
           station_no %in% c("Qu15", "Qu9")  ~ 'East',
           TRUE ~ 'Other'))

df %>%
  group_by(location, month) %>%
  summarise(detections = n()) %>% ungroup() %>% 
  ggplot(aes(x = month, y = detections, color = location)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Monthly Detection Counts by Hotspot",
       x = "Month", y = "Number of Detections")       

df %>%
  group_by(location, new_class) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = new_class, y = count, fill = location)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  labs(title = "Size Class Distribution Across Hotspots",
       x = "Size Class", y = "Count")

# get movement from one to another
df_filtered <- df %>%
  filter(location %in% c("North", "East"))

df_movements <- df_filtered %>%
  arrange(transmitter_id, detection_time) %>%
  group_by(transmitter_id) %>%
  mutate(next_location = lead(location), # Get the next location
         next_time = lead(detection_time)) %>%
  ungroup() %>%
  filter((location == "North" & next_location == "East") | 
           (location == "East" & next_location == "North"))

movement_summary <- df_movements %>%
  group_by(transmitter_id) %>%
  summarise(movement_count = n(),
            first_detection = min(detection_time),
            last_detection = max(detection_time),
            life_stage = first(life_stage),
            .groups = 'drop')

# Plotting the number of movements over time
df_movements %>%
  mutate(movement_date = date(detection_time)) %>%
  group_by(movement_date) %>%
  summarise(movement_count = n()) %>%
  ggplot(aes(x = movement_date, y = movement_count)) +
  geom_path() +
  theme_minimal() +
  labs(title = "Daily Movement Counts Between North and East",
       x = "Date", y = "Number of Movements")


library(tidyverse)
library(lubridate)
library(sf)         
library(brms)       

df_movements <- df_filtered %>%
  arrange(transmitter_id, detection_time) %>%
  group_by(transmitter_id) %>%
  mutate(movement_occurred = ifelse(location != lag(location) & 
                                      location %in% c("North", "East"), 1, 0)) %>%
  ungroup()

# Create a new data frame that includes only the necessary variables for modeling
movement_data <- df_movements %>%
  filter(!is.na(movement_occurred)) %>%
  dplyr::select(transmitter_id, movement_occurred, season, life_stage)

# Run a Bayesian logistic regression model
movement_model <- brm(
  formula = movement_occurred ~ season + life_stage + (1 | transmitter_id),
  family = bernoulli(),
  data = movement_data,
  prior = c(set_prior("normal(0, 1)", class = "b")),
  iter = 4000, chains = 4
)


summary(movement_model)

# Plot posterior estimates
plot(movement_model)

# Posterior predictive checks
pp_check(movement_model)

