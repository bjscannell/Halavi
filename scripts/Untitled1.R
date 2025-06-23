

ggplot(dets) +
  geom_point(aes(x=detection_timestamp_utc, y=transmitter_id, color = station_no), alpha = 0.6)

td <- dets %>% 
  arrange(detection_timestamp_utc) %>% 
  group_by(transmitter_id) %>% 
  mutate(time_diff = as.numeric(difftime(detection_timestamp_utc, lag(detection_timestamp_utc), units = "mins")))  




# Load libraries
library(dplyr)
library(igraph)
library(lubridate)
library(tidyr)
library(sna) 

library(glatos)

test <- dets %>% 
  rename(animal_id = transmitter_id)  %>% 
  mutate(station_no2 = ifelse(station_no == "Qu2" |station_no == "Qu3", "Qu23", station_no))


detections <- detection_events(test, location_col = "station_no2", time_sep = 3600) 


# inter-station movement matrix per individual

# Detect reef-to-reef movements per individual
movements <- detections %>%
  arrange(animal_id, first_detection) %>%
  group_by(animal_id) %>%
  mutate(prev_reef = lag(location)) %>%
  filter(!is.na(location) & prev_reef != location) %>%
  count(animal_id, prev_reef, location, name = "n_moves")

# create weighted directed graphs 

# List of graphs per shark
reef_graphs <- movements %>%
  group_by(animal_id) %>%
  group_split() %>%
  setNames(unique(movements$animal_id)) %>% 
  lapply(function(df) {
    graph_from_data_frame(df %>% select(from = prev_reef, to = location, weight = n_moves),
                          directed = TRUE)
  })



# Create metrics

# Compute centrality metrics for one example individual

calc_metrics <- function(g) {
  
  strength <- strength(g, mode = "all", loops = FALSE)
  closeness_cent <- igraph::closeness(g)
  eigen_cent <- eigen_centrality(g, directed = TRUE, weights = E(g)$n_moves)$vector
  
  return(c(strength = strength, closeness_cent = closeness_cent, eigen_cent = eigen_cent))
  
}


n_boot <- 1000  # Number of bootstrap replicates

boot_metrics <- replicate(n_boot, {
  g_rand <- rewire(g_obs, keeping_degseq(niter = gsize(g_obs) * 10))
  calculate_metrics(g_rand)
}, simplify = "matrix")

# Transpose for easier handling
boot_metrics <- t(boot_metrics)
colnames(boot_metrics) <- names(obs_metrics)

  



n_boot <- 1000  # Number of bootstrap replicates

boot_metrics <- replicate(n_boot, {
  g_rand <- rewire(g, keeping_degseq(niter = gsize(g) * 10))
  strength <- strength(g_rand, mode = "all", loops = FALSE)
  closeness_cent <- igraph::closeness(g_rand)
}, simplify = "matrix")

# Transpose for easier handling
boot_metrics <- t(boot_metrics)
colnames(boot_metrics) <- names(obs_metrics)
