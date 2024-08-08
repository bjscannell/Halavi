library(ggplot2)
library(sf)

# Constraining all the data to the first 17 tags --------------------------

dets_og <- dets %>% 
  filter(tag_activation_date < as.Date('01-27-2023',format ="%m-%d-%Y")) %>% 
  mutate(date = date(detection_timestamp_utc))



# Lengths  ------------------------------------------------------------
n_fun <- function(x){
  return(data.frame(y = 0, 
                    label = paste0("n = ",length(x))))
}

n_med <- function(x){
  return(data.frame(y = quantile(x, prob = 0.25) - 6, 
                    label = paste0("", round(median(x),digits =3))))
}

HalaviTaggingMetadata %>% 
  filter(utc_release_date_time < as.Date('01-27-2023',format ="%m-%d-%Y")) %>% 
  ggplot(aes(x=life_stage, y = length_m)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(size = 1.5,alpha = .6,
             position = position_jitter(seed = 1, width = .1)) +
  scale_x_discrete(name = "Life Stage",
                   labels = c("Juvenile", "Young of the Year")) +
  scale_y_continuous(name = "Length (cm)",
                   breaks = seq(25,75,10),
                   limits = c(25,75)) +
  stat_summary(geom = "text",fun.data = n_fun, vjust = 5) +
  stat_summary(geom = "text",fun.data = n_med) +
  theme_classic() +
  coord_cartesian(clip = "off") 
  
  


# Abacus plot -------------------------------------------------------------

dets_og %>% 
  group_by(transmitter_id) %>% distinct(date, .keep_all = T) %>% 
  ggplot() +
  geom_point(aes(x = date, y = reorder(transmitter_id, tag_activation_date, decreasing = T)), shape = 20) +
  geom_point(aes(x=tag_activation_date, y = transmitter_id), shape = 4) +
  geom_point(data = filter(dets_og, tag_model == "V9"), 
             aes(x = tag_activation_date + days(403), y = transmitter_id), 
             color = "red", shape = 4) +
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



# Calculating individual metrics ---------------------------------------------

# consecutive days

transmitter <- unique(dets_og$transmitter_id)

consec <- list()

for (i in 1:length(unique(dets_og$transmitter_id))) {
  consec[[i]] <- dets_og %>% filter(transmitter_id == transmitter[i]) %>%
    distinct(date, .keep_all = T) %>% 
    group_by(grp = cumsum(c(0, diff(date) > 1)), transmitter_id) %>% 
    mutate(ConsecutiveDays = row_number()) %>% ungroup(grp) %>% group_by(transmitter_id) %>% 
    summarise(transmitter_id = first(transmitter_id),
              consec = max(ConsecutiveDays))
}

consec <- dplyr::bind_rows(consec)


# full summary table -----------------------------------------------------------


res <- dets_og %>% group_by(transmitter_id) %>% 
  summarise(
    transmitter_id = first(transmitter_id),
    ID = first(animal_id_floy_tag_id_pit_tag_code_etc),
    Sex = first(sex),
    DW = first(length2_m),
    TL = first(length_m),
    Class = first(life_stage),
    Deployment_Date = first(tag_activation_date),
    Last_Detection = max(date),
    Total_No_Dets = n(),
    Days_Liberty = max(date) - min(date) + 1,
    Days_Monitored = as.numeric(mdy('02-15-2024') - first(tag_activation_date)) + 1, # change to pull from sheet
    Days_Present = length(unique(date)),
    residency_max = as.numeric(Days_Present) / as.numeric(Days_Liberty),
    residency_min = as.numeric(Days_Present)/ as.numeric(Days_Monitored),
    res_ratio = as.numeric(Days_Liberty)/ as.numeric(Days_Monitored),
    res_type = case_when(
      residency_min >= 0.5 & res_ratio > 0.50 ~ "Resident",
      residency_min < 0.6 & res_ratio > 0.40 ~ "Intermittent Resident",
      residency_min < 0.5 & res_ratio <= 0.5 ~ "Transient",
      TRUE ~ NA_character_),
    n_stations = n_distinct(station_name)) %>% 
  left_join(consec, by = "transmitter_id")


# residency plot ----------------------------------------------------------

scatter <- res %>% 
  ggplot(aes(x=res_ratio, y = residency_min,
             color = res_type)) +
  geom_point(size = 3) +
  scale_color_manual(values=c("#8b0700", "#f3ac00", "#5b8fab")) +
  annotate("text", x = .7, y = .35, label = "Inter-Res", size=4, color = "#8b0700", fontface = "bold") + 
  annotate("text", x = .75, y = .75, 
                label = "Resident", size=4, color = "#f3ac00", fontface = "bold") + 
  annotate("text", x = .15, y = .15, 
                label = "Transient", size=4, color = "#5b8fab", fontface = "bold") + 
  xlab(bquote(RI[min]/RI[max]))+ 
  ylab(bquote(RI[min])) +
  scale_shape_manual(labels = c('Juvenile', 'Young of the Year'),values = factor(c('0', '1')),
                     name = 'Life Stafe') +
  guides(color = "none") +
  theme_bw()

  

pie <-res %>% count(res_type) %>% 
ggplot(aes(x="", y=n, fill=res_type)) +
  geom_bar(width = 1, stat = "identity") + coord_polar("y") +
  scale_fill_manual(values=c("#8b0700", "#f3ac00", "#5b8fab")) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = n), size=5, color = "white", fontface = "bold") + 
  theme_void() +
  theme(legend.position = "none")

ggdraw() +
  draw_plot(scatter) +
  draw_plot(pie,x = 0.07, y = .58, width = .3, height = .4)

# Hotspot Plot ----------------------------coord_polar()# Hotspot Plot ------------------------------------------------------------

station_counts <- dets_og %>% 
  filter(utc_release_date_time < as.Date('04-27-2023',format ="%m-%d-%Y")) %>% 
  group_by(station_name) %>% 
  mutate(n = n()) %>% 
  distinct(station_name, .keep_all = T)

# Load your shapefile
shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp")

# Replace "dets" with your data frame containing latitude, longitude, and transmitter_id
data_sf <- st_as_sf(station_counts, coords = c("longitude", "latitude"), crs = 4326)

# Transform to UTM
data_utm <- st_transform(data_sf, crs = 32637)

stations <- dets_og %>% distinct(station_name, .keep_all = T) %>% 
  select(station_name, latitude, longitude) %>%
  st_as_sf( coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(crs = 32637)

# Create a ggplot object
ggplot() +
  geom_sf(data = shape.data) +  # Plot your shapefile
  geom_sf(data = data_utm, aes(size = n)) +  # Plot data points with color by transmitter_id
  geom_sf(data = stations, shape = 1, alpha = .5) +
  labs(title = "Quman Detections") +
  xlim(st_bbox(data_utm)[1],
       st_bbox(data_utm)[3]) +
  ylim(c(st_bbox(data_utm)[2],
         st_bbox(data_utm)[4])) 



# number of stations by size ----------------------------------------------

dets_og %>% group_by(transmitter_id) %>% 
  mutate(n_stations = n_distinct(station_name)) %>% 
  distinct(transmitter_id, .keep_all = T) %>% 
  ggplot(aes(x=n_stations, y=length_m)) +
  geom_point(color = "#639465") +
  geom_smooth(method = "lm", se = F, color = "#a69e90") + 
  scale_y_continuous(name = "Length (cm)",
                     breaks = seq(20,70,10),
                     limits = c(20,70))  +
  scale_x_continuous(name = "Number of Stations Visted",
                     breaks = seq(0,15,5),
                     limits = c(0,15)) +
  theme_bw() 
  

data <- dets_og %>% group_by(transmitter_id) %>% 
  mutate(n_stations = n_distinct(station_name)) %>% 
  distinct(transmitter_id, .keep_all = T) %>% 
  select(transmitter_id, n_stations, length_m) 


# Remove missing data
data_clean <- data %>% filter(!is.na(length_m))

# Spearman's rank correlation
spearman_corr <- cor(data_clean$n_stations, data_clean$length_m, method = "spearman")
spearman_corr

# Pearson's correlation (if appropriate)
pearson_corr <- cor(data_clean$n_stations, data_clean$length_m, method = "pearson")
pearson_corr


# Load necessary libraries
library(MASS)

# Poisson regression model
poisson_model <- glm(n_stations ~ length_m, family = poisson(), data = data_clean)

# Summary of the model
summary(poisson_model)


# Check for overdispersion
dispersion_test <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
dispersion_test

# Plotting the predicted counts from the Poisson model
data_clean$predicted_counts <- predict(poisson_model, type = "response")


ggplot(data_clean, aes(x = length_m, y = n_stations)) +
  geom_point() +
  geom_line(aes(y = predicted_counts), color = "blue") +
  labs(title = "Poisson Regression",
       x = "Length (m)",
       y = "station count")
