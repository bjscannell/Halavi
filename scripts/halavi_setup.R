library(readr)
library(stringr)
library(janitor)
library(dplyr)
library(lubridate)
library(vroom)


# Check we have all the files ---------------------------------------------
HalaviTaggingMetadata <- read_csv("data/tags/HalaviTaggingMetadata.csv", 
                                  col_types = cols(UTC_RELEASE_DATE_TIME = col_datetime(format = "%m/%d/%y %H:%M"),
                                                   TAG_ACTIVATION_DATE = col_date(format = "%d-%b-%y"))) %>%  clean_names() %>% 
  mutate(year = year(utc_release_date_time),
         end_date = tag_activation_date+est_tag_life) %>% 
  mutate(new_class = case_when(
    length_cm < 40 ~ "YOY",
    length_cm >= 80 ~ "ADULT",
    .default = "JUV"
  )) 


HalaviArray <- read_csv("data/stations/HalaviArray.csv", 
                        col_types = cols(`DEPLOY_DATE_TIME (yyyy-mm-ddThh:mm:ss)` = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         `RECOVER_DATE_TIME (yyyy-mm-ddThh:mm:ss)` = col_datetime(format = "%Y-%m-%d %H:%M:%S")),
                        n_max=103) 


files <- list.files("data/detections/halavi_files")

# Making sure we have all the files
names <- unique(c(str_extract(files[which(str_count(files, " ") == 3)],  "^[^.]*"),
                  str_extract(files[which(str_count(files, " ") == 4)],"^([^ ]* +){3}[^ ]*")))


test <- data.frame(names, x = seq(1, length(names)))

HalaviArray <- HalaviArray %>%
  filter(!is.na(FILENAME)) %>%
  mutate(names = if_else(str_count(FILENAME, " ") == 3,str_extract(FILENAME, "^[^.]*"),
                         str_extract(FILENAME, "^([^ ]* +){3}[^ ]*"))) %>%
  clean_names()

# HalaviArray %>% left_join(test, by = "names") %>% View()



# Join files with station and clip to dates -------------------------------


files <- list.files("data/detections/halavi_files", full.names = T)

raw <- vroom(files, skip = 1, id = "source") %>% clean_names()

dets_raw <- raw %>% filter(record_type == "DET") %>% 
  dplyr::select(field_2, field_7, field_10, source) %>% 
  rename(detection_timestamp = field_2,
         receiver = field_7,
         tag_id = field_10) %>% 
  filter(!str_detect(tag_id, "A69-1601")) %>% 
  mutate(id = str_extract(source,  "VR2Tx-69 .*?(?=\\.)"))


dets <- dets_raw %>% left_join(HalaviArray, by = c("id" = "names")) %>%  
  mutate(detection_timestamp = ymd_hms(detection_timestamp, quiet = FALSE, tz = "UTC")) %>%
  group_by(id) %>% 
  filter(detection_timestamp > deploy_date_time_yyyy_mm_dd_thh_mm_ss &
           detection_timestamp < recover_date_time_yyyy_mm_dd_thh_mm_ss) %>% ungroup() %>% 
  dplyr::select(detection_timestamp, tag_id, receiver, otn_array,
                station_no, deploy_lat, deploy_long, bottom_depth)


# filter for false detections ---------------------------------------------

dets_g <- dets %>% 
  mutate(transmitter_codespace = str_extract(tag_id, "^[^-]*-[^-]*"),
         day = date(detection_timestamp)) %>% 
  rename(detection_timestamp_utc = detection_timestamp,
         transmitter_id = tag_id,
         receiver_sn = receiver)

dets <-  glatos::false_detections(dets_g, tf = 3600) %>% filter(passed_filter == 1) %>% 
  filter(transmitter_id != "A69-1605-73")


# Join with metadata ------------------------------------------------------

dets <- dets %>% left_join(HalaviTaggingMetadata, by = "transmitter_id")

# remove the first line
# dets <- dets %>% 
#   filter(!str_detect(station_no, "GH")) %>% 
#   filter(transmitter_id %in% HalaviTaggingMetadata$transmitter_id)
  

# lets take just the quman tags that are dead
# tags <- HalaviTaggingMetadata %>% 
#   filter(capture_island == "Quman") %>% 
#   #mutate(end_date = tag_activation_date+est_tag_life) %>% 
#   #filter(end_date <= date("2025-01-01")) %>%
#   pull(transmitter_id)

# heres the dataset


dets <- dets %>% ungroup() %>% 
  filter(str_detect(station_no,  paste(c("GH06", "GH14"), collapse = "|")) |
           !str_detect(station_no, "GH")) %>% 
  mutate(station_no = case_when(
    station_no == "GH06" ~ "Qu2",
    station_no == "GH14" ~ "Qu3",
    .default = station_no
  )) %>% 
  filter(transmitter_id %in% HalaviTaggingMetadata$transmitter_id) %>% 
  filter(year(utc_release_date_time) < year("2025-05-27 08:27:38")) %>% 
  filter(new_class != "ADULT") %>% 
  mutate(date = date(detection_timestamp_utc)) %>% 
  arrange(detection_timestamp_utc)



library(ggplot2)
library(sf)
library(cowplot)




# Lengths  ------------------------------------------------------------
n_fun <- function(x){
  return(data.frame(y = 0, 
                    label = paste0("n = ",length(x))))
}

n_med <- function(x){
  return(data.frame(y = quantile(x, prob = 0.25) - 6, 
                    label = paste0("", round(median(x),digits =3))))
}

length_plot <- HalaviTaggingMetadata %>% 
  #filter(transmitter_id %in% tags) %>% 
  ggplot(aes(x=new_class, y = length_cm)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(size = 1.5,alpha = .6,
             position = position_jitter(seed = 1, width = .1)) +
  # scale_x_discrete(name = "Life Stage",
  #                  labels = c("Juvenile", "Young of the Year")) +
  # scale_y_continuous(name = "Length (cm)",
  #                    breaks = seq(25,75,10),
  #                    limits = c(25,75)) +
  stat_summary(geom = "text",fun.data = n_fun, vjust = 7) +
  stat_summary(geom = "text",fun.data = n_med, size = 5, vjust = 3) +
  xlab("\nNew Class") +
  theme_classic() +
  coord_cartesian(clip = "off") +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    text = element_text(size = 10)
  )

ggsave("plots/length_distr.png",length_plot, dpi = 360, width = 10, height = 10, units = "in")




# Abacus plot -------------------------------------------------------------

abacus <- dets %>% 
  mutate(date = date(detection_timestamp_utc)) %>% 
  group_by(transmitter_id, station_no) %>% distinct(date, .keep_all = T) %>% 
  ggplot() +
  geom_point(aes(x = date, y = reorder(transmitter_id, tag_activation_date, decreasing = T), color = otn_array), shape = 20) +
  # geom_point(data = HalaviTaggingMetadata,
  #            aes(x=tag_activation_date, y = transmitter_id), shape = 4, color = "green") +
  # geom_point(data = HalaviTaggingMetadata %>% filter(transmitter_id %in% tags),
  #            aes(x=end_date, y = transmitter_id),color = "red", shape = 4) +
  theme_minimal() +
  scale_x_date(date_labels = "%b-%Y") +
  labs(
    x = "Date",
    y = "Halavi Guitarfish") +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.title.position = "plot", 
        plot.caption.position =  "plot",  
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(vjust=-5),
        axis.title.x = element_text(vjust=0),
        text = element_text(size = 10)
  )

ggsave("plots/abacus.png",abacus, dpi = 360, width = 12, height = 6, units = "in")


# Calculating individual metrics ---------------------------------------------

# consecutive days

transmitter <- unique(dets$transmitter_id)

consec <- list()

for (i in 1:length(unique(dets$transmitter_id))) {
  consec[[i]] <- dets %>% filter(transmitter_id == transmitter[i]) %>%
    distinct(date, .keep_all = T) %>% 
    group_by(grp = cumsum(c(0, diff(date) > 1)), transmitter_id) %>% 
    mutate(ConsecutiveDays = row_number()) %>% ungroup(grp) %>% group_by(transmitter_id) %>% 
    summarise(transmitter_id = first(transmitter_id),
              consec = max(ConsecutiveDays))
}

consec <- dplyr::bind_rows(consec)


# full summary table -----------------------------------------------------------


res <- dets %>% group_by(transmitter_id) %>% 
  summarise(
    transmitter_id = first(transmitter_id),
    ID = first(animal_id_floy_tag_id_pit_tag_code_etc),
    Sex = first(sex),
    DW = first(length2_cm),
    TL = first(length_cm),
    Class = first(new_class),
    Tagging_Island = first(otn_array),
    Deployment_Date = first(tag_activation_date),
    Last_Detection = max(date),
    First_Detection = min(date),
    Total_No_Dets = n(),
    Days_Liberty = as.numeric(first(Last_Detection) - first(First_Detection)) + 1,
    Tag_on = first(tag_activation_date),
    Tag_off = first(tag_activation_date) + days(first(est_tag_life)),
    Days_Monitored = if_else(first(Last_Detection) > first(Tag_off),
                             first(Last_Detection) - first(Tag_on) + 1,
                             first(Tag_off) - first(Tag_on) +1),
    Days_Present = length(unique(date)),
    residency_max = as.numeric(Days_Present) / as.numeric(Days_Liberty),
    residency_min = as.numeric(Days_Present)/ as.numeric(Days_Monitored),
    res_ratio = as.numeric(Days_Liberty)/ as.numeric(Days_Monitored),
    # res_type = case_when(
    #   residency_min >= 0.65 & res_ratio > 0.70 ~ "Resident",
    #   residency_min < 0.65 & res_ratio > 0.40 ~ "Intermittent Resident",
    #   residency_min < 0.5 & res_ratio <= 0.5 ~ "Transient",
    #   TRUE ~ NA_character_),
    n_stations = n_distinct(station_no)) %>% 
  left_join(consec, by = "transmitter_id")



# residency plot ----------------------------------------------------------

scatter <- res %>% 
  ggplot(aes(x=res_ratio, y = residency_min,
             color = res_type)) +
  geom_point(size = 3) +
  scale_color_manual(values=c("#8b0700", "#f3ac00", "#5b8fab")) +
  annotate("text", x = .75, y = .35, 
           label = "Inter-Res", size=4, color = "#8b0700", fontface = "bold") + 
  annotate("text", x = .75, y = .70, 
           label = "Resident", size=4, color = "#f3ac00", fontface = "bold") + 
  annotate("text", x = .08, y = .08, 
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

residency <- ggdraw() +
  draw_plot(scatter) +
  draw_plot(pie,x = 0.11, y = .6, width = .3, height = .4)

ggsave("plots/residency.png",residency, dpi = 360, width = 6, height = 6, units = "in")

# Hotspot Plot ---------------------------------------------------------------------

station_counts <- dets %>% 
  group_by(station_no) %>% 
  mutate(n = n()) %>% 
  distinct(station_no, .keep_all = T)

# Load your shapefile
shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp")

# Replace "dets" with your data frame containing latitude, longitude, and transmitter_id
data_sf <- st_as_sf(station_counts, coords = c("deploy_long", "deploy_lat"), crs = 4326)

# Transform to UTM
data_utm <- st_transform(data_sf, crs = 32637)

stations <- dets %>% distinct(station_no, .keep_all = T) %>% 
  dplyr::select(station_no, deploy_lat, deploy_long) %>%
  st_as_sf( coords = c("deploy_long", "deploy_lat"), crs = 4326) %>% st_transform(crs = 32637)

# Create a ggplot object
ggplot() +
  geom_sf(data = shape.data) +  # Plot your shapefile
  geom_sf(data = data_utm, aes(size = n)) +  # Plot data points with color by transmitter_id
  geom_sf(data = stations, shape = 1, alpha = .5) +
  geom_sf_text(data = stations, aes(label = station_no), vjust = -1, size = 3) +  # Add station labels
  labs(title = "Quman Detections") +
  xlim(st_bbox(data_utm)[1],
       st_bbox(data_utm)[3]) +
  ylim(c(st_bbox(data_utm)[2],
         st_bbox(data_utm)[4])) 



# number of stations by size ----------------------------------------------

dets %>% group_by(transmitter_id) %>% 
  mutate(n_stations = n_distinct(station_no)) %>% 
  distinct(transmitter_id, .keep_all = T) %>% 
  ggplot(aes(x=n_stations, y=length_cm)) +
  geom_point(color = "#639465") +
  geom_smooth(method = "lm", se = F, color = "#a69e90") + 
  scale_y_continuous(name = "Length (cm)",
                     breaks = seq(20,70,10),
                     limits = c(20,70))  +
  scale_x_continuous(name = "Number of Stations Visted",
                     breaks = seq(0,15,5),
                     limits = c(0,15)) +
  theme_bw() 


data <- dets %>% group_by(transmitter_id) %>% 
  mutate(n_stations = n_distinct(station_no)) %>% 
  distinct(transmitter_id, .keep_all = T) %>% 
  dplyr::select(transmitter_id, n_stations, length_cm) 


# Remove missing data
data_clean <- data %>% filter(!is.na(length_cm))

# Spearman's rank correlation
spearman_corr <- cor(data_clean$n_stations, data_clean$length_cm, method = "spearman")
spearman_corr

# Pearson's correlation 
pearson_corr <- cor(data_clean$n_stations, data_clean$length_cm, method = "pearson")
pearson_corr


# Load necessary libraries
library(MASS)

# Poisson regression model
poisson_model <- glm(n_stations ~ length_cm, family = poisson(), data = data_clean)

# Summary of the model
summary(poisson_model)


# Check for overdispersion
dispersion_test <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
dispersion_test

# Plotting the predicted counts from the Poisson model
data_clean$predicted_counts <- predict(poisson_model, type = "response")


station_length <- ggplot(data_clean, aes(x = length_cm, y = n_stations)) +
  geom_point(color = "#486e4a", size = 3) +
  geom_line(aes(y = predicted_counts),  color = "#736a66", linewidth = 1) +
  annotate("text", x = 41, y = 15.3, 
           label = paste(round(summary(poisson_model)$coefficients[,4][2], digits = 8)),
           size=4, color = "black", fontface = "bold") + 
  scale_x_continuous(name = "Length (cm)",
                     breaks = seq(40,70,10),
                     limits = c(40,70))  +
  scale_y_continuous(name = "Number of Stations Visted",
                     breaks = seq(0,15,5),
                     limits = c(0,15.3)) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    text = element_text(size = 10)
  )


ggsave("plots/station_length.png",station_length, dpi = 360, width = 8, height = 6, units = "in")



# season by location ------------------------------------------------------


getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

det_szn <- dets %>% 
  mutate(season = getSeason(detection_timestamp_utc))

shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp") %>% 
  filter(IslandName == "Quman") %>% st_transform(32637)

ggplot(det_szn %>% filter(transmitter_id %in% unique(det_szn$transmitter_id)[1:10])) +
  geom_point(aes(x= deploy_long, y = deploy_lat, color = transmitter_id), position = "jitter") +
  facet_grid(transmitter_id~season)

ggplot(det_szn %>% filter(transmitter_id %in% unique(det_szn$transmitter_id)[11:25])) +
  geom_point(aes(x= deploy_long, y = deploy_lat, color = transmitter_id), position = "jitter") +
  facet_grid(transmitter_id~season)



det_szn %>% group_by(deploy_lat, deploy_long, season, station_no) %>% 
  count() %>% ungroup(deploy_lat, deploy_long, season) %>% 
  filter(n == max(n)) %>% 
  ggplot() +
  #geom_sf(data = shape.data) +
  geom_point(aes(x= deploy_long, y = deploy_lat, color = season))




# Animation ---------------------------------------------------------------

# library(ggplot2)
# library(stringr)
# library(sf)
# library(gganimate)
# library(forcats)
# 
# df_r <- dets %>%
#   filter(date > as.Date('04-27-2023',format ="%m-%d-%Y"))
# 
# sum <- dets %>%
#   filter(date > as.Date('04-27-2023',format ="%m-%d-%Y")) %>% 
#   distinct(transmitter_id, station_name, .keep_all = T) %>%
#   group_by(transmitter_id) %>%
#   summarize(Detected_Stations = paste(station_name, collapse = ', '),
#             length = first(length_m),
#             class = first(life_stage)) %>% 
#   mutate(num_stations = str_count(Detected_Stations, ",")+1) 




# one animated gf ---------------------------------------------------------

library(sf)
library(gganimate)
library(purrr)
library(pathroutr)

fish <- dets %>% arrange(detection_timestamp_utc) %>% filter(transmitter_id == "A69-1605-60") %>% 
  mutate(run = cumsum(c(0, diff(as.integer(factor(station_no))) != 0))) %>% 
  group_by(run) %>%
  slice(1) %>%
  ungroup() %>% 
  filter(date > as.Date('04-27-2023',format ="%m-%d-%Y")) 

data_sf <- st_as_sf(fish, coords = c("deploy_long", "deploy_lat"), crs = 4326)
data_utm <- st_transform(data_sf, crs = 32637)

shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp") %>% 
  filter(IslandName == "Quman") %>% st_transform(32637)

path <- fish %>% dplyr::select(deploy_long, deploy_lat)
path <- SpatialPoints(path, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

path <-  st_as_sf(path)  %>% st_transform(32637)


ggplot() + 
  ggspatial::annotation_spatial(shape.data, fill = "cornsilk3", size = 0) +
  geom_point(data = path, aes(x=unlist(map(geometry,1)), y=unlist(map(geometry,2)))) +
  geom_path(data = path, aes(x=unlist(map(geometry,1)), y=unlist(map(geometry,2))))  +
  geom_sf(data = distinct(data_sf,receiver_sn,.keep_all = T), color = "blue", size = 2, alpha = 0.7) +
  theme_void() 

plot_path <- path %>% summarise(do_union = FALSE) %>% st_cast('LINESTRING')

track_pts <- st_sample(plot_path, size = 10000, type = "regular")

vis_graph <- prt_visgraph(shape.data, buffer = 100)

track_pts_fix <- prt_reroute(track_pts, shape.data, vis_graph, blend = TRUE)

track_pts_fix <- prt_update_points(track_pts_fix, track_pts)

pathroutrplot <- ggplot() + 
  ggspatial::annotation_spatial(shape.data, fill = "cornsilk3", size = 0) +
  geom_point(data = track_pts_fix, aes(x=unlist(map(geometry,1)), y=unlist(map(geometry,2)))) +
  geom_path(data = track_pts_fix, aes(x=unlist(map(geometry,1)), y=unlist(map(geometry,2))))  +
  geom_sf(data = distinct(data_sf,receiver_sn,.keep_all = T), color = "blue", size = 2, alpha = 0.7) +
  theme_void() 

pathroutrplot.animation <-
  pathroutrplot +
  transition_reveal(fid) +
  shadow_mark(past = TRUE, future = FALSE)


# upping the frame rate reduces cross over
gganimate::animate(pathroutrplot.animation, nframes=300, detail=2)


