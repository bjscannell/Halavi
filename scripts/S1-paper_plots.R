
# Maps --------------------------------------------------------------------

library(ggspatial)
library(patchwork)
library(units)
library(grid)
library(kableExtra)
library(sf)

world <- rnaturalearth::ne_countries(returnclass = "sf", scale = 50) 
shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp")

ggplot() +
  geom_sf(data = world) +
  coord_sf(xlim = c(25, 55), ylim = c(7, 33)) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.01, "in"), pad_y = unit(0.001, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("plots/region.png", dpi = 360)

middle_east <- rnaturalearth::ne_countries(returnclass = "sf", scale = 110) %>% 
  filter(name == "Saudi Arabia") %>% st_transform(crs = 32637)

ggplot() +
  geom_sf(data = middle_east) +
  geom_sf(data = shape.data) +
  geom_sf(data = shape.data %>% filter(IslandName == "Quman"), color = "#cb6527", lwd =0.8) +
  geom_sf(data = shape.data %>% filter(IslandName == "Al Ishsh Ash Sharqi"), color = "#489b7b", lwd =0.8) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.01, "in"), pad_y = unit(0.23, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(xlim = c(247220.3, 321072.7), ylim = c(2783820,2865031))


ggsave("plots/lagoon.png", dpi = 360)

ggplot() +
  geom_sf(data = shape.data) +
  geom_sf(data = shape.data %>% filter(IslandName == "Quman"), color = "#cb6527", lwd =0.8) +
  geom_sf(data = shape.data %>% filter(IslandName == "Al Ishsh Ash Sharqi"), color = "#489b7b", lwd =0.8) +
  geom_sf(data = receiver %>% filter(otn_array == "QUMAN" | otn_array == "Al Osh Al Sharqi" )) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.01, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "tr", width_hint = 0.1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(xlim = c(274747.6, 286018.9), ylim = c(2825883,2840711))


ggsave("plots/islands.png", dpi = 360)


# Abacus ------------------------------------------------------------------


  
# Abacus plot

dets %>% 
  mutate(date = date(detection_timestamp_utc)) %>% 
  add_count(transmitter_id, date) %>% distinct(transmitter_id, date, .keep_all = T) %>% 
  ggplot() +
  geom_point(aes(x = date, y = reorder(transmitter_id, tag_activation_date, decreasing = T), 
                 color = otn_array, size = n), shape = 20) +
  geom_point(data = dets %>% distinct(transmitter_id, .keep_all = T),
             aes(x=tag_activation_date, y = transmitter_id), 
             shape = 4, size = 2, color = "grey41") +
  geom_point(data = dets %>% distinct(transmitter_id, .keep_all = T),
             aes(x=tag_activation_date +est_tag_life, y = transmitter_id),
             shape = 4, size = 2, color = "grey41") +
  geom_vline(xintercept = date("2025-05-01 08:41:16 EDT"), color = "grey41", linetype = "longdash") +
  facet_wrap(~otn_array, scales = "free_y", nrow = 2)+
  scale_color_manual(values = c("#489b7b", "#cb6527")) +
  theme_minimal() +
  scale_x_date(breaks= seq(date("2022-11-15"), date("2025-05-15"), length=6),
               date_labels = "%b-%Y") +
  coord_cartesian(xlim = c(date("2022-11-15"), date("2025-05-20"))) +
  labs(
    x = "Date",
    y = "Halavi Guitarfish") +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.title.position = "plot", 
        plot.caption.position =  "plot",  
        axis.text.y = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(vjust=-5),
        axis.title.x = element_text(vjust=0),
        text = element_text(size = 10),
        legend.position = "none")
ggsave("plots/abacus.png", dpi = 360)




# hotspot -----------------------------------------------------------------

receiver <- st_as_sf(HalaviArray, coords = c("deploy_long", "deploy_lat"), crs = 4326) %>% distinct(station_no, .keep_all = T) %>% 
  filter(!str_detect(station_no, "GH")) %>% filter(!str_detect(station_no, "Deep"))

station_counts <- dets %>% group_by(station_no) %>% 
  mutate(n = n()) %>% 
  distinct(station_no, .keep_all = T)

# Load your shapefile
shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp")


data_sf <- st_as_sf(station_counts, coords = c("deploy_long", "deploy_lat"), crs = 4326)

# Transform to UTM
data_utm <- st_transform(data_sf, crs = 32637)

Qu <- ggplot() +
  geom_sf(data = shape.data %>% filter(IslandName == "Quman")) +  # Plot shapefile
  geom_sf(data = receiver %>% filter(otn_array == "QUMAN"), color=alpha("black",0.1), lwd =2) +
  geom_sf(data = data_utm %>% filter(otn_array == "QUMAN"), aes(size = n, color = n)) +  # Plot data points with color by transmitter_id
  scale_color_gradient(limits = range(0,190479),
                       low = "grey", high = "#900000") +#geom_sf(data = stations, shape = 1, alpha = .5) +
  #geom_sf_text(data = stations, aes(label = station_no), vjust = -1, size = 3) +  # Add station labels
  #labs(title = "Detections") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
# xlim(st_bbox(data_utm)[1],
#      st_bbox(data_utm)[3]) +
# ylim(c(st_bbox(data_utm)[2],
#        st_bbox(data_utm)[4])) 

Sharqi <- ggplot() +
  geom_sf(data = shape.data %>% filter(IslandName == "Al Ishsh Ash Sharqi")) +  # Plot your shapefile
  geom_sf(data = data_utm %>% filter(otn_array == "Al Osh Al Sharqi"), aes(size = n,  color = n)) +  # Plot data points with color by transmitter_id
  scale_color_gradient(limits = range(0,190479),
                       low = "grey", high = "#900000") +
  #geom_sf(data = stations, shape = 1, alpha = .5) +
  #geom_sf_text(data = stations, aes(label = station_no), vjust = -1, size = 3) +  # Add station labels
  #labs(title = "Detections") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 


layout <- (Qu |  Sharqi) + plot_layout(guides = 'collect')
layout

ggsave("plots/hotspot.png", dpi = 360)


# residency table ---------------------------------------------------------

res %>% 
  select(1:8,11, 17, 18,20,21) %>% arrange(Deployment_Date) %>% kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria")



# GAM monthly models ------------------------------------------------------


