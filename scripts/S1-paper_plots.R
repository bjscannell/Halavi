
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
  scale_color_manual(values = c("#489b7b", "#48579b")) +
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
        panel.border = element_blank(),
        legend.position = "none",
        text = element_text(family = "serif")) 
  


ggsave("plots/abacus.png", dpi = 600, height = 4.5, width = 9.2)




# hotspot -----------------------------------------------------------------
# 
# receiver <- st_as_sf(HalaviArray, coords = c("deploy_long", "deploy_lat"), crs = 4326) %>% distinct(station_no, .keep_all = T) %>% 
#   filter(!str_detect(station_no, "GH")) %>% filter(!str_detect(station_no, "Deep"))
# 
# station_counts <- dets %>% group_by(station_no) %>% 
#   mutate(n = n()) %>% 
#   distinct(station_no, .keep_all = T)
# 
# # Load your shapefile
# shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp")
# 
# 
# data_sf <- st_as_sf(station_counts, coords = c("deploy_long", "deploy_lat"), crs = 4326)
# 
# # Transform to UTM
# data_utm <- st_transform(data_sf, crs = 32637)
# 
# Qu <- ggplot() +
#   geom_sf(data = shape.data %>% filter(IslandName == "Quman")) +  # Plot shapefile
#   geom_sf(data = receiver %>% filter(otn_array == "QUMAN"), color=alpha("black",0.1), lwd =2) +
#   geom_sf(data = data_utm %>% filter(otn_array == "QUMAN"), aes(size = n, color = n)) +  # Plot data points with color by transmitter_id
#   scale_color_gradient(limits = range(0,190479),
#                        low = "grey", high = "#900000") +#geom_sf(data = stations, shape = 1, alpha = .5) +
#   #geom_sf_text(data = stations, aes(label = station_no), vjust = -1, size = 3) +  # Add station labels
#   #labs(title = "Detections") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank()) 
# # xlim(st_bbox(data_utm)[1],
# #      st_bbox(data_utm)[3]) +
# # ylim(c(st_bbox(data_utm)[2],
# #        st_bbox(data_utm)[4])) 
# 
# Sharqi <- ggplot() +
#   geom_sf(data = shape.data %>% filter(IslandName == "Al Ishsh Ash Sharqi")) +  # Plot your shapefile
#   geom_sf(data = data_utm %>% filter(otn_array == "Al Osh Al Sharqi"), aes(size = n,  color = n)) +  # Plot data points with color by transmitter_id
#   scale_color_gradient(limits = range(0,190479),
#                        low = "grey", high = "#900000") +
#   #geom_sf(data = stations, shape = 1, alpha = .5) +
#   #geom_sf_text(data = stations, aes(label = station_no), vjust = -1, size = 3) +  # Add station labels
#   #labs(title = "Detections") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank()) 
# 
# 
# layout <- (Qu |  Sharqi) + plot_layout(guides = 'collect')
# layout
# 
# ggsave("plots/hotspot.png", dpi = 360)


# residency table ---------------------------------------------------------

res %>% 
  select(1:8,11, 17, 18,20,21) %>% arrange(Deployment_Date) %>% kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria")





# Total length model ------------------------------------------------------

TL <- ggplot() +
  geom_line(data = res_predicts, aes(x = x, y = predicted), color = "#9b5a48") +
  geom_ribbon(data = res_predicts,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#9b5a48") +
  geom_point(data = overall_metrics, aes(x = TL, y = residency_min), color = "#9b5a48", alpha = 0.75) +
  scale_y_continuous(limits = c(0, 1),
                     labels = seq(0, 1, by = 0.25)) +
  labs(x = "Total Length (cm)",
       y = "Residency Minimum") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.title.position = "plot", 
        plot.caption.position =  "plot",  
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 25),
        axis.title.y = element_text(vjust=0),
        axis.title.x = element_text(vjust=0),
        text = element_text(family = "serif", size = 10),
        legend.position = "none")

TL

ggsave("plots/TL.png", dpi = 600, height = 4.8, width = 8.3)

# GAM monthly models ------------------------------------------------------

# this is the montly model just for plotting over temperature 
res_gam <- ggplot(newdata_monthly_res, aes(x = month, y = fit)) +
  geom_line(color = "#9b5a48", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9b5a48", alpha = 0.3) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_y_continuous(limits = c(0, 1),
                     labels = seq(0, 1, by = 0.25)) +
  labs(x = "Month", y = "Predicted Monthly Residency") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.title.position = "plot", 
        plot.caption.position =  "plot",  
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(vjust=0),
        axis.title.x = element_text(vjust=0),
        text = element_text(size = 10, family = "serif"),
        legend.position = "none")

res_gam

ggsave("plots/monthly_res.png", dpi = 600, height = 4.8, width = 8.3)


# temperature effect ------------------------------------------------------


temp_effect <- ggplot(newdata_temp_res) +
  geom_ribbon(aes(x = temp, ymin= lower, ymax=upper), alpha = 0.3) +
  geom_line(aes(x = temp, y = fit)) +
  labs(x = "Temperature",
       y = "Predicted Probability of Residency") +
  theme_bw() +
  scale_x_continuous(limits = c(16, 32), breaks = seq(16, 32, by = 2)) +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.title.position = "plot", 
        plot.caption.position =  "plot",  
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 50),
        axis.title = element_text(size = 60),
        axis.title.y = element_text(vjust=0),
        axis.title.x = element_text(vjust=0),
        text = element_text(size = 10, family = "serif"),
        legend.position = "none")

temp_effect


temp_partial <-
  draw(gam_temp_res, select = c(1), smooth_col ="#703534", ci_col = "#703534") +
  scale_x_continuous(limits = c(16, 32), breaks = seq(16, 32, by = 2)) +
  scale_y_continuous(position = "right") +
  labs(x = "Temperature (C)",title = "")  +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.title.position = "plot", 
        plot.caption.position =  "plot",  
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(vjust=0),
        axis.title.x = element_text(vjust=0),
        text = element_text(size = 10, family = "serif"),
        legend.position = "none")
  

temp_partial

ggsave("plots/temp_partial.png", dpi = 360, height = 4.2, width = 4.55)


# monthly temp ------------------------------------------------------------


temp <- ggplot(temp_rec, aes(month, monthly_temp)) + 
  geom_smooth(se=F, color = "#703534", linetype = "dashed") +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_y_continuous(limits = c(15, 35), breaks = seq(15, 35, by = 5),
                     position = "right") +
  labs(x = "Month", y = "Monthly Average Temperature (C)") +
  theme(
    plot.caption = element_text(hjust = 0, face= "italic"), 
    plot.title.position = "plot", 
    plot.caption.position =  "plot",  
    strip.text.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 50),
    axis.text.y = element_text(colour = "#703534"),
    axis.title = element_text(size = 60),
    axis.title.y = element_text(vjust=0, color = "#703534"),
    axis.ticks.y = element_line(color = "#703534"),
    axis.title.x = element_text(vjust=0),
    text = element_text(size = 10, family = "serif"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA))

temp

ggsave("plots/temp.png", dpi = 360, height = 5.3, width = 8.3, bg = "transparent")
ggsave("plots/temp.png", dpi = 360, height = 4.8, width = 8.3, bg = "transparent")

# EDMC --------------------------------------------------------------------

juv_plot <- edmc_plot(edmc_juv)
yoy_plot <- edmc_plot(edmc_yoy)

winter_plot <- edmc_plot(edmc_winter)
summer_plot <- edmc_plot(edmc_summer)


juv_plot + yoy_plot + winter_plot + summer_plot + plot_layout(guides = "collect")

ggsave("plots/edmc.png", dpi = 600, height = 12, width = 8, bg = "transparent")


# Tables ------------------------------------------------------------------


library(kableExtra)
library(sjPlot)

tab_model(gam_temp_res, title = "Linear Model Summary")
tab_model(glmmfull_res, title = "Linear Model Summary")



tibble(edmc_yoy[[2]]) %>% kable() %>% kable_classic(full_width = F, html_font = "Cambria")
tibble(edmc_juv[[2]]) %>% kable() %>% kable_classic(full_width = F, html_font = "Cambria")

tibble(edmc_winter[[2]]) %>% kable() %>% kable_classic(full_width = F, html_font = "Cambria")
tibble(edmc_summer[[2]]) %>% kable() %>% kable_classic(full_width = F, html_font = "Cambria")


left_join(tibble(edmc_winter[[2]]), tibble(edmc_summer[[2]]) , by = "station") %>% 
  left_join(tibble(edmc_yoy[[2]]), by = "station") %>% 
  left_join(tibble(edmc_juv[[2]]), by = "station") %>% View()

