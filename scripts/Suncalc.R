library(suncalc)
library(dplyr)
library(lubridate)

# Adding time of day ------------------------------------------------------


dets_tz <- dets %>% 
  mutate(date_a = as.Date(with_tz(detection_timestamp_utc, tzone = "Asia/Riyadh")),
         date_times_a = with_tz(detection_timestamp_utc, tzone = "Asia/Riyadh")) 

SaudiSUN <- getSunlightTimes(distinct(dets_tz, date_a)$date_a,
                             lat = 25.551725,
                             lon = 36.845717,
                             keep = c("sunrise", "sunset"),
                             tz = "Asia/Riyadh")

dets_tz_tod <- dets_tz %>% 
  left_join(SaudiSUN, by = c("date_a" = "date")) %>%
  mutate(tod = ifelse(date_times_a >= sunrise & date_times_a < sunset, 'Day', 'Night')) %>% 
  dplyr::select(-c(lat, lon, sunrise, sunset))




# time of day plot --------------------------------------------------------


dets_tz_tod %>% 
  ggplot(aes(x = as.factor(tod))) +
  geom_histogram(stat="count") 

  # scale_y_reverse() +
  # facet_wrap(~ species, nrow = 1, strip.position = "bottom") +
  # theme(panel.spacing = unit(0, "cm"),
  #       strip.placement = "outside") +
  # theme(panel.spacing = unit(0, "cm"),
  #       strip.placement = "outside") +
  # scale_fill_manual(values = c("#ffde65","#1e3b7a")) +
  # theme_minimal() +
  # theme(panel.grid.minor = element_blank(),
  #       panel.grid.major.x = element_blank(),
  #       legend.position = "bottom",
  #       strip.placement = "outside",
  #       strip.text = element_text(face = "bold", size = 14),
  #       panel.spacing = unit(0, "cm"),
  #       plot.title.position = "plot",
  #       plot.title = element_text(face = "bold",
  #                                 size = 20,
  #                                 margin = margin(b = 10)),
  #       plot.subtitle = element_text(lineheight = 1.1,
  #                                    margin = margin(b = 20),
  #                                    size = 14,
  #                                    color = "grey30"),
  #       plot.caption.position = "plot",
  #       plot.caption = element_text(color = "grey50",
  #                                   hjust = 0)) +
  # labs(x = "", y = "Depth (m)",
  #      fill = "",
  #      title = "Depth Distribution by time of day ")



