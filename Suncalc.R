# multiple coordinates
data <- data.frame(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1),
                   lat = c(rep(50.1, 10), rep(49, 10)),
                   lon = c(rep(1.83, 10), rep(2, 10)))


getSunlightTimes(data = data,
                 keep = c("sunrise", "sunset", "night", "nightEnd"), tz = "UTC") %>% View()

getMoonIllumination(date = Sys.Date(), keep = c("fraction", "phase", "angle"))
                    
 



df1 <- structure(list(datetime = structure(c(1533081600, 1533081900, 
                                             1533082200, 1533082500, 1533082800, 1533083100), class = c("POSIXct", 
                                                                                                        "POSIXt"), tzone = "UTC"), lat = c(50, 50, 50, 50, 50, 50), lon = c(110, 
                                                                                                                                                                            110, 110, 110, 110, 110)), row.names = c(NA, -6L), class = c("tbl_df", 
                                                                                                                                                                                                                                         "tbl", "data.frame"))

df2 <- structure(list(date = structure(c(17744, 17744, 17744, 17744, 
                                         17744, 17744), class = "Date"), lat = c(50, 50, 50, 50, 50, 50
                                         ), lon = c(110, 110, 110, 110, 110, 110), sunrise = structure(c(1533071390, 
                                                                                                         1533071390, 1533071390, 1533071390, 1533071390, 1533071390), class = c("POSIXct", 
                                                                                                                                                                                "POSIXt"), tzone = "UTC"), sunset = structure(c(1533126318, 1533126318, 
                                                                                                                                                                                                                                1533126318, 1533126318, 1533126318, 1533126318), class = c("POSIXct", 
                                                                                                                                                                                                                                                                                           "POSIXt"), tzone = "UTC"), night = structure(c(1533135716, 1533135716, 
                                                                                                                                                                                                                                                                                                                                          1533135716, 1533135716, 1533135716, 1533135716), class = c("POSIXct", 
                                                                                                                                                                                                                                                                                                                                                                                                     "POSIXt"), tzone = "UTC"), nightEnd = structure(c(1533061993, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                       1533061993, 1533061993, 1533061993, 1533061993, 1533061993), class = c("POSIXct", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "POSIXt"), tzone = "UTC")), row.names = c(NA, -6L), class = c("tbl_df", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "tbl", "data.frame"))

x <- dets %>% dplyr::select(date, latitude, longitude)     
x <- x[1:5,] %>% rename(lat = latitude,
                        lon = longitude)

suncalc <- getSunlightTimes(data = x ,keep = c("sunrise", "sunset", "night"), tz = "UTC")

library(suncalc)
library(dplyr)
library(lubridate)

df1 %>%
  left_join(df2) %>%
  distinct() %>%
  mutate(period = case_when(datetime %within% interval(sunset, night) ~ 'dusk',
                            datetime %within% interval(nightEnd, sunrise) ~ 'dawn',
                            datetime %within% interval(sunrise, sunset) ~ 'day',
                            datetime %within% interval(night, nightEnd) ~ 'night')) %>%
  dplyr::select(datetime, lat, lon, period) 



df1_1 <- dets %>% 
  rename(lat = latitude,
         lon = longitude) %>% 
  head(5000)


suncalc_prep <- dets %>% dplyr::select(date, latitude, longitude) %>% 
  rename(lat = latitude,
         lon = longitude)  %>% 
  head(5000)

df2_1 <- getSunlightTimes(data=suncalc_prep,
                 keep = c("sunrise", "sunset"), tz = "UTC") 

dets_time <- df1_1 %>%
  left_join(df2_1) %>%
  distinct() %>%
  mutate(period = case_when(detection_timestamp_utc %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ 'night'),
         detection_timestamp_AST = with_tz(detection_timestamp_utc, "Asia/Riyadh"), 
         hour = hour(detection_timestamp_AST))


#overall
overall_df <- dets %>% 
  mutate(detection_timestamp_AST = with_tz(detection_timestamp_utc, "Asia/Riyadh"),
         hour = hour(detection_timestamp_AST),
         period = ifelse(hour < 18 & hour > 6, 'day', 'night')) %>% 
  group_by(hour, period) %>% 
  summarise(n = n()) 

#daily mean

daily_df <- dets %>% 
  mutate(detection_timestamp_AST = with_tz(detection_timestamp_utc, "Asia/Riyadh"),
         hour = hour(detection_timestamp_AST),
         period = ifelse(hour < 18 & hour > 6, 'day', 'night')) %>% 
  group_by(date, hour) %>% 
  summarise(n = n()) %>% ungroup() %>% 
  group_by(hour) %>% 
  summarise(mean_n = mean(n))


df <- left_join(overall_df, daily_df, by = "hour")

ggplot(df) +
  geom_col(aes(x = hour, y = mean_n, fill = period),
           position = "dodge2",
           show.legend = TRUE,
           alpha = .9) + 
  coord_polar()


library(stringr)

hike_data <- tuesdata$hike_data


hike_data$region <- as.factor(word(hike_data$location, 1, sep = " -- "))

hike_data$length_num <- as.numeric(sapply(strsplit(hike_data$length, " "), "[[", 1))

plot_df <- hike_data %>%
  group_by(region) %>%
  summarise(
    sum_length = sum(length_num),
    mean_gain = mean(as.numeric(gain)),
    n = n()
  ) %>%
  mutate(mean_gain = round(mean_gain, digits = 0))

plt <- ggplot(df) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:2) * 100),
    color = "lightgrey"
  ) + 
  # Add bars to represent the cumulative track lengths
  # str_wrap(region, 5) wraps the text so each line has at most 5 characters
  # (but it doesn't break long words!)
  geom_col(
    aes(
      x = hour,
      y = mean_n,
      fill = period
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  
  # Add dots to represent the mean gain
  #geom_point(
  #  aes(
  #    x = reorder(str_wrap(region, 5),sum_length),
  #    y = mean_gain
  #  ),
  #  size = 3,
  #  color = "gray12"
  #) +
  
  # Lollipop shaft for mean gain per region
  geom_segment(
    aes(
      x = hour,
      y = 0,
      xend = hour,
      yend = 200
    ),
    linetype = "dashed",
    color = "gray12",
    alpha = 0.7
  ) + 
  
  # Make it circular!
  coord_polar() 

plt

plt <- plt +
  # Annotate the bars and the lollipops so the reader understands the scaling
  annotate(
    x = 11, 
    y = 1300,
    label = "Mean Elevation Gain\n[FASL]",
    geom = "text",
    angle = -67.5,
    color = "gray12",
    size = 2.5) +
  annotate(
    x = 11, 
    y = 3150,
    label = "Cummulative Length [FT]",
    geom = "text",
    angle = 23,
    color = "gray12",
    size = 2.5) +
  # Annotate custom scale inside plot
  annotate(
    x = 11.7, 
    y = 1100, 
    label = "1000", 
    geom = "text", 
    color = "gray12") +
  annotate(
    x = 11.7, 
    y = 2100, 
    label = "2000", 
    geom = "text", 
    color = "gray12") +
  annotate(
    x = 11.7, 
    y =3100, 
    label = "3000", 
    geom = "text", 
    color = "gray12") +
  # Scale y axis so bars don't start in the center
  scale_y_continuous(
    limits = c(-1500, 3500),
    expand = c(0, 0),
    breaks = c(0, 1000, 2000, 3000)
  ) + 
  # New fill and legend title for number of tracks per region
  scale_fill_gradientn(
    "Amount of Tracks",
    colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195")
  ) +
  # Make the guide for the fill discrete
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 12),
    # Move the legend to the bottom
    legend.position = "bottom",
  )

plt


plt <- plt + 
  # Add labels
  labs(
    title = "\nHiking Locations in Washington",
    subtitle = paste(
      "\nThis Visualisation shows the cummulative length of tracks,",
      "the amount of tracks and the mean gain in elevation per location.\n",
      "If you are an experienced hiker, you might want to go",
      "to the North Cascades since there are a lot of tracks,",
      "higher elevations and total length to overcome.",
      sep = "\n"
    ),
    caption = "\n\nData Visualisation by Tobias Stalder\ntobias-stalder.netlify.app\nSource: TidyX Crew (Ellis Hughes, Patrick Ward)\nLink to Data: github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-11-24/readme.md") +
  # Customize general theme
  theme(
    
    # Set default color and font family for the text
    text = element_text(color = "gray12"),
    
    # Customize the text in the title, subtitle, and caption
    plot.title = element_text(face = "bold", size = 25, hjust = 0.05),
    plot.subtitle = element_text(size = 14, hjust = 0.05),
    plot.caption = element_text(size = 10, hjust = .5),
    
    # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )
# Use `ggsave("plot.png", plt,width=9, height=12.6)` to save it as in the output
plt
