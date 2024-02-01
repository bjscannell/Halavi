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

