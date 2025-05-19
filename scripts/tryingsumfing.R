anova_results <- aov(res_ratio ~ Class, data = res)
summary(anova_results)
TukeyHSD(anova_results)

shapiro.test(residuals(anova_results))
bartlett.test(res_ratio ~ Class, data = res)


kruskal_result <- kruskal.test(n_stations ~ Class, data = res)
print(kruskal_result)


pairwise.wilcox.test(res$res_ratio, res$Class)

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
  group_by(location, life_stage) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = life_stage, y = count, fill = location)) +
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

# Example: Plotting the number of movements over time
df_movements %>%
  mutate(movement_date = date(detection_time)) %>%
  group_by(movement_date) %>%
  summarise(movement_count = n()) %>%
  ggplot(aes(x = movement_date, y = movement_count)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Daily Movement Counts Between North and East",
       x = "Date", y = "Number of Movements")


library(tidyverse)
library(lubridate)
library(sf)         # For spatial analysis, if needed
library(brms)       # For Bayesian modeling

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
