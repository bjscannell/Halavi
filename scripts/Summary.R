
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(lubridate)
library(readr)
library(ggplot2)



# distinct ids over time --------------------------------------------------


dets %>% 
  group_by(date, sex) %>% 
  summarise(count = n_distinct(transmitter_id)) %>% 
  ggplot(aes(x=date, y = count, color = sex)) + geom_smooth(se = F) + theme_bw()


# abacus ------------------------------------------------------------------


dets %>% 
  distinct(date, transmitter_id, .keep_all = T) %>%
  filter(date > as.Date('04-27-2023',format ="%m-%d-%Y")) %>% 
  ggplot(aes(x = date, y = transmitter_id, color = sex)) +
  geom_point(aes()) + theme_minimal() +
  scale_x_date(date_labels = "%b-%Y") +
  labs(
    x = "Date",
    y = "Halavi Guitarfish ID",
    title = "Presence of Halavi Guitarfish by day in Al Wajh",
    subtitle = paste("Detections for", 
                     length(unique(dets$transmitter_id)),
                     "tagged individuals by date from acoustic recivers at Quman Island")
    
  ) +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.title.position = "plot", 
        plot.caption.position =  "plot",
        plot.title = element_text(face = "bold", size = 16)) +
  facet_wrap(~station_name)



# getting consecutive days ------------------------------------------------

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
    DW = first(length2_m),
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
      residency_min >= 0.5 ~ "Resident",
      residency_min < 0.5 & res_ratio > 0.5 ~ "Intermittent Resident",
      residency_min < 0.5 & res_ratio <= 0.5 ~ "Transient",
      TRUE ~ NA_character_),
    n_stations = n_distinct(station_name)) %>% 
  left_join(consec, by = "transmitter_id")



res %>% 
  ggplot(aes(x=res_ratio, y = residency_min,
             color = res_type, size = DW)) +
  geom_point()



res %>% 
  ggplot(aes(x=res_ratio, y = residency_min,
             color = res_type, shape = Class)) +
  geom_point()


dets %>% 
  distinct(date, transmitter_id, .keep_all = T) %>%
  ggplot(aes(x = date, y = transmitter_id, color = sex)) +
  geom_point(aes()) + theme_minimal() +
  scale_x_date(date_labels = "%b-%Y") 


# number of stations by size ----------------------------------------------


dets %>% group_by(transmitter_id) %>% 
  mutate(n_stations = n_distinct(station_name)) %>% 
  distinct(transmitter_id, .keep_all = T) %>% 
  ggplot(aes(x=n_stations, y=length2_m)) +
  geom_point() +
  geom_label(aes(label =length_m)) + 
  geom_smooth(method = "lm", se = T)

res %>% 
  filter(Deployment_Date < as.Date('01-27-2023',format ="%m-%d-%Y")) %>% View()
