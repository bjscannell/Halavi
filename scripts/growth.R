
# length classes  ---------------------------------------------------------


n_med <- function(x){
  return(data.frame(y =5, 
                    label = paste0("", round(median(x),digits =3))))
}

HalaviTaggingMetadata$life_stage <- factor(HalaviTaggingMetadata$life_stage, levels = c("ADULT", "SUB", "JUV", "YOY", "NEO"))


ggsave("Halavi_lifestages.png", width = 12.5, height = 8.5, dpi = 360)


library(dplyr)
library(fuzzyjoin)
library(ggnewscale)
library(ggplot2)
library(RColorBrewer)

length_range <- HalaviTaggingMetadata %>% group_by(life_stage) %>% 
  summarise(max_l = max(length_cm),
            min_l = min(length_cm))


# Join based on condition: length_cm >= min and length_cm <= max
HalaviTaggingMetadata_matched <- HalaviTaggingMetadata %>% dplyr::select(life_stage, length_cm, tag_serial_number) %>% 
  fuzzy_left_join(
    length_range,
    by = c("length_cm" = "min_l", "length_cm" = "max_l"),
    match_fun = list(`>=`, `<=`)
  ) %>% group_by(tag_serial_number) %>%
  mutate(dup = n() > 1) %>% ungroup() %>% distinct(tag_serial_number, .keep_all = T) %>% dplyr::select(tag_serial_number, dup)


problem <- HalaviTaggingMetadata %>%  left_join(HalaviTaggingMetadata_matched) %>% 
  mutate(dup = case_when(length_cm == 33.3 ~ F,
                         length_cm == 43.6 ~ F,
                         .default = dup))


lp <- problem %>% 
  #filter(transmitter_id %in% tags) %>% 
  ggplot(aes(x=life_stage, y = length_cm)) +
  geom_point(aes( color = dup), size = 1.5,alpha = .6,
             position = position_jitter(seed = 1, width = .1)) +
  scale_colour_manual(values = c("green", "red"), name = "Multiple \nRanges") +
  new_scale_color() +
  scale_x_discrete(name = "Life Stage") +
  scale_y_continuous(name = "Length (cm)") +
  #   geom_segment(data = length_range, 
  #                aes(
  #                  x = 0.5, y = min_l,
  #                  xend = 5.5, yend = min_l,
  #                   color = life_stage)) +
  # geom_segment(data = length_range, 
  #              aes(
  #                x = 0.5, y = max_l,
  #                xend = 5.5, yend = max_l,
  #                color = life_stage)) +
  #   scale_color_brewer(palette="Spectral") +
  stat_summary(geom = "text",fun.data = n_fun, vjust = 7.5) +
  stat_summary(geom = "label",fun.data = n_med, size = 5,
               fill = "white", alpha = 0.7, label.padding = unit(0.20, "lines"),) +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 25),
    axis.title.x = element_text(vjust = -3),
    text = element_text(size = 10),
    plot.margin = margin(1,1,1.5,1.2, "cm")
  )

ggsave("plots/length_isssues.png", width = 12.5, height = 8.5, dpi = 360)


# growth rates ------------------------------------------------------------



caps <- read_csv("data/tags/capture_data.csv") %>% dplyr::select(ID, Date, TL, DW, Wt_g, Recapture, Lifestage, acoustic_sn) %>%
  mutate(Date = dmy(Date),
         TL = as.numeric(TL),
         DW = as.numeric(DW))

recap_ind <- caps %>% filter(Recapture == "Y" & !is.na(ID)) %>% pull(ID)

recaps <- caps %>% filter(ID %in% recap_ind) %>% filter(!is.na(TL))

# lets only use our fish 
# sns <- fish %>% distinct(tag_serial_number) %>% pull()
# 
# sns_ID <-recaps %>% filter(acoustic_sn %in% sns) %>% pull(ID)
# acoustic_recaps <- caps %>% filter(ID %in% sns_ID) %>% filter(!is.na(TL))

# lets use all the fish
all_recaps <- caps %>% filter(!is.na(TL))


df <- all_recaps %>% 
  mutate(key = 1) %>% 
  group_by(ID) %>% 
  mutate(cap = cumsum(key)) %>% dplyr::select(-key) %>% 
  arrange(ID,Date) %>% 
  mutate(prev_date = lag(Date),
         prev_TL = lag(TL),
         days_diff = as.numeric(Date-prev_date),
         years_diff = days_diff/365.25,
         delta_TL = TL-prev_TL,
         growth_per_year = delta_TL/years_diff)

df <- df %>% filter(!is.na(growth_per_year)) %>% arrange(ID, Date) 


View(df)

ggplot(all_recaps) +
  geom_histogram(aes(x=TL))

TL <-caps %>% filter(!is.na(TL)) %>% pull(TL)

cl <- kmeans(TL, centers = 5)

plot(TL, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)

as.data.frame(cbind(TL,cl$cluster)) %>% group_by(V2) %>% 
  summarise(maxTL = max(TL),
            minTL = min(TL)) %>% 
  arrange(minTL)



# How long do umbicilical scars last in gluc? We captured one 
# @ 35.9 with scar

# min - 36 is YOY?

library(gghighlight)
df2 <- df %>% filter(!is.na(growth_per_year)) %>% arrange(ID, Date) %>% filter(days_diff > 100 & growth_per_year < 100)

ggplot(df2, aes(x = TL, y = growth_per_year, color = as.factor(ID))) +
  geom_point(size = 4) +
  gghighlight(ID == 3706,
              unhighlighted_params = list(linewidth = 1, colour = alpha("black", 0.4))) +
  guides(colour="none") +
  coord_cartesian(ylim = c(0, 30)) +
  theme_bw()
  
ggsave("plots/growth.png", dpi = 360)

# 40, and 45+
# adults at 80 

new_class <- data.frame(class = c("YOY", "JUV", "ADULT"),
           age = c("25-40", "40-80", "80+"))
new_class %>% kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria")
