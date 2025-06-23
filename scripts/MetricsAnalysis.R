

# Residency Index ---------------------------------------------------------

# overall
resid <- res %>% 
  dplyr::select(transmitter_id, residency_min, residency_max, Class)

# monthly
monthly_res <- dets %>% 
  mutate(month = month(detection_timestamp_utc),
         month_day = format(as.Date(date), "%d-%m")) %>% 
  group_by(transmitter_id, month) %>% 
  mutate(n = length(unique(month_day)),
         monthly_res = n/(days_in_month(month)),
         monthly_res = ifelse(monthly_res > 1, 1, monthly_res)) %>% 
  distinct(transmitter_id, month, .keep_all = T) %>%
  select(transmitter_id, month, otn_array, sex, length_cm, 
         length2_cm, new_class, monthly_res) 
  
# 
#   dets %>%
#   mutate(month = month(date),
#          month_day = format(as.Date(date), "%d-%m")) %>% 
#   group_by(transmitter_id, month) %>% 
#   summarise(
#     transmitter_id = first(transmitter_id),
#     sex = first(sex),
#     length2_cm = first(length2_cm),
#     length_cm = first(length_cm),
#     life_stage = first(new_class),
#     otn_array = first(otn_array),
#     Deployment_Date = first(tag_activation_date),
#     # Last_Detection = max(date),
#     # First_Detection = min(date),
#     # Days_Liberty = as.numeric(first(Last_Detection) - first(First_Detection)) + 1,
#     # Tag_on = first(date),
#     # Tag_off = first(date) + days(first(est_tag_life)),
#     # Days_Monitored = if_else(first(Last_Detection) > first(Tag_off),
#     #                          first(Last_Detection) - first(Tag_on) + 1,
#     #                          first(Tag_off) - first(Tag_on) +1),
#     Days_Present = length(unique(month_day)),
#     #residency_max = as.numeric(Days_Present) / as.numeric(Days_Liberty),
#     #residency_min = as.numeric(Days_Present)/ as.numeric(Days_Monitored),
#     #res_ratio = as.numeric(Days_Liberty)/ as.numeric(Days_Monitored),
#     monthly_res = Days_present/(days_in_month(month))) %>%
#   distinct(transmitter_id, month, .keep_all = T) %>% 
#   dplyr::select(transmitter_id, month, otn_array, sex, length_cm, 
#                 length2_cm, life_stage, monthly_res)


# Roaming Index -----------------------------------------------------------

#' Use all the receivers per island even if they have received no
#' detections

Quman <- 15
Sharqi <- 8

# Overall
roam <- res %>% 
  mutate(RI = case_when(Tagging_Island == "QUMAN" ~ n_stations/Quman,
                        Tagging_Island == "Al Osh Al Sharqi" ~ n_stations/Sharqi)) %>% 
  select(transmitter_id, RI, Sex, TL, DW)

# monthly

monthly_roam <- dets %>%
  mutate(month = month(date)) %>% 
  group_by(transmitter_id, month) %>% 
  mutate(n_stations = n_distinct(station_no),
         RI = case_when(otn_array == "QUMAN" ~ n_stations/Quman,
                        otn_array == "Al Osh Al Sharqi" ~ n_stations/Sharqi)) %>% 
  dplyr::select(transmitter_id, month, n_stations, RI ) %>% 
  distinct(transmitter_id, month, .keep_all = T)



# Join Metrics ----------------------------------------------------

monthly_metrics <-left_join(monthly_res, monthly_roam, by = c("transmitter_id", "month"))

overall_metrics <- left_join(roam, resid, by = c("transmitter_id")) %>% 
  mutate(Sex = as.factor(Sex),
         Class = as.factor(Class))


# monthly overview --------------------------------------------------------


ggplot(monthly_metrics) +
  geom_smooth(aes(x = month, y = RI), color = "cadetblue") +
  geom_smooth(aes(x = month, y = monthly_res), color = "coral3") +

  theme_bw()

ggsave("plots/overall_index.png", dpi = 360)


# RESIDENCY PLOTS -------------------------------------------------------------------
## Sex

showtext_auto()

quota_halves_sex <- ggplot(overall_metrics, aes(x = Sex, y = residency_min)) +
  geom_half_point(aes(color = Sex), 
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 0.5, alpha = 0.5) +
  geom_half_boxplot(aes(fill = Sex), side = "r") + 
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Sex", y = "Residency Index") +
  theme_clean()

quota_densities_sex <- ggplot(overall_metrics, aes(x = residency_min, fill = Sex)) +
  geom_density(alpha = 0.6) +
  scale_x_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  labs(x = "Residency Index", y = "Density", fill = "Sex") +
  theme_clean() +
  theme(legend.position = "bottom")

res_sex <- quota_halves_sex | quota_densities_sex

ggsave("plots/overall_res_sex.png", dpi = 300)

## Lifestage
showtext_auto()

quota_halves_class <- ggplot(overall_metrics, aes(x = Class, y = residency_min)) +
  geom_half_point(aes(color = Sex), 
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 0.5, alpha = 0.5) +
  geom_half_boxplot(aes(fill = Sex), side = "r") + 
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Age Class", y = "Residency Index") +
  theme_minimal()

quota_densities_class <- ggplot(overall_metrics, aes(x = residency_min, fill = Class)) +
  geom_density(alpha = 0.6) +
  scale_x_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  labs(x = "Residency Index", y = "Density", fill = "Age Class") +
  theme_clean() +
  theme(legend.position = "bottom")

res_class <-quota_halves_class | quota_densities_class

ggsave("plots/overall_res_class.png", dpi = 300)


# Total Length
ggplot(overall_metrics %>% drop_na(residency_min), aes(x = TL, y = residency_min)) +
  geom_point(aes(color = Sex), size = 1) +
  geom_smooth(method = "lm", formula = 'y ~ x') +
  #geom_label_repel(data = filter(vdem_2015, highlight == TRUE), 
  #                 aes(label = country_name),
  #                 seed = 1234) +
  scale_y_continuous(labels = label_percent()) +
  #scale_color_manual(values = c("grey30", "#FF4136"), guide = "none") +
  labs(x = "TL", y = "residency index") +
  theme_clean()

# Monthly residency
ggplot(monthly_metrics, aes(x = as.factor(month), y = monthly_res)) +
  geom_half_point(aes(color = sex), 
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 0.5, alpha = 0.5) +
  geom_half_boxplot(aes(fill = sex), side = "r") + 
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none") +
  labs(x = "Month", y = "Residency Index") +
  theme_clean()

ggsave("plots/monthly_res_sex.png", dpi = 300)

ggplot(monthly_metrics, aes(x = as.factor(month), y = monthly_res)) +
  geom_half_point(aes(color = new_class), 
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 0.5, alpha = 0.5) +
  geom_half_boxplot(aes(fill = new_class), side = "r") + 
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none") +
  labs(x = "Month", y = "Residency Index") +
  theme_clean()

ggplot(monthly_metrics) +
  geom_smooth(aes(x=month, y= monthly_res, color = new_class))

ggsave("plots/monthly_res_class.png", dpi = 300)

# residency MODEL ---------------------------------------------------------------

# OVERAL 
library(brms)

# Fit the model
beta_model <- brm(
  formula = residency_min ~ TL + Sex + (1|transmitter_id),
  data = overall_metrics,
  family = Beta(),
  chains = 4,
  cores = 4,
  iter = 5000,
  seed = 123
)



# Fit the model
# beta_model_class <- brm(
#   formula = residency_min ~ Class + Sex,
#   data = overall_metrics,
#   family = Beta(),
#   chains = 4,
#   cores = 4,
#   iter = 5000,
#   seed = 123
# )

plot(beta_model)
pp_check(beta_model,ndraws = 100)
summary(beta_model)


library(tidybayes)
library(tidyr)

#' fix the plotting for random effets
#' https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/

pred_res <- beta_model %>% 
  epred_draws(newdata = expand_grid(Sex = c("M", "F"),
                                    transmitter_id = "A69-1605-52",
                                    TL = seq(25, 70, by = 5)))

ggplot(pred_res, aes(x = TL, y = .epred, color = Sex)) +
  geom_point(data = overall_metrics, aes(x = TL, y = residency_min)) +
  stat_lineribbon(alpha = 0.3, .width = 0.95, fill = "gray65") +
  scale_fill_brewer(palette = "Greys")  +
  coord_cartesian(xlim = c(34,70)) +
  labs(x = "TL", y = "Predicted Residency Index",
       fill = "Credible interval") +
  theme_bw() +
  theme(legend.position = "bottom")


ggsave("overal_resid_model.png", dpi=360)

# MONTHLY

# Fit the model
beta_model_monthly <-  brm(RI ~ 0 + Intercept + length_cm + sex + month, data=monthly_metrics,
                                            family=ord_beta_reg,
                                            cores=4,chains=4,
                                            prior = priors,
                                            refresh=0,
                                            iter = 5000,
                                            backend="cmdstanr",
                                            stanvars=stanvars)

summary(beta_model_monthly)
plot(beta_model_monthly)
pp_check(beta_model_monthly,ndraws = 100)




pred_res_month <- beta_model_monthly %>% 
  epred_draws(newdata = expand_grid(sex = c("M", "F"),
                                    length_cm = seq(25, 85, by = 5),
                                    #transmitter_id = "A69-1605-52",
                                    month = seq(1,12, by = 1)))

ggplot(pred_res_month, aes(x = month, y = .epred, color = sex)) +
  geom_point(data = monthly_metrics, aes(x = month, y = monthly_res)) +
  stat_lineribbon(alpha = 0.5) +
  scale_fill_brewer(palette = "Greys")  +
  #coord_cartesian(xlim = c(34,85)) +
  labs(x = "TL", y = "Predicted residency Index",
       fill = "Credible interval") +
  theme_bw() +
  theme(legend.position = "bottom")


# roaming MODEL -----------------------------------------------------------------

library(brms)


# Fit the model
brms_fit <- brm(RI ~ 0 + Intercept + TL + Sex , data=overall_metrics,
                family=ord_beta_reg,
                cores=4,chains=4,
                prior = priors,
                refresh=0,
                iter = 5000,
                backend="cmdstanr",
                stanvars=stanvars)



brms_fit_class <- brm(RI ~ 0 + Intercept + Class + Sex , data=overall_metrics,
                family=ord_beta_reg,
                cores=4,chains=3,
                prior = priors,
                refresh=0,
                iter = 5000,
                backend="cmdstanr",
                stanvars=stanvars)

summary(brms_fit)

ggplot(overall_metrics, aes(x = Class, y = RI)) +
  geom_point()

summary(brms_fit)
plot(brms_fit_class)
pp_check(brms_fit_class,ndraws = 100)

library(tidybayes)

pred_RI <- brms_fit %>% 
  epred_draws(newdata = expand_grid(TL = seq(25, 85, by = 5),
                                    #transmitter_id = "A69-1605-52",
                                    Sex = c("M", "F")))

ggplot(pred_RI, aes(x = TL, y = .epred, color = Sex)) +
  #geom_point(data = overall_metrics, aes(x = Class, y = RI)) +
  stat_lineribbon(alpha = 0.5, .width = .8) +
  scale_fill_brewer(palette = "Greys")  +
  #coord_cartesian(xlim = c(34,85)) +
  labs(x = "TL", y = "Predicted Roaming Index",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom")
  

# Fit the model
brms_fit_month <- brm(RI ~ 0 + Intercept + length_cm + sex + month +(1|transmitter_id), data=monthly_metrics,
                family=ord_beta_reg,
                cores=4,chains=3,
                prior = priors,
                refresh=0,
                iter = 5000,
                backend="cmdstanr",
                stanvars=stanvars)

summary(brms_fit_month)
plot(brms_fit_month)
pp_check(brms_fit_month,ndraws = 100)

library(tidybayes)

pred_RI_monthly <- brms_fit_month %>% 
  epred_draws(newdata = expand_grid(sex = c("M", "F"),
                                    transmitter_id = "A69-1605-52",
                                    length_cm = seq(25, 85, by = 5),
                                    month = seq(1,12, by = 1)))

ggplot(pred_RI_monthly, aes(x = as.factor(month), y = .epred, color = sex)) +
  geom_point(data = monthly_metrics, aes(x = as.factor(month), y = RI)) +
  stat_lineribbon(alpha = 0.5, .width = 0.95, color = "grey46") +
  #coord_cartesian(xlim = c(34,85)) +
  labs(x = "Month", y = "Predicted Roaming Index",
       fill = "Credible interval") +
  theme_clean() +
  theme(legend.position = "bottom")



# Roaming PLOTS -----------------------------------------------------------

quota_halves_sex_roam <- ggplot(overall_metrics, aes(x = Sex, y = RI)) +
  geom_half_point(aes(color = Sex), 
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 0.5, alpha = 0.5) +
  geom_half_boxplot(aes(fill = Sex), side = "r") + 
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Sex", y = "Roam Index") +
  theme_minimal()

quota_densities_sex_roam <- ggplot(overall_metrics, aes(x = RI, fill = Sex)) +
  geom_density(alpha = 0.6) +
  scale_x_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  labs(x = "Residency Index", y = "Density", fill = "Sex") +
  theme_clean() +
  theme(legend.position = "bottom")

roam_sex <- quota_halves_sex_roam | quota_densities_sex_roam

ggsave("plots/overall_roam_sex.png", dpi = 300)


## Lifestage
showtext_auto()

quota_halves_class <- ggplot(overall_metrics, aes(x = Class, y = residency_min)) +
  geom_half_point(aes(color = Sex), 
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 0.5, alpha = 0.5) +
  geom_half_boxplot(aes(fill = Sex), side = "r") + 
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Age Class", y = "Residency Index") +
  theme_minimal()

quota_densities_class <- ggplot(overall_metrics, aes(x = residency_min, fill = Class)) +
  geom_density(alpha = 0.6) +
  scale_x_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  labs(x = "Roam Index", y = "Density", fill = "Age Class") +
  theme_clean() +
  theme(legend.position = "bottom")

quota_halves_class | quota_densities_class


# Monthly residency
ggplot(monthly_metrics, aes(x = as.factor(month), y = RI)) +
  geom_half_point(aes(color = sex), 
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 0.5, alpha = 0.5) +
  geom_half_boxplot(aes(fill = sex), side = "r") + 
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none") +
  labs(x = "Month", y = "Roaming Index") +
  theme_clean()


ggplot(monthly_metrics, aes(x = RI, y = as.factor(month), fill = sex)) +
  geom_density_ridges(alpha = 0.6, scale = 0.8) +
  scale_x_continuous(labels = label_percent()) +
  labs(x = "Residency Index", y = "Density", fill = "Age Class") +
  theme_clean() +
  theme(legend.position = "bottom")

ggsave("plots/monthly_roam_sex.png", dpi = 300)
