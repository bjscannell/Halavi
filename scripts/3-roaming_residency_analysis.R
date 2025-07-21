library(gghalves)
library(ggdist)
library(scales) 
library(ggbeeswarm)
library(patchwork)
library(brms)
library(tidybayes)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(ggpubr)
library(emmeans)
library(car)
library(mgcv)
library(gratia)  


# Residency Index ---------------------------------------------------------

# overall
resid <- res %>% 
  dplyr::select(transmitter_id, residency_min, residency_max, Class)

# monthly
monthly_res  <- dets %>% 
  mutate(month = month(detection_timestamp_utc),
         year = year(detection_timestamp_utc),
         month_day = format(as.Date(date), "%d-%m"),
         month_year = format(as.Date(date), "%m-%y")) %>% 
  group_by(transmitter_id, month_year) %>% 
  mutate(n = length(unique(month_day)),
         monthly_res = n/(days_in_month(month)),
         monthly_res = ifelse(monthly_res > 1, 1, monthly_res)) %>% ungroup(month_year) %>% 
  mutate(first_det = min(detection_timestamp_utc),
         last_det = max(detection_timestamp_utc)) %>% 
  distinct(transmitter_id, month_year, .keep_all = T) %>% 
  select(transmitter_id, year, month, otn_array, sex, length_cm, 
         length2_cm, new_class, monthly_res, first_det, last_det,n, month_year) %>% 
  ## This is to fix the first month residency index
  mutate(first_day = day(first_det),
         first_month = month(first_det),
         first_year = year(first_det))  %>% 
  mutate(is_first_month = if_else(month == first_month & year == first_year, 1,0),
         first_month_day_count = if_else(is_first_month == 1, days_in_month(month)-first_day + 1, NA),
         monthly_res = if_else(is_first_month == 1, n/first_month_day_count, monthly_res)) 



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



# Do overall metrics differ by sex or class? --------------------------------------

showtext_auto()

# residency
res_overall <- ggplot(overall_metrics, aes(x = Class, y = residency_min)) +
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

# roaming
showtext_auto()

roam_overall <- ggplot(overall_metrics, aes(x = Class, y = RI)) +
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

#' some stats that say that there is no difference in sex but there 
#' is a difference between age classes




# Overall Models ------------------------------------------------------------------

# residency overall
beta_model <- brm(
  formula = residency_min ~ TL + Sex + Class + (1|transmitter_id),
  data = overall_metrics,
  family = Beta(),
  chains = 4,
  cores = 4,
  iter = 5000,
  seed = 123
)

plot(beta_model)
pp_check(beta_model,ndraws = 100)
summary(beta_model)


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



# roaming overall
brms_fit <- brm(RI ~ 0 + Intercept + TL + Sex , data=overall_metrics,
                family=ord_beta_reg,
                cores=4,chains=4,
                prior = priors,
                refresh=0,
                iter = 5000,
                backend="cmdstanr",
                stanvars=stanvars)

summary(brms_fit)
plot(brms_fit)
pp_check(brms_fit, ndraws = 100)

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


# Monthly models ----------------------------------------------------------

monthly_metrics <- monthly_metrics %>%
  mutate(
    sex = factor(sex),
    new_class = factor(new_class),
    transmitter_id = factor(transmitter_id),
    month = as.numeric(month)  # make sure month is numeric
  ) %>%
  drop_na(monthly_res, length_cm)

# residency
gam_mon_res <- gam(
  monthly_res ~ length_cm + sex + new_class + s(month) + s(transmitter_id, bs = "re"),
  data = monthly_metrics,
  family = quasibinomial(link = "logit"),
  method = "REML"
)

summary(gam_mon_res)
appraise(gam_mon_res)
draw(gam_mon_res, select = c(1, 2))  # Only draw smooth terms, skip random effects


newdata_month <- data.frame(
  length_cm = mean(monthly_metrics$length_cm, na.rm = TRUE),
  month = seq(1, 12, by = 0.1),
  transmitter_id = "A69-1605-58"                    
)


pred_monthly_res <- predict(gam4_qb, newdata = newdata_month, se.fit = TRUE, type = "response")

newdata_monthly_res <- newdata_month %>%
  mutate(fit = pred_monthly_res$fit,
         se = pred_monthly_res$se.fit,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se)


res_gam <- ggplot(newdata_monthly_res, aes(x = month, y = fit)) +
  #geom_swarm(data = monthly_metrics, aes(x = month, y = monthly_res), cex = 3) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "green", alpha = 0.3) +
  labs(title = "Predicted Monthly Residency",
       x = "Month", y = "Predicted Monthly Residency") +
  theme_minimal(base_size = 14)


# roaming

gam_RI <- gam(
  RI ~ length_cm + s(month) + s(transmitter_id, bs = "re"),
  data = monthly_metrics,
  family = quasibinomial(link = "logit"),
  method = "REML"
)

summary(gam_RI)
appraise(gam_RI)
draw(gam_RI, select = c(1, 2))  # Only draw smooth terms, skip random effects


newdata_month <- data.frame(
  length_cm = mean(monthly_metrics$length_cm, na.rm = TRUE),
  month = seq(1, 12, by = 0.1),
  transmitter_id = "A69-1605-52"                     # ignore random effect
)


pred_monthRI <- predict(gam_RI, newdata = newdata_month, se.fit = TRUE, type = "response")

newdata_monthRI <- newdata_month %>%
  mutate(fit = pred_monthRI$fit,
         se = pred_monthRI$se.fit,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se)


roam_gam <- ggplot(newdata_monthRI, aes(x = month, y = fit)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "green", alpha = 0.3) +
  labs(title = "Predicted Roaming Index by Month",
       x = "Month", y = "Predicted Monthly Roaming Index") +
  theme_minimal(base_size = 14)



roam_gam | res_gam
