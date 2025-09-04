library(gghalves)
library(ggdist)
library(scales) 
library(ggbeeswarm)
library(patchwork)
library(brms)
library(tidybayes)
library(tidyr)
library(betareg)
library(glmmTMB)
library(ggeffects)
library(DHARMa)
library(lme4)
library(car)       
library(MuMIn)    
library(tidyverse)
library(ggplot2)
library(broom)
library(ggpubr)
library(emmeans)
library(car)
library(mgcv)
library(gratia)  
library(showtext)



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

#' showtext_auto()
#' 
#' # residency
#' res_overall <- ggplot(overall_metrics, aes(x = Class, y = residency_min)) +
#'   geom_half_point(aes(color = Sex), 
#'                   transformation = position_quasirandom(width = 0.1),
#'                   side = "l", size = 0.5, alpha = 0.5) +
#'   geom_half_boxplot(aes(fill = Sex), side = "r") + 
#'   scale_y_continuous(labels = label_percent()) +
#'   scale_fill_viridis_d(option = "plasma", end = 0.8) +
#'   scale_color_viridis_d(option = "plasma", end = 0.8) +
#'   guides(color = "none", fill = "none") +
#'   labs(x = "Age Class", y = "Residency Index") +
#'   theme_minimal()
#' 
#' # roaming
#' showtext_auto()
#' 
#' roam_overall <- ggplot(overall_metrics, aes(x = Class, y = RI)) +
#'   geom_half_point(aes(color = Sex), 
#'                   transformation = position_quasirandom(width = 0.1),
#'                   side = "l", size = 0.5, alpha = 0.5) +
#'   geom_half_boxplot(aes(fill = Sex), side = "r") + 
#'   scale_y_continuous(labels = label_percent()) +
#'   scale_fill_viridis_d(option = "plasma", end = 0.8) +
#'   scale_color_viridis_d(option = "plasma", end = 0.8) +
#'   guides(color = "none", fill = "none") +
#'   labs(x = "Age Class", y = "Roam Index") +
#'   theme_minimal()
#' 
#' #' some stats that say that there is no difference in sex but there 
#' #' is a difference between age classes




# Overall Models ------------------------------------------------------------------

# overall_metrics <- overall_metrics |>
#   dplyr::mutate(
#     TL_s = as.numeric(scale(TL)),          # scale
#     SexM = ifelse(Sex == "M", 1, 0),   
#     ClassYOY = ifelse(Class == "YOY", 1, 0)
#   )
# 
# # residency overall
# beta_model <- brm(
#   formula = residency_min ~ TL + Sex + Class + (1|transmitter_id),
#   data = overall_metrics,
#   family = Beta(),
#   chains = 4,
#   cores = 4 ,
#   warmup = 5000,
#   iter = 25000,
#   seed = 123
# )
# 
# plot(beta_model)
# pp_check(beta_model,ndraws = 100)
# summary(beta_model)
# 
# 
# pred_res <- beta_model %>% 
#   epred_draws(newdata = expand_grid(Sex = c("M", "F"),
#                                     transmitter_id = "A69-1605-52",
#                                     TL = seq(25, 70, by = 5)))
# 
# ggplot(pred_res, aes(x = TL, y = .epred, color = Sex)) +
#   geom_point(data = overall_metrics, aes(x = TL, y = residency_min)) +
#   stat_lineribbon(alpha = 0.3, .width = 0.95, fill = "gray65") +
#   scale_fill_brewer(palette = "Greys")  +
#   coord_cartesian(xlim = c(34,70)) +
#   labs(x = "TL", y = "Predicted Residency Index",
#        fill = "Credible interval") +
#   theme_bw() +
#   theme(legend.position = "bottom")
# 
# 
# 
# # roaming overall
# brms_fit <- brm(RI ~ 0 + Intercept + TL + Sex , data=overall_metrics,
#                 family=ord_beta_reg,
#                 cores=4,chains=4,
#                 prior = priors,
#                 refresh=0,
#                 iter = 5000,
#                 backend="cmdstanr",
#                 stanvars=stanvars)
# 
# summary(brms_fit)
# plot(brms_fit)
# pp_check(brms_fit, ndraws = 100)
# 
# pred_RI <- brms_fit %>% 
#   epred_draws(newdata = expand_grid(TL = seq(25, 85, by = 5),
#                                     #transmitter_id = "A69-1605-52",
#                                     Sex = c("M", "F")))
# 
# ggplot(pred_RI, aes(x = TL, y = .epred, color = Sex)) +
#   #geom_point(data = overall_metrics, aes(x = Class, y = RI)) +
#   stat_lineribbon(alpha = 0.5, .width = .8) +
#   scale_fill_brewer(palette = "Greys")  +
#   #coord_cartesian(xlim = c(34,85)) +
#   labs(x = "TL", y = "Predicted Roaming Index",
#        fill = "Credible interval") +
#   theme_minimal() +
#   theme(legend.position = "bottom")


# I think everything needed is below here ---------------------------------

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
  monthly_res ~ length_cm + sex + s(month, bs = "cc") + s(transmitter_id, bs = "re"),
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
  transmitter_id = "A69-1605-58",
  sex = c("F")
)

# plot the model
res_predicts <- data.frame(ggpredict(gam_mon_res, terms = c("month")))

plot(ggpredict(gam_mon_res, terms = c("month")))


pred_monthly_res <- predict(gam_mon_res, newdata = newdata_month, se.fit = TRUE, type = "response")

newdata_monthly_res <- newdata_month %>%
  mutate(fit = pred_monthly_res$fit,
         se = pred_monthly_res$se.fit,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se)

avgs <- monthly_metrics %>% 
  ungroup() %>% group_by(month) %>% 
  mutate(month_avg = mean(monthly_res),
         upper = quantile(monthly_res,0.9),
         lower = quantile(monthly_res,0.1)) %>% 
  distinct(month, .keep_all = T)

res_gam <- ggplot(newdata_monthly_res, aes(x = month, y = fit)) +
  # geom_swarm(data = monthly_metrics, 
  #            aes(x = month, y = monthly_res,
  #                fill = new_class, color = new_class), cex = 3) +
  #geom_point(data = avgs, aes(x = month, y = month_avg)) +
  # geom_point(data = avgs, aes(x = month, y = upper)) +
  # geom_point(data = avgs, aes(x = month, y = lower)) +
  #geom_segment(data = avgs, aes(x = month, y = lower, xend = month, yend = upper)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "green", alpha = 0.3) +
  labs(title = "Predicted Monthly Residency",
       x = "Month", y = "Predicted Monthly Residency") +
  theme_minimal(base_size = 14)


# 
# ggplot(newdata_monthly_res, aes(x = month, y = fit)) +
#   geom_swarm(data = monthly_metrics, 
#              aes(x = month, y = monthly_res, fill = new_class, color = new_class)) #+
  # geom_point(data = monthly_metrics, 
  #            aes(x = jitter(month), y = monthly_res, color = new_class)) 

# +
#   geom_text_repel(data = monthly_metrics %>% filter(monthly_res < 0.25 & month ==7),
#                   aes(x = month, y = monthly_res, label = length_cm))
  



# roaming
# 
# gam_RI <- gam(
#   RI ~ length_cm + s(month) + s(transmitter_id, bs = "re"),
#   data = monthly_metrics,
#   family = quasibinomial(link = "logit"),
#   method = "REML"
# )
# 
# summary(gam_RI)
# appraise(gam_RI)
# draw(gam_RI, select = c(1, 2))  # Only draw smooth terms, skip random effects
# 
# 
# newdata_month <- data.frame(
#   length_cm = mean(monthly_metrics$length_cm, na.rm = TRUE),
#   month = seq(1, 12, by = 0.1),
#   transmitter_id = "A69-1605-52"                     # ignore random effect
# )
# 
# 
# pred_monthRI <- predict(gam_RI, newdata = newdata_month, se.fit = TRUE, type = "response")
# 
# newdata_monthRI <- newdata_month %>%
#   mutate(fit = pred_monthRI$fit,
#          se = pred_monthRI$se.fit,
#          lower = fit - 1.96 * se,
#          upper = fit + 1.96 * se)
# 
# 
# roam_gam <- ggplot(newdata_monthRI, aes(x = month, y = fit)) +
#   geom_line(color = "darkgreen", linewidth = 1.2) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "green", alpha = 0.3) +
#   labs(title = "Predicted Roaming Index by Month",
#        x = "Month", y = "Predicted Monthly Roaming Index") +
#   theme_minimal(base_size = 14)



#roam_gam | res_gam




# ugh ---------------------------------------------------------------------

# Check distribution first
hist(overall_metrics$residency_min)

# Fit full model

glmmfull_res <- glmmTMB(residency_min ~ Sex:TL + TL + Sex + (1|transmitter_id),
                    data = overall_metrics, family=beta_family(link="logit"))

# check the model
simres <- simulateResiduals(fittedModel = glmmfull_res, n = 1000)
plot(simres)


testUniformity(simres)   
testDispersion(simres)   
testZeroInflation(simres) 

# plot the model
res_predicts <- data.frame(ggpredict(glmmfull_res, terms = c("TL")))

ggplot() +
  geom_line(data = res_predicts, aes(x = x, y = predicted)) +
  geom_ribbon(data = res_predicts,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  geom_point(data = overall_metrics, aes(x = TL, y = residency_min)) +
  labs(x = "Total Length (cm)",
       y = "Residency Minimum") +
  theme_bw() 

plot(ggpredict(glmmfull_res, terms = c("TL")))




# Temperature -------------------------------------------------------------

temp_rec <- raw %>% filter(record_type == "TEMP") %>% select(field_2, field_7, field_8) %>% 
  mutate(datetime = ymd_hms(field_2),
         receiver = field_7,
         temp = as.numeric(field_8),
         month = month(datetime)) %>% 
  drop_na() %>% 
  filter(datetime > ymd_hms("2022-09-03 16:01:43 EDT")) %>% ungroup() %>%
  # over the daily quantiles??
  # for now lets just do monthly averages
  dplyr::group_by(month) %>% 
  dplyr::mutate(monthly_temp = mean(temp)) %>% 
  ungroup() %>%
  distinct(month, .keep_all = T)


df <- monthly_metrics %>% left_join(temp_rec) %>% 
  mutate(month = as.numeric(month))

# GAM with temp
gam_mon_res <- gam(
  monthly_res ~ length_cm + sex  + s(monthly_temp) +s(month) + s(transmitter_id, bs = "re"),
  data = df,
  family = quasibinomial(link = "logit"),
  method = "REML"
)

# we have to remove month because of "multicolinearity"
concurvity(gam_mon_res, full = TRUE)

# temperature is super correlated with month
# equivalent of multicolinearity
gam_temp_res <- gam(
  monthly_res ~ length_cm + sex + new_class + s(temp)+ s(transmitter_id, bs = "re"),
  data = df,
  family = quasibinomial(link = "logit"),
  method = "REML"
)


summary(gam_temp_res)
appraise(gam_temp_res)
draw(gam_temp_res, select = c(1, 2))  # Only draw smooth terms, skip random effects



new_temp <- data.frame(
  temp = seq(min(df$temp, na.rm = TRUE), max(df$temp, na.rm = TRUE), length.out = 100),
  #month = median(df$month, na.rm = TRUE),
  length_cm = median(df$length_cm, na.rm = TRUE),
  sex = "M",
  new_class = "JUV",
  transmitter_id = "A69-1605-58"  # exclude RE for population-level effect
)

new_temp_fit <- predict(gam_mon_res, newdata = new_temp, type = "response", se.fit = TRUE)


pred <- predict.gam(gam_mon_res,new_temp, type = "response", se.fit = TRUE)


newdata_temp_res <- data.frame(pred) %>%
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit,
         temp = new_temp$temp)


ggplot(newdata_temp_res, aes(x = temp, y = fit)) +
  geom_line() +
  labs(title = "Predicted Residency vs Temperature",
       x = "Temperature",
       y = "Predicted Probability of Residency")






ggplot(newdata_monthly_res, aes(x = month, y = fit)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "green", alpha = 0.3) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  labs(title = "Predicted Monthly Residency",
       x = "Month", y = "Predicted Monthly Residency") +
  theme_minimal(base_size = 14) 


ggplot(temp_rec, aes(month, monthly_temp)) + 
  geom_smooth(se=F) +
  scale_y_continuous(position = "right") +
  scale_x_continuous(breaks = seq(1,12,1)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

ggsave("temp.png")
