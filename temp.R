temp_rec <- raw %>% filter(record_type == "TEMP") %>% select(field_2,field_7, field_8) %>% 
  mutate(datetime = ymd_hms(field_2),
         receiver = field_7,
         temp = as.numeric(field_8),
         month = month(datetime)) %>% 
  drop_na() %>% 
# over the daily quantiles??
# for now lets just do monthly averages
  group_by(month) %>% 
  mutate(monthly_temp = mean(temp)) %>% 
  distinct(month, .keep_all = T)
  
  
df <- monthly_metrics %>% left_join(temp_rec)

# GAM with temp
gam_mon_res <- gam(
  monthly_res ~ length_cm + sex + new_class + s(temp) +s(month) + s(transmitter_id, bs = "re"),
  data = df,
  family = quasibinomial(link = "logit"),
  method = "REML"
)

# we have to remove month because of "multicolinearity"
concurvity(gam_mon_res, full = TRUE)

# temperature is super correlated with month
# equivalent of multicolinearity
gam_mon_res <- gam(
  monthly_res ~ length_cm + sex + new_class + s(temp)+ s(transmitter_id, bs = "re"),
  data = df,
  family = quasibinomial(link = "logit"),
  method = "REML"
)



summary(gam_mon_res)
appraise(gam_mon_res)
draw(gam_mon_res, select = c(1, 2))  # Only draw smooth terms, skip random effects



new_temp <- data.frame(
  temp = seq(min(df$temp, na.rm = TRUE), max(df$temp, na.rm = TRUE), length.out = 100),
  month = median(df$month, na.rm = TRUE),
  length_cm = median(df$length_cm, na.rm = TRUE),
  sex = "M",
  new_class = "JUV",
  transmitter_id = "A69-1605-58"  # exclude RE for population-level effect
)

new_temp <- predict(gam_mon_res, newdata = new_temp, type = "response", se.fit = TRUE)


newdata_temp_res <- new_temp %>%
  mutate(fit = pred_monthly_res$fit,
         se = pred_monthly_res$se.fit,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se)


ggplot(new_temp, aes(x = temp, y = pred)) +
  geom_line() +
  labs(title = "Predicted Residency vs Temperature",
       x = "Temperature",
       y = "Predicted Probability of Residency")











pred_monthly_res <- predict(gam4_qb, newdata = newdata_month, se.fit = TRUE, type = "response")

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
  geom_swarm(data = monthly_metrics, 
             aes(x = month, y = monthly_res,
                 fill = new_class, color = new_class), cex = 3) +
  geom_point(data = avgs, aes(x = month, y = month_avg)) +
  # geom_point(data = avgs, aes(x = month, y = upper)) +
  # geom_point(data = avgs, aes(x = month, y = lower)) +
  geom_segment(data = avgs, aes(x = month, y = lower, xend = month, yend = upper)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "green", alpha = 0.3) +
  labs(title = "Predicted Monthly Residency",
       x = "Month", y = "Predicted Monthly Residency") +
  theme_minimal(base_size = 14)


