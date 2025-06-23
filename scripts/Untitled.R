save(dets, res, file="data.RData")
load("data.RData")


overall_metrics <- overall_metrics %>%  filter(TL < 70)
monthly_metrics <- monthly_metrics %>%  filter(length_cm < 70)

ggplot(overall_metrics) +
  geom_smooth(method = "lm", aes(x=TL, y = RI, colour = Sex)) +
  geom_point(aes(x=TL, y = RI)) +
  theme_minimal()




library(tidyverse)
library(ggplot2)
library(broom)
library(ggpubr)
library(emmeans)
library(car)


overall_metrics <- overall_metrics %>%
  mutate(Sex = factor(Sex),
         Class = factor(Class))

# Quick check of structure
str(overall_metrics)
summary(overall_metrics)


# Boxplots
ggplot(overall_metrics, aes(x = Sex, y = residency_min, fill = Class)) +
  geom_boxplot() +
  labs(title = "Residency Min by Sex and Class") +
  theme_minimal()

ggplot(overall_metrics, aes(x = Sex, y = RI, fill = Class)) +
  geom_boxplot() +
  labs(title = "RI by Sex and Class") +
  theme_minimal()


ggplot(overall_metrics, aes(x = TL, y = residency_min)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

ggplot(overall_metrics, aes(x = TL, y = RI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()


kruskal.test(residency_min ~ interaction(Sex, Class), data = overall_metrics)
kruskal.test(RI ~ interaction(Sex, Class), data = overall_metrics)

library(brms)

# Model monthly_res (or residency_min) ~ predictors + random effect on transmitter_id
# Use zero-one inflated beta to allow 0 and 1 values

# Fit beta regression model
fit_beta <- brm(
  residency_min ~ TL + Sex + Class + (1 | transmitter_id),
  data = overall_metrics,
  family = Beta(),
  cores = 4, chains = 4, iter = 2000,
  control = list(adapt_delta = 0.95)
)


summary(fit_beta)
# Plot marginal effect of TL
# marg_eff <- marginal_effects(fit_beta, effects = "TL", probs = c(0.1, 0.9))
# plot(marg_eff, plot = FALSE)[[1]] + 
#   ggtitle("Marginal Effect of TL on Residency") +
#   theme_minimal()

# marg_eff_interact <- marginal_effects(fit_beta, effects = "TL:Sex")
# plot(marg_eff_interact, plot = FALSE)[[1]] + 
#   ggtitle("Interaction: TL by Sex") +
#   theme_minimal()


# Create prediction data
newdata <- expand.grid(
  TL = seq(min(overall_metrics$TL), max(overall_metrics$TL), length.out = 100),
  Sex = unique(overall_metrics$Sex),
  Class = unique(overall_metrics$Class),
  transmitter_id = "A69-1605-52"  # marginalize over random effect
)

# Get predicted posterior means
preds <- posterior_epred(fit_beta, newdata = newdata, re_formula = NA)

# Summarize predictions
pred_summary <- data.frame(
  newdata,
  pred_mean = apply(preds, 2, mean),
  pred_lower = apply(preds, 2, quantile, probs = 0.05),
  pred_upper = apply(preds, 2, quantile, probs = 0.95)
)



ggplot() +
  geom_point(data = overall_metrics, aes(x = TL, y = residency_min, color = Sex), alpha = 0.5) +
  geom_line(data = pred_summary, aes(x = TL, y = pred_mean, color = Sex), size = 1) +
  geom_ribbon(data = pred_summary, aes(x = TL, ymin = pred_lower, ymax = pred_upper, fill = Sex), alpha = 0.2) +
  labs(title = "Predicted Residency by TL and Sex", y = "Residency (Beta scale)", x = "TL") +
  theme_minimal()


# monthly -----------------------------------------------------------------

library(tidyverse)
library(mgcv)
library(gratia)  # for visualization and diagnostics


monthly_metrics <- monthly_metrics %>%
  mutate(
    sex = factor(sex),
    new_class = factor(new_class),
    transmitter_id = factor(transmitter_id),
    month = as.numeric(month)  # make sure month is numeric
  ) %>%
  drop_na(monthly_res, length_cm)


gam4_qb <- gam(
  monthly_res ~ length_cm + s(month) + s(transmitter_id, bs = "re"),
  data = monthly_metrics,
  family = quasibinomial(link = "logit"),
  method = "REML"
)

summary(gam4_qb)
appraise(gam4_qb)
draw(gam4_qb, select = c(1, 2))  # Only draw smooth terms, skip random effects

# Generate prediction data across TL range
newdata_month <- data.frame(
  length_cm = mean(monthly_metrics$length_cm, na.rm = TRUE),
  month = seq(1, 12, by = 0.1),
  transmitter_id = "A69-1605-58"                    
)

# Predict
pred_monthly_res <- predict(gam4_qb, newdata = newdata_month, se.fit = TRUE, type = "response")

newdata_monthly_res <- newdata_month %>%
  mutate(fit = pred_monthly_res$fit,
         se = pred_monthly_res$se.fit,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se)

# Plot
ggplot(newdata_monthly_res, aes(x = month, y = fit)) +
  #geom_point(data = monthly_metrics, aes(x = month, y = monthly_res)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "green", alpha = 0.3) +
  labs(title = "Effect of Total Length on Monthly Residency",
       x = "Month", y = "Predicted Monthly Residency") +
  theme_minimal(base_size = 14)



library(tidyverse)
library(mgcv)
library(gratia)  # for visualization and diagnostics


monthly_metrics <- monthly_metrics %>%
  mutate(
    sex = factor(sex),
    new_class = factor(new_class),
    transmitter_id = factor(transmitter_id),
    month = as.numeric(month)  # make sure month is numeric
  ) %>%
  drop_na(monthly_res, length_cm)

gam_RI <- gam(
  monthly_res ~ length_cm + s(month) + s(transmitter_id, bs = "re"),
  data = monthly_metrics,
  family = quasibinomial(link = "logit"),
  method = "REML"
)
summary(gam_RI)
appraise(gam_RI)
draw(gam_RI, select = c(1, 2))  # Only draw smooth terms, skip random effects

# Generate prediction data across TL range
newdata_month <- data.frame(
  length_cm = mean(monthly_metrics$length_cm, na.rm = TRUE),
  month = seq(1, 12, by = 0.1),
  transmitter_id = "A69-1605-52"                     # ignore random effect
)

# Predict
pred_monthRI <- predict(gam_RI, newdata = newdata_month, se.fit = TRUE, type = "response")

newdata_monthRI <- newdata_month %>%
  mutate(fit = pred_monthRI$fit,
         se = pred_monthRI$se.fit,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se)

# Plot
ggplot(newdata_monthRI, aes(x = month, y = fit)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "green", alpha = 0.3) +
  labs(title = "Effect of Total Length on Monthly Residency",
       x = "Month", y = "Predicted Monthly Residency") +
  theme_minimal(base_size = 14)



# net ---------------------------------------------------------------------

library(dplyr)
library(lubridate)

# Order detections by animal and time
dets_n <- dets %>%
  arrange(transmitter_id, detection_timestamp_utc) %>%
  group_by(transmitter_id) %>%
  mutate(next_station = lead(station_no),
         next_time = lead(detection_timestamp_utc)) %>%
  filter(station_no != next_station) %>%
  ungroup()

edge_list <- dets_n %>%
  filter(!is.na(next_station)) %>%
  select(transmitter_id, station_no, next_station, sex, life_stage) %>%
  rename(from = station_no, to = next_station)

library(tidygraph)
library(ggraph)

# Create graph object
graph <- as_tbl_graph(edge_list, directed = TRUE)

# Add node metadata (station coordinates)
nodes_df <- dets %>%
  select(station_no, deploy_lat, deploy_long) %>%
  distinct() %>%
  rename(name = station_no)

graph <- graph %>%
  activate(nodes) %>%
  left_join(nodes_df, by = "name")


graph <- graph %>%
  activate(nodes) %>%
  mutate(
    degree_centrality = centrality_degree(mode = "all"),
    betweenness_centrality = centrality_betweenness(),
    eigen_centrality = centrality_eigen()
  ) 



as_tibble(graph, active = "nodes") %>%
  ggplot(aes(x = deploy_long, y = deploy_lat)) +
  geom_point(aes(size = degree_centrality, color = betweenness_centrality), alpha = 0.8) +
  scale_color_viridis_c(option = "D", name = "Betweenness") +
  scale_size(range = c(2, 8), name = "Degree") +
  labs(title = "Map of Node Centrality",
       x = "Longitude", y = "Latitude") +
  theme_minimal()



# random
# Your directed graph with coordinates
graph_obs <- as_tbl_graph(edge_list, directed = TRUE) %>%
  activate(nodes) %>%
  left_join(nodes_df, by = "name") %>%
  mutate(degree = centrality_degree(mode = "all"),
         betweenness = centrality_betweenness(),
         eigen = centrality_eigen())

# Extract observed centrality metrics
df_obs <- as_tibble(graph_obs, active = "nodes") %>%
  select(name, degree, betweenness, eigen) %>%
  mutate(type = "observed")

library(future)
library(furrr)
plan(multisession)  # Or use plan(multicore) on Linux/Mac

# Convert observed graph to igraph
igraph_obs <- graph_obs

# Function to randomize and calculate centralities
randomize_graph_metrics <- function(i) {
  library(dplyr)
  library(tidygraph)
  library(ggraph)
  set.seed(123)
  ig_rand <- rewire(igraph_obs, keeping_degseq(niter = gsize(igraph_obs) * 10))
  tbl_rand <- as_tbl_graph(ig_rand) %>%
    activate(nodes) %>%
    left_join(nodes_df, by = "name") %>%
    mutate(degree = centrality_degree(mode = "all"),
           betweenness = centrality_betweenness(),
           eigen = centrality_eigen()) %>%
    as_tibble() %>%
    select(name, degree, betweenness, eigen) %>%
    mutate(type = paste0("rand_", i))
  
  return(tbl_rand)
}

# Run in parallel with furrr
n_iter <- 100  # Number of randomizations

rand_results <- future_map(1:n_iter, randomize_graph_metrics, .progress = TRUE)
df_rand <- bind_rows(rand_results) %>%
  mutate(type = "random")


df_compare <- bind_rows(df_obs, df_rand)

library(ggplot2)
ggplot(df_compare, aes(x = degree, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Degree Centrality: Observed vs Randomized",
       x = "Degree", y = "Density") +
  theme_minimal()

ggplot(df_compare, aes(x = betweenness, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Betweenness Centrality: Observed vs Randomized",
       x = "Betweenness", y = "Density") +
  theme_minimal()


# Compare mean observed to mean of randomized metrics
obs_mean <- df_obs %>% summarise(across(degree:eigen, mean))
rand_mean <- df_rand %>% summarise(across(degree:eigen, mean))

# Optionally run Wilcoxon or permutation tests
wilcox.test(df_obs$degree, df_rand$degree)
wilcox.test(df_obs$betweenness, df_rand$betweenness)
wilcox.test(df_obs$eigen, df_rand$eigen)

rand_summ <- df_rand %>%
  group_by(name) %>%
  summarise(across(c(degree, betweenness, eigen), list(mean = mean, sd = sd), .names = "{.col}_{.fn}"))

obs_vs_rand <- left_join(df_obs, rand_summ, by = "name") %>%
  mutate(
    degree_z = (degree - degree_mean) / degree_sd,
    betweenness_z = (betweenness - betweenness_mean) / betweenness_sd,
    eigen_z = (eigen - eigen_mean) / eigen_sd
  )



# emdg --------------------------------------------------------------------



transitions <- dets %>%
  arrange(transmitter_id, detection_timestamp_utc) %>%
  group_by(transmitter_id) %>%
  mutate(next_station = lead(station_no)) %>%
  filter(!is.na(next_station) & station_no != next_station) %>%
  ungroup()

transition_counts <- transitions %>%
  count(station_no, next_station, name = "n_transitions")

transition_probs <- transition_counts %>%
  group_by(station_no) %>%
  mutate(prob = n_transitions / sum(n_transitions)) %>%
  ungroup()

library(ggplot2)

ggplot(transition_probs, aes(x = station_no, y = next_station, fill = prob)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Pij") +
  labs(title = "Global Markov Chain: Transition Probabilities",
       x = "From Station", y = "To Station") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



