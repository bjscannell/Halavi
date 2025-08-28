# Check distribution first
hist(overall_metrics$RI)

# Fit full Gaussian GLM
library(betareg)
glm1 <- betareg(RI ~ Sex * TL, data = overall_metrics)

# Check for overdispersion
dispersion <- sum(residuals(glm1, type = "pearson")^2) / df.residual(glm1)
print(dispersion)  # If > 1.5–2 → consider alternatives


# detection_data: transmitter_id, detection_date (Date format)

library(lubridate)
library(dplyr)

wilcox.test(monthly_res ~ sex, data = monthly_metrics)
kruskal.test(monthly_res ~ new_class, data = monthly_metrics)


library(lme4)
library(car)       
library(MuMIn)    



# Fit global model
glmm_global <- lmer(
  monthly_res ~ month + sex + length_cm + (1 | transmitter_id) + (1 | year),
  data = monthly_metrics
)

# Check multicollinearity
vif(glmm_global)

# Check model
summary(glmm_global)


# Set global options for dredge
options(na.action = "na.fail")

# Dredge generates all model combinations
dredge_results <- dredge(glmm_global)

# View top models
head(dredge_results)

# Get best model
best_model <- get.models(dredge_results, 1)[[1]]
summary(best_model)
confint(best_model, method = "Wald")

null_model <- lmer(monthly_res ~ 1 + (1 | transmitter_id) + (1 | year),
                   data = monthly_metrics)

anova(null_model, best_model)  # Likelihood ratio test


# spice it up -------------------------------------------------------------

# Transform to within (0, 1)
epsilon <- 1e-3
monthly_metrics <- monthly_metrics %>%
  mutate(monthly_res_trans = (monthly_res * (1 - 2 * epsilon)) + epsilon)

# Nonlinear effect using a spline
brm_nonlinear <- brm(
  formula = bf(
    monthly_res_trans ~ s(month) + (1 | transmitter_id) + (1 | year)
  ),
  data = monthly_metrics,
  family = Beta(),
  chains = 4, cores = 4, iter = 4000,
  seed = 123
)

summary(brm_nonlinear)
plot(brm_nonlinear)            
pp_check(brm_nonlinear)         # Posterior predictive checks
conditional_effects(brm_nonlinear, "month")  # Plot smooth trend


# -------------------------------
# Load Packages
# -------------------------------
library(brms)
library(tidyverse)
library(lubridate)
library(bayesplot)

# -------------------------------
# Data Prep
# -------------------------------

# Assuming your data frames are: overall_metrics and monthly_metrics
# Transform bounded variables to avoid 0 and 1 for Beta distribution
epsilon <- 1e-3

monthly_metrics <- monthly_metrics %>%
  mutate(
    monthly_res_trans = (monthly_res * (1 - 2 * epsilon)) + epsilon,
    length_z = scale(length_cm)
  )

overall_metrics <- overall_metrics %>%
  mutate(
    TL_z = scale(TL)
  )

# -------------------------------
# 1. Bayesian GLM for Roaming Index (RI) ~ Sex * TL
# -------------------------------

brm_glm1 <- brm(
  formula = RI ~ Sex * TL_z,
  data = overall_metrics,
  family = gaussian(),
  chains = 4, cores = 4, iter = 4000,
  seed = 123
)

summary(brm_glm1)
pp_check(brm_glm1)

# -------------------------------
# 2. Bayesian Equivalent of Wilcoxon Test
#    monthly_res ~ sex
# -------------------------------

brm_sex <- brm(
  formula = monthly_res_trans ~ sex,
  data = monthly_metrics,
  family = Beta(),
  chains = 4, cores = 4, iter = 4000,
  seed = 123
)

summary(brm_sex)

# -------------------------------
# 3. Bayesian Equivalent of Kruskal-Wallis Test
#    monthly_res ~ new_class
# -------------------------------

brm_class <- brm(
  formula = monthly_res_trans ~ new_class,
  data = monthly_metrics,
  family = Beta(),
  chains = 4, cores = 4, iter = 4000,
  seed = 123
)

summary(brm_class)

# -------------------------------
# 4. Bayesian Mixed Model for Monthly Residency
# -------------------------------

brm_glmm <- brm(
  formula = monthly_res_trans ~ s(month) + sex + length_z + (1 | transmitter_id) + (1 | year),
  data = monthly_metrics,
  family = Beta(),
  chains = 4, cores = 4, iter = 4000,
  seed = 123
)

summary(brm_glmm)
pp_check(brm_glmm)

# -------------------------------
# 5. Model Comparison (Bayesian LOO-CV)
# -------------------------------

# Null model
brm_null <- brm(
  formula = monthly_res_trans ~ 1 + (1 | transmitter_id) + (1 | year),
  data = monthly_metrics,
  family = Beta(),
  chains = 4, cores = 4, iter = 4000,
  seed = 123
)

# Reduced models for comparison
brm_model1 <- brm(
  formula = monthly_res_trans ~ month + (1 | transmitter_id) + (1 | year),
  data = monthly_metrics,
  family = Beta(),
  chains = 4, cores = 4, iter = 4000,
  seed = 123
)

brm_model2 <- brm(
  formula = monthly_res_trans ~ month + sex + (1 | transmitter_id) + (1 | year),
  data = monthly_metrics,
  family = Beta(),
  chains = 4, cores = 4, iter = 4000,
  seed = 123
)

# Compare all models using LOO
waic_null <- brm::loo(brm_null)
waic_model1 <- waic(brm_model1)
waic_model2 <- waic(brm_model2)
waic_full <- waic(brm_glmm)

loo_compare(loo_null, loo_model1, loo_model2, loo_full)

# -------------------------------
# 6. Posterior Plots for Group Differences (Sex/Class)
# -------------------------------

# Example: Plot difference between sexes from brm_sex
posterior <- as_draws_df(brm_sex)
diff_sex <- posterior %>% transmute(diff = b_sexM)

bayesplot::mcmc_areas(diff_sex, pars = "diff", prob = 0.95) +
  ggtitle("Posterior Distribution: Male vs Female Residency")

# Example: Pairwise differences from class model
posterior_class <- as_draws_df(brm_class)
diffs <- posterior_class %>%
  transmute(
    JUV_YOY = b_new_classJUV,        # Assuming YOY is reference
    ADULT_YOY = b_new_classADULT,
    ADULT_JUV = b_new_classADULT - b_new_classJUV
  )

bayesplot::mcmc_areas(diffs, prob = 0.95) +
  ggtitle("Posterior Distributions of Class Differences in Residency")

# -------------------------------
# Done!
# -------------------------------

