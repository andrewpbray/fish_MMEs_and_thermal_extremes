library(tidyverse)
library(lme4)
library(broom)
library(bestglm)

setwd("data/processed/")
historical_data <- read_csv("historical_data.csv",
                            col_types = list(wbic = col_factor(NULL),
                                             month  = col_factor(NULL),
                                             season = col_factor(NULL),
                                             summerkill = col_factor(NULL),
                                             ice_duration = col_double()))

# logistic reg

h <- historical_data %>%
  dplyr::select(wbic, variance_after_ice_30, variance_after_ice_60,
         stratified_period_count, log_schmidt, cumulative_above_10,
         ice_duration, summerkill, population, lon, lat, season,
         temp_index)

h2 <- h %>%
  mutate_if(is.numeric, scale)

h3 <- h %>%
  group_by(season) %>%
  mutate_if(is.numeric, scale)

# logistic regression w no season or wbic

m1 <- glm(summerkill ~ . - wbic - season, 
          data = h2, family = "binomial")
write_rds(m1, "../models/vanilla_model_1.rds")

m1p5 <- glm(summerkill ~ . - wbic - season, 
          data = h3, family = "binomial")

# m1 w fixed effect for season
m2 <- glm(summerkill ~ . -wbic, 
          data = h3, family = "binomial")
write_rds(m2, "../models/vanilla_model_2.rds")

m2p5 <- glm(summerkill ~ . -wbic, 
          data = h2, family = "binomial")


# m2 w leaps and bounds selection algorithm
h4 <- h3[complete.cases(h3),]
y <- h4$summerkill == "pos"
xy <- h4 %>%
  dplyr::select(-wbic) %>%
  model.matrix(y ~ ., data = .) %>%
  cbind(y)
xy <- as.data.frame(xy)[,-1]
xy$y <- as.integer(xy$y)
  
m3 <- bestglm(Xy = xy,
        family = binomial,
        IC = "AIC",
        method = "exhaustive")


# m2 w random effect for lake
m4 <- glmer(summerkill ~ . - wbic + (1|wbic),
             data = h2, family = binomial, 
             control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 10)

m4_seasonally_scaledb <- glmer(summerkill ~ . - wbic + (1|wbic),
            data = h3, family = binomial, 
            control = glmerControl(optimizer = "bobyqa"),
            nAGQ = 10)



# compare models
stack_models <- bind_rows(tidy(m1), 
                          tidy(m2), 
                          tidy(m1p5),
                          tidy(m4_seasonally_scaled)) %>%
  add_column(model_num = rep(c("Model 1", "Model 2", "Model 3", "Model 4"),
                             c(nrow(tidy(m1)), 
                               nrow(tidy(m2)), 
                               nrow(tidy(m1p5)),
                               nrow(tidy(m4_seasonally_scaled)))))

stack_models %>%
  filter(term != "(Intercept)", p.value < .05) %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  facet_grid(~ model_num) +
  labs(
    x = NULL
  ) +
  theme_bw()


# simple model for fig 2

h <- historical_data %>%
  dplyr::select(wbic, variance_after_ice_30, variance_after_ice_60,
                log_schmidt, cumulative_above_10, ice_duration,
                summerkill, population, lon, lat, season, temp)

h2 <- h %>%
  mutate_if(is.numeric, scale)

m1 <- glm(summerkill ~ . - wbic, 
          data = h2, family = "binomial")
