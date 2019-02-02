# Load libraries
library(tidyverse)
library(broom)

# Load models
setwd("../data/models/") 
model_names <- c("full_logistic_f1.rds",
                 "full_ridge_f1_logloss_downsampled.rds",
                 "full_lasso_f1_logloss_downsampled.rds")
full_models <- model_names %>%
  map(., ~read_rds(.x))
full_models <- tibble(name  = str_sub(model_names, 1, -5),
                       model = full_models)

# Extract coefficients from each model.
extract_coef <- function(model, name) {
  if ("train" %in% class(model)) {
    m <- model$finalModel %>%
      coef(model$bestTune$lambda)
    out <- tibble(term = row.names(m),
                  estimate = m[, 1]) %>%
      filter(term != "(Intercept)") %>%
      select(term, estimate)
  }
  if ("glmerMod" %in% class(model)) {
    out <- model %>%
      tidy() %>%
      filter(group == "fixed") %>%
      filter(term != "(Intercept)") %>%
      filter(p.value <= .3) %>%
      select(term, estimate)
  }
  if ("glm" %in% class(model)) {
    out <- model %>%
      tidy() %>%
      filter(term != "(Intercept)") %>%
      select(term, estimate)
  }
  out
}

full_models <- full_models %>%
  mutate(coef = map2(model, name, extract_coef),
         name = factor(name, levels = c("full_logistic_f1",
                                        "full_ridge_f1_logloss_downsampled",
                                        "full_lasso_f1_logloss_downsampled")))

c(12.7, 2.16, 2.13)

# Plot coefficients.
p1 <- full_models %>%
  unnest(coef) %>%
  mutate(name = fct_recode(name,
                           "Logistic Regression" = "full_logistic_f1",
                           "Ridge Regression"    = "full_ridge_f1_logloss_downsampled",
                           "Lasso Regression"    = "full_lasso_f1_logloss_downsampled")) %>%
  ggplot(aes(x = fct_reorder(term, estimate), 
             y = estimate, 
             fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ name, ncol = 4) +
  coord_flip() +
  labs(x = NULL,
       y = "Coefficient estimate") +
  theme_bw() +
  theme(strip.background = element_rect(colour = "black", fill = "white"))

setwd("../../figures") 
ggsave("coef_comparison.png", p1, height = 3, width = 8)


