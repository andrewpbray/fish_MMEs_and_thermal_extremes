# Load libraries
library(tidyverse)
library(broom)

# Load models
setwd("../data/models") 
model_names <- list.files()
final_models <- model_names %>%
  str_subset("^final") %>%
  map(., ~read_rds(.x))
final_models <- tibble(name  = str_sub(str_subset(model_names, "^final"), 1, -5),
                       model = final_models)

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

final_models <- final_models %>%
  mutate(coef = map2(model, name, extract_coef),
         name = factor(name, levels = c("final_logistic_f1_backwards",
                                        "final_ridge_f1_logloss_downsampled",
                                        "final_lasso_f1_logloss_downsampled")))

# Plot coefficients.
p1 <- final_models %>%
  unnest(coef) %>%
  mutate(name = fct_recode(name,
                           "Logistic Regression" = "final_logistic_f1_backwards",
                           "Ridge Regression"    = "final_ridge_f1_logloss_downsampled",
                           "Lasso Regression"    = "final_lasso_f1_logloss_downsampled")) %>%
  ggplot(aes(x = fct_reorder(term, estimate), 
             y = estimate, 
             fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ name, ncol = 3) +
  coord_flip() +
  labs(x = NULL,
       y = "Coefficient estimate") +
  theme_bw() +
  theme(strip.background = element_rect(colour = "black", fill = "white"))

setwd("../../figures") 
ggsave("coef_comparison.png", p1, height = 3, width = 8)


