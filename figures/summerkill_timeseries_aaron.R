# Load packages
library(tidyverse)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(scales)
library(car)
library(DescTools)
library(spdep)

# Load data
historical_data  <- read_csv('../data/processed/historical_data.csv')
future_data      <- read_csv('../data/processed/future_data.csv')
lasso_f1_logloss <- read_rds("../data/models/lasso_f1_logloss.rds")

# Stand-in model
m1 <- glm(summerkill ~ variance_after_ice_30 + variance_after_ice_60 + log_schmidt +
            cumulative_above_10 + population + lon + lat + season + temp,
          data =  historical_data,
          family = binomial)

# Function to compute prediction interval via simulation
compute_quantile <- function(x, q, reps = 1000) {
  x %>%
    map_dfc( ~ rbinom(reps, 1, prob = .x)) %>%
    rowSums() %>%
    quantile(q)
}


# Process data
predictions <- predict(object = m1, 
                       newdata = future_data, 
                       type = "response")

f_data <- future_data %>%
  add_column(prob = predictions) %>%
  group_by(year) %>%
  summarize(temp       = mean(mean_surf),
            kills      = sum(prob),
            lb_kill    = compute_quantile(prob, q = .025, reps = 300),
            ub_kill    = compute_quantile(prob, q = .975, reps = 300)
            ) %>%
  mutate(kills_smooth = loess(kills ~ year, .)$fitted,
         lb_smooth    = loess(kills ~ year, .)$fitted,
         ub_smooth    = loess(kills ~ year, .)$fitted)

hist_data <- historical_data %>% 
  filter(year < 2014) %>%
  group_by(year) %>%
  summarize(temp = mean(mean_surf),
            kills = sum(summerkill)) %>%
  mutate(kills_smooth = loess(kills ~ year, .)$fitted)

gap_data <- tibble(year = c(2014:2040, 2060:2080))

full_data <- f_data %>%
  bind_rows(hist_data, gap_data)



# Construct timeseries plot
p_timeseries <- ggplot(full_data, aes(x = year)) +
  geom_rect(xmin = 2013, xmax = 2041, ymin = -5, ymax = 110, fill = 'gray99', color = 'black', alpha = 0.01) +
  geom_rect(xmin = 2059, xmax = 2081, ymin =-5, ymax = 110, fill = "gray99",color = 'black', alpha = .01) +
  geom_line(aes(y = kills_smooth)) +
  geom_line(aes(y = lb_smooth), linetype = "dashed") +
  geom_line(aes(y = ub_smooth), linetype = "dashed") +
  ylab("Total Predicted Summerkills") +
  xlab(NULL) +
  theme_bw()


# Temperature plot
p_temp <- full_data %>% mutate(Temp = temp) %>%
  ggplot(aes(x = year, y = 1, fill = Temp)) +
  geom_tile() + 
  scale_fill_distiller(type = "seq", palette = "Reds", na.value = 'grey70', direction = 1) +
  theme_void()

p_temp
devtools::install_github("thomasp85/patchwork")
library(patchwork)
plot = p_timeseries + p_temp + plot_layout(ncol = 1, heights = c(8, 1))







ggsave("summerkill_time_serise.png", plot, width = 8, height = 5)

