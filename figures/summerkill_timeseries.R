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
  geom_rect(xmin = 2015, xmax = 2040, ymin = 0, ymax = 110, fill = "gray", alpha = .002) +
  geom_rect(xmin = 2060, xmax = 2080, ymin = 0, ymax = 110, fill = "gray", alpha = .002) +
  geom_line(aes(y = kills_smooth)) +
  geom_line(aes(y = lb_smooth), linetype = "dashed") +
  geom_line(aes(y = ub_smooth), linetype = "dashed") +
  ylab("Total Predicted Summerkills") +
  xlab(NULL) +
  theme_bw()


# Temperature plot
p_temp <- full_data %>%
  ggplot(aes(x = year, y = 1, fill = temp)) +
  geom_tile() +
  theme_void()

library(patchwork)
p_timeseries + p_temp + plot_layout(ncol = 1, heights = c(8, 1))





make_quantiles <- function(x, probs = seq(0, 1, by = .2)) {
  qu <- quantile(x, probs = probs)
  cut(x, 
      breaks = qu,
      labels = names(qu)[-1], 
      include.lowest = TRUE)
}

full_data <- full_data%>%#bind_rows(tibble(Year = c(2015:2040, 2060:2080))) %>% 
  mutate(
        Temp     = make_quantiles(Temp),
        early_lb = ifelse(year < 2060, lb_kill, NA),
        late_lb  = ifelse(year > 2060, lb_kill, NA), 
        early_ub = ifelse(year < 2060, ub_kill, NA),
        late_ub  = ifelse(year > 2060, ub_kill, NA))

pred_smooth       <- loess(pred_kills ~ year, full_data)
pred_early_smooth <- loess(early_kills ~ year, full_data %>% filter(year < 2070))
pred_late_smooth  <- loess(late_kills ~ year, full_data %>% filter(year>2070))
early_lb_smooth   <- loess(early_lb ~ year, full_data %>% filter(year < 2070))
late_lb_smooth    <- loess(late_lb ~ year, full_data %>% filter(year>2070))
early_ub_smooth   <- loess(early_ub ~ year, full_data %>% filter(year < 2070))
late_ub_smooth    <- loess(late_ub ~ year, full_data %>% filter(year>2070))

full_data_smoothed <- full_data %>%
  mutate(
    pred_s     = ifelse(year > 2040, pred_smooth$fitted, NA),
    pred_e_s   = ifelse(year > 2040 & year < 2070, pred_early_smooth$fitted, NA),
    pred_l_s   = ifelse(year > 2070, pred_late_smooth$fitted, NA),
    early_lb_s = ifelse(year > 2040 & year < 2070, early_lb_smooth$fitted, NA),
    late_lb_s  = ifelse(year> 2070, late_lb_smooth$fitted, NA),
    early_ub_s = ifelse(year> 2040 & year < 2070, early_ub_smooth$fitted, NA),
    late_ub_s  = ifelse(year> 2070, late_ub_smooth$fitted, NA)
  )

plot <- full_data_smoothed %>%
  ggplot(aes(x=year,y = pred_s)) +
  geom_rect(xmin = 2019, xmax = 2041, ymin = 0, ymax = 110, alpha = 0.002)+
  geom_rect(xmin = 2059, xmax = 2081, ymin = 0, ymax = 110, alpha = 0.002)+
  #geom_line(color = 'black') +
  #geom_ribbon(aes(ymax = ub_kill, ymin = lb_kill), alpha = 0.5)+
  geom_point(aes(y = actual_kills), color = 'black', alpha = 0.8)+
  geom_line(aes(y = pred_e_s), color = 'black')+
  geom_line(aes(y = pred_l_s), color = 'black')+
  geom_line(aes(y = early_lb_s), color = 'black', linetype = 'dashed') +
  geom_line(aes(y = late_lb_s), color = 'black', linetype = 'dashed') +
  geom_line(aes(y = early_ub_s), color = 'black', linetype = 'dashed')+
  geom_line(aes(y = late_ub_s), color = 'black', linetype = 'dashed')+
  ylab("Total Predicted Summerkills")+
  xlab(NULL) +
  theme_tufte()+
  theme(text = element_text(family = 'sans'))+
  ylim(0,70) 
plot

side_plot <- full_data %>%
  ggplot +
  geom_bar(stat = 'identity', aes(x = year, y = 1, fill = Temp)) +
  theme_tufte() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(text = element_text(family = 'sans')) +
  scale_color_brewer(palette =  'OrRd', aesthetics = 'fill', direction = 1)
#  scale_fill_discrete(low = 'blue', high = 'red', limits = c(9, 16)) 

side_plot

plot_full <- grid.arrange(plot, side_plot)

ggsave("summerkill_time_serise.png", plot, width = 8, height = 5)

