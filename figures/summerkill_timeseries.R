# Load packages
library(tidyverse)
library(patchwork)

# Load functions to compute prediction interval via simulation
compute_quantile <- function(x, q, reps = 1000) {
  x %>%
    map_dfc( ~ rbinom(reps, 1, prob = .x)) %>%
    rowSums() %>%
    quantile(q)
}

# Load data
setwd("../data/processed")
historical_data <- read_csv("historical_data.csv",
                            col_types = list(wbic = col_character(),
                                             month  = col_character(),
                                             season = col_character(),
                                             summerkill = col_character(),
                                             ice_duration = col_double())) %>%
  select(-cause.category.4, -anthropogenic, -infectious, -unknown, -winterkill) %>%
  mutate(summerkill = fct_recode(as.factor(summerkill),
                                 "neg" = "0",
                                 "pos" = "1"))
future_data <- read_csv('future_data.csv')

# Lasso selected logistic model
m1 <- glm(summerkill ~ temp + population + lat + season,
          data =  historical_data,
          family = binomial)

p <- predict(object = m1, 
             newdata = future_data, 
             type = "response")

# Prepare data for plot
set.seed(3318)
f_data <- future_data %>%
  add_column(prob = p) %>%
  group_by(year) %>%
  summarize(temp       = mean(mean_surf),
            kills      = sum(prob),
            lb_kill    = compute_quantile(prob, q = .025, reps = 1000),
            ub_kill    = compute_quantile(prob, q = .975, reps = 1000),
            draw_kills = sum(map_int(prob, ~rbinom(1, 1, .x))))

f_early <- f_data %>%
  filter(year < 2075) %>%
  mutate(kills_smooth = loess(kills ~ year, .)$fitted,
         lb_smooth    = loess(lb_kill ~ year, .)$fitted,
         ub_smooth    = loess(ub_kill ~ year, .)$fitted)

f_late <- f_data %>%
  filter(year > 2075) %>%
  mutate(kills_smooth = loess(kills ~ year, .)$fitted,
         lb_smooth    = loess(lb_kill ~ year, .)$fitted,
         ub_smooth    = loess(ub_kill ~ year, .)$fitted)

hist_data <- historical_data %>% 
  filter(year < 2014) %>%
  group_by(year) %>%
  summarize(temp = mean(mean_surf),
            kills_obs = sum(summerkill == "pos")) %>%
  mutate(kills_obs_smooth = loess(kills_obs ~ year, .)$fitted)

gap_data <- tibble(year = c(2014:2040, 2060:2080))

full_data <- f_early %>%
  bind_rows(f_late, hist_data, gap_data)

# Construct timeseries plot
p_timeseries <- ggplot(full_data, aes(x = year)) +
  geom_point(aes(y = kills_obs)) +
  geom_ribbon(aes(ymin = lb_smooth, ymax = ub_smooth), fill = "steelblue", alpha = .2) +
  geom_point(aes(y = draw_kills), color = "steelblue") +
  geom_line(aes(y = kills_smooth)) +
  annotate("rect", xmin = 2014, xmax = 2041, ymin = 0, ymax = 35, fill = "gray", alpha = .15) +
  annotate("rect", xmin = 2059, xmax = 2080, ymin = 0, ymax = 35, fill = "gray", alpha = .15) +
  #geom_line(aes(y = lb_smooth), linetype = "dashed") +
  #geom_line(aes(y = ub_smooth), linetype = "dashed") +
  annotate("text", label = "Historical", x = 2007, y = 36, size = 3) +
  annotate("text", label = "Early 21st Cent.", x = 2050, y = 36, size = 3) +
  annotate("text", label = "Late 21st Cent.", x = 2090, y = 36, size = 3) +
  ylab("Total Predicted Summerkills") +
  xlab(NULL) +
  ylim(c(0, 36)) +
  theme_bw()

# Temperature plot
p_temp <- full_data %>%
  ggplot(aes(x = year, y = 1, fill = temp)) +
  geom_tile(na.rm = TRUE) + 
  scale_fill_distiller(type = "seq", palette = "Reds", 
                       direction = 1, na.value = "#e3e3e3") +
  labs(fill = "Temp") +
  theme_void() +
  guides(fill = guide_colourbar(title.theme = element_text(size = 8),
                                label.theme = element_text(size = 7),
                                barwidth = .8,
                                barheight = 4))

final_p <- p_timeseries + p_temp + plot_layout(ncol = 1, heights = c(15, 1))
setwd("../../figures")
ggsave("summerkill_timeseries.png", final_p, height = 4.5, width = 8)
