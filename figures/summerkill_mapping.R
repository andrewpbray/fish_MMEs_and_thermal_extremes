# Load packages
library(tidyverse)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(scales)
library(car)
library(DescTools)
library(spdep)
library(rgdal)

# Load data
historical_data <- read_csv('../data/processed/historical_data.csv',
                            col_types = list(wbic = col_character()))
future_data     <- read_csv('../data/processed/future_data.csv',
                            col_types = list(wbic = col_character()))

# Load and process map data
dsn2 <- "../data/raw/wisconsin_outline"
ogrListLayers(dsn2)
spatial_w  <- readOGR(dsn2, layer = 'wisco_only')
map_data_w <- fortify(spatial_w) 
bbox <- c(-92.9, 42.4, -87, 46.9) 
wisconsin_map <- get_map(bbox, zoom = 7, maptype = 'toner-lines')

# Extract model predictions
# Lasso selected logistic model
m1 <- glm(summerkill ~ population  + lat + season + temp,
          data =  historical_data,
          family = binomial)
p_event <- predict(object = m1, 
             newdata = future_data, 
             type = "response")

# Combine data for plot
hist_data <- historical_data %>%
  mutate(prob = summerkill) %>%
  select(wbic, lat, lon, prob) %>%
  group_by(wbic, lat, lon) %>%
  summarize(prob = sum(prob)) %>%
  mutate(prob = ifelse(prob >= 1, 1, 0)) %>%
  add_column(generation = "Historical")
  
fut_data <- future_data %>%
  select(wbic, year, lat, lon) %>%
  add_column(p_event) %>%
  mutate(p_no_event = 1 - p_event,
         generation = ifelse(year > 2070, "Late 21st Century", "Early 21st Century")) %>%
  group_by(wbic, generation) %>%
  summarise(prob = (1 - prod(p_no_event)), 
            lon = mean(lon), 
            lat = mean(lat))
  
map_event_data <- hist_data %>%
  bind_rows(fut_data) %>%
  arrange(prob) %>%
  mutate(generation = factor(generation, levels = c("Historical",
                                                    "Early 21st Century",
                                                    "Late 21st Century")))

# Construct map
map <- ggmap(wisconsin_map) + 
  geom_point(data = map_event_data, 
             aes(x = lon, y = lat, color = prob, alpha = prob), 
             size = 0.6) +
  scale_color_gradient(low = 'Blue', high = 'Red', limits = c(0, 1)) +
  scale_alpha(limits = c(0,1), guide = "none") +
  geom_path(data = map_data_w, aes(x = long, y = lat, group = group)) +
  theme(text = element_text(family = 'sans')) +
  facet_grid(. ~ generation) + 
  theme_bw() +
  theme(strip.background = element_rect(colour = "white", fill = "white")) +
  guides(color = guide_colourbar(title.theme = element_text(size = 9),
                                barwidth = .8)) +
  labs(x = "Longitude",
       y = "Latitude",
       color = "Probability")

map

ggsave("summerkill_mapping.png", map, width = 8, height = 5)


