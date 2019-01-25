
library(tidyverse)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(scales)
library(car)
library(DescTools)
library(spdep)
library(rgdal)




dsn2 <- "../data/wisconsin_outline"
ogrListLayers(dsn2)
spatial_w <- readOGR(dsn2, layer = 'wisco_only')


historical_data <- read_csv('../processed-data/historical_data.csv')

future_data <- read_csv('../processed-data/future_data.csv')


lasso_fit_1 <- read_rds("../models/lasso_fit_1")

predictions_1 <- predict(object=lasso_fit_1, future_data,
                         type = "prob") %>%
  select(2)

map_data_w <-fortify(spatial_w) 

bbox =c(-92.9, 42.4, -87, 46.9) 
Wisconsin_map <- get_map(bbox, zoom = 7, maptype = 'toner-lines')



# building data



future_predictions = future_data %>% bind_cols(predictions_1) %>% mutate(event = pos)

fig3_data = bind_rows(historical_data, future_predictions) %>%
  mutate(generation = ifelse(year > 2070, 'c', 'b')) %>%
  mutate(generation = ifelse(generation == 'b' & year < 2040, 'a', generation))%>%
  mutate(summerkill_binary = ifelse(is.na(summerkill) | summerkill == 0, 0 , 1)) %>%
  mutate(event = ifelse(year> 2040, event, NA)) %>%
  mutate(no_event_prob = 1 - event) %>%
  group_by(wbic, generation) %>%
  summarise(Prob = (1 - prod(no_event_prob)), 
            Prob = ifelse(is.na(Prob), max(summerkill_binary), Prob), 
            V1 = mean(lon), 
            V2 = mean(lat))
  

# building map



map <- ggmap(Wisconsin_map) + 
  geom_point(data = arrange(fig3_data, Prob), aes(x = V1, V2, color = Prob, alpha = Prob), size = 0.5)+
  scale_color_gradient(low = 'Blue', high = 'Red',na.value = 'red', limits = c(0, 1)) +
  #theme( panel.border = element_rect(colour = "black", fill=NA, size=3),
        #plot.margin=unit(c(1,0.3,1,2), "cm")+
  ylab('Latitude')+
  xlab('Longitude')+
  guides(alpha = FALSE) +
  scale_alpha(limits = c(0,1))+
  geom_path(data = map_data_w, aes(x = long, y = lat, group = group))  +
  theme(text = element_text(family = 'sans'))+
  theme_bw()+
  facet_grid(.~generation) + 
  theme(strip.background = element_blank())
  facet_grid(.~generation) +
  theme(strip.background = element_rect(colour = "black", fill = "white"))
map


ggsave("summerkill_mapping.png", map, width = 8, height = 5)


