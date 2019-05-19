library(tidyverse)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(scales)
library(car)
library(DescTools)
library(spdep)

historical_data = read_csv('data/processed/historical_data.csv')


snow_data = read_csv('data/processed/snow_data.csv')

snow_data = snow_data %>%
  mutate(month = ifelse(Month == '01', 'jan', Month)) %>%
  mutate(month = ifelse(Month == '02', 'feb', month)) %>%
  mutate(month = ifelse(Month == '12', 'dec', month))

snow_data$year = snow_data$Year
snow_data$lat = snow_data$lat_round
snow_data$lon = snow_data$long_round

fig_SI4_data <- historical_data %>% 
  filter(month == 'jan' | month == 'feb' | month == 'dec') %>%
  mutate(lon = round(lon,1)) %>%
  mutate(lat = round(lat,1))

  
fig_SI4_data <- inner_join(fig_SI4_data, snow_data, by = c('year', 'month', 'lat','lon')) 
  
  
fig_SI4_data = fig_SI4_data %>%  
  mutate(mme = ifelse(is.na(cause.category.4), 0, 1), 
         winterkill = ifelse(is.na(winterkill), 0 , winterkill))




fig_SI4_data$sig_ice <- ifelse(fig_SI4_data$winterkill == 1, 'p>.05', 'p>.05')

fig_SI4_data$sig_ice <- factor(fig_SI4_data$sig_ice, levels = c('p>.05', 'p<.05'))

boxplot <- fig_SI4_data %>%
  filter(winterkill == 1 | mme == 0 ) %>%
  ggplot(aes(y =Snow,x = factor(winterkill))) +
  theme_tufte() +
  ylab('Precipitation')+
  xlab('Winterkill')+
  geom_boxplot(outlier.alpha = 0.1, aes(fill = sig_ice)) + 
  theme(text = element_text(size=13),axis.text = element_text(size=13),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"))+
  theme(text = element_text(family = 'sans'))+
  scale_fill_manual(values = c('grey', 'gold'), guide = guide_legend(title = NULL))


boxplot

ggsave("figures/winterkill_boxplot.png", boxplot, width = 8, height = 5)
