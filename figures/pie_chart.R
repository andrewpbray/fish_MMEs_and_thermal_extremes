

library(tidyverse)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(scales)
library(car)
library(DescTools)
library(spdep)

#historical_data <- read_csv('../data/processed/historical_data.csv')
historical_data_old <- read_csv("../data/processed/historical_data.csv",
         col_types = list(wbic = col_character(),
                          year = col_character(),
                          month  = col_character(),
                          season = col_character(),
                          summerkill = col_character(),
                          ice_duration = col_double()))

fig_S1_data <- historical_data_old %>%
  filter(!is.na(cause.category.4)) %>%
  filter(cause.category.4 != 'UNKNOWN') %>%
  mutate(cause.category.4 =  replace(as.character(cause.category.4), which(cause.category.4 == 'SUMMERKILL'), 'Summerkill'))%>%
  mutate(cause.category.4 =  replace(as.character(cause.category.4), which(cause.category.4 == 'INFECTIOUS AGENT'), 'Infectious Agent')) %>% 
  mutate(cause.category.4 =  replace(as.character(cause.category.4), which(cause.category.4 == 'WINTERKILL'), 'Winterkill')) %>%
  mutate(cause.category.4 =  replace(as.character(cause.category.4), which(cause.category.4 == 'ANTHROPOGENIC CONDITION'), 'Human Perturbation')) %>%
  mutate(cause.category.4 =  replace(as.character(cause.category.4), which(cause.category.4 == 'UNKNOWN'), 'Unknown')) 
  
  
fig_S1_data <-  fig_S1_data %>%
    group_by(cause.category.4) %>%
    summarise(count = n()) %>%
    arrange(count)

fig_S1_data$pos = (cumsum(c(0, fig_S1_data$count)) + c(fig_S1_data$count / 2, .01))[1:nrow(fig_S1_data)]

pal <- wes_palette("Darjeeling1")
pie <- fig_S1_data %>%
 ggplot(aes(x = "", y = count, fill = cause.category.4))+ 
      geom_bar(width = 1, stat = 'identity') +
      coord_polar("y", start=0) + 
    xlab(NULL) +
    ylab(NULL) + 
    theme_tufte() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), legend.title = element_blank())+
    geom_text(aes( y = pos, x = 1.7, label = percent(count/373)),size=3)+
    theme(text = element_text(family = 'sans'))
    
    
ggsave("pie_chart.png", pie, width = 8, height = 5)
  
