library(ggplot2)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(scales)
library(car)
library(DescTools)
library(spdep)

historical_data = read_csv('data/processed/historical_data.csv')



fig_SI2_data <- historical_data %>%
  mutate(cause.category.4 = replace(as.character(cause.category.4), which(is.na(cause.category.4)), 'Winter Non-event')) %>%
  mutate(cause.category.4 = replace(as.character(cause.category.4), which(cause.category.4 == 'winterkill'), 'Winterkill'))
         
         
fig_SI2_data$sig_ice <- 'p<.05'
fig_SI2_data$sig_ice <- factor(fig_SI2_data$sig_ice, levels = c('p>.05', 'p<.05'))

boxplot <- fig_SI2_data %>%
  filter(month == 'jan' | month == 'feb' | month == 'dec' | month == 'mar') %>%
  filter(cause.category.4 == 'Winterkill' | cause.category.4 == 'Winter Non-event' ) %>%
  ggplot(aes(y = ice_duration,x = cause.category.4)) +
  theme_tufte() +
  ylab('Ice Duration (Days)')+
  xlab('Category of Killtype')+
  xlab(NULL) +
  theme(text = element_text(size=13),axis.text = element_text(size=13),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"))+
  geom_boxplot(outlier.alpha = 0.1, aes(fill = sig_ice)) + 
  theme(text = element_text(family = 'sans'))+
  scale_fill_manual(values = c('gold'), guide = guide_legend(title = NULL))

boxplot

ggsave("figures/ice_duration_boxplot.png", boxplot, width = 8, height = 5)

