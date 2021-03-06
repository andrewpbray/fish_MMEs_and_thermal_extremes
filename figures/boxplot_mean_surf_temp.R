# Load packages and data
library(tidyverse)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(scales)
library(car)
library(DescTools)
library(spdep)

historical_data <- read_csv('data/processed/historical_data.csv')


# Summer

fig1a_data <- historical_data %>%
  mutate(cause.category.4 = as.factor(replace(as.character(cause.category.4), which(is.na(cause.category.4)), 'Summer Non-event')),
         cause.category.4 = fct_recode(cause.category.4,
                                       "Infectious Agent" = "infectious agent",
                                       "Summerkill"       = "summerkill"))

fig1a_data$sig <- factor(ifelse(fig1a_data$cause.category.4 == 'Summerkill', 'p<.05', 'p>.05'),levels = c('p>.05', 'p<.05'))

boxplot_a <- fig1a_data %>%
  filter(month %in% c("jul", "jun", "aug", "sep")) %>%
  filter(cause.category.4 %in% c('Infectious Agent', "Summerkill", "Summer Non-event")) %>%   
  ggplot(aes(y = mean_surf, x = cause.category.4)) +
  theme_tufte() +  
  ylab('Mean Surf. Temp. (°C)') +
  theme(text = element_text(size=13),
        axis.text = element_text(size=13),
        legend.justification = c(1, -0.4), 
        legend.position = c(0.4, 0),
        legend.box.margin=margin(c(50,50,50,50)),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  xlab(NULL) +
  geom_boxplot(outlier.alpha = 0.1, aes(fill = sig)) + 
  ggtitle('a') +
  theme(text = element_text(family = 'sans'))+
  scale_fill_manual(values = c('grey', 'gold'), guide = guide_legend(title = NULL))

boxplot_c <- fig1a_data %>%
  filter(month == 'jul' | month == 'jun' | month == 'aug' | month == 'sep') %>%
  filter(cause.category.4 == 'Summerkill' | cause.category.4 == 'Infectious Agent' | cause.category.4 == 'Summer Non-event') %>%   
  ggplot(aes(y = mean_surf_z,x = cause.category.4)) +
  theme_tufte() +
  ylab('Z-score Mean Surf. Temp') +
  guides(fill = FALSE) +
  theme(text = element_text(size = 13, family = 'sans'), 
        axis.text = element_text(size = 11),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid")) +
  geom_boxplot(outlier.alpha = 0.1, aes(fill = sig)) +
  xlab(NULL) +
  geom_hline(yintercept = 0, alpha = 0.5) + 
  ggtitle('c') +
  scale_fill_manual(values = c('grey', 'gold'), guide = guide_legend(title = 'T-test'))




# Winter

fig1b_data <- historical_data %>%
  mutate(cause.category.4 = replace(as.character(cause.category.4), which(is.na(cause.category.4)), 'Winter Non-event'),
         cause.category.4 = fct_recode(cause.category.4,
                                       "Winterkill" = "winterkill"))


fig1b_data$sig <- ifelse(fig1b_data$cause.category.4 == 'Winterkill', 'p<.05', 'p>.05')
fig1b_data$sig <- factor(fig1b_data$sig, levels = c('p>.05', 'p<.05'))


boxplot_b <- fig1b_data%>%
  filter(month == 'dec' | month == 'jan' | month == 'feb' | month == 'mar') %>%
  filter(cause.category.4 == 'Winterkill' | cause.category.4 == 'Winter Non-event') %>%
  ggplot(aes(y = mean_surf,x = cause.category.4)) +
  theme_tufte() +
  ylab(NULL)+
  xlab(NULL)+
  theme(text = element_text(size=13), 
        axis.text = element_text(size = 13),
        axis.line = element_line(colour = "black", 
                   size = 0.5, linetype = "solid"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_boxplot(outlier.alpha = 0.1, aes(fill = sig))+ 
  guides(fill = FALSE)+ 
  ggtitle('b') +
  theme(text = element_text(family = 'sans'))+
  scale_fill_manual(values = c('grey', 'grey'))

boxplot_d <- fig1b_data %>%
  filter(month == 'dec' | month == 'jan' | month == 'feb' | month == 'mar') %>%
  filter(cause.category.4 == 'Winterkill' | cause.category.4 == 'Winter Non-event') %>%   
  ggplot(aes(y = mean_surf_z,x = cause.category.4)) +
  theme_tufte() +
  xlab('Category of Killtype')+
  geom_boxplot(outlier.alpha = 0.1, aes(fill = sig)) +
  theme(text = element_text(size=13),
        axis.text = element_text(size = 11),
        axis.line = element_line(colour = "black", 
                   size = 0.5, linetype = "solid"))+
  xlab(NULL) +
  ylab(NULL)+
  ylim(-2.5,2) +
  geom_hline(yintercept = 0, alpha = 0.5)+
  scale_fill_brewer()+ 
  guides(fill = FALSE)+ 
  ggtitle('d') +
  theme(text = element_text(family = 'sans'))+
  scale_fill_manual(values = c('grey', 'grey'))


xbox <- grid.arrange(boxplot_a, boxplot_b,ncol = 2, widths = c(6, 3.6))
ybox <- grid.arrange(boxplot_c, boxplot_d, ncol = 2, widths = c(6, 3.6))

p <- grid.arrange(xbox, ybox)
ggsave("figures/boxplot_mean_surf_temp.png", p, width = 8, height = 5, dpi = 300)

