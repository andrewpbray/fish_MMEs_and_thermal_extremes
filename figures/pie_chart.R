library(tidyverse)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(scales)
library(car)
library(DescTools)
library(spdep)
library(wesanderson)

MME <-read_csv("data/raw/fish_kill_data_10_24_2018.csv",
               col_types = list(WBIC = col_character(),
                                Year = col_character()))

MME <- MME %>%
  rename_all(tolower) %>%
  mutate_all(tolower) %>%
  dplyr::filter(min.kill.size!="excludable") %>%
  dplyr::select(wbic,
                year,
                investigation.start.month,
                fishkill.inv.seq.no,
                cause.category.4) %>%
  rename(month = investigation.start.month) %>%
  mutate(dummy = 1,
         cause.categories = cause.category.4) %>% 
  spread(cause.categories, dummy, fill = 0) %>%
  rename(anthropogenic = `anthropogenic condition`,
         infectious    = `infectious agent`) %>%
  dplyr::select(-fishkill.inv.seq.no) %>%
  distinct(wbic, year, month, .keep_all = TRUE)

fig_S1_data <- MME %>%
  group_by(cause.category.4) %>%
  summarise(count = n()) %>%
  arrange(count)

fig_S1_data$pos = (cumsum(c(0, fig_S1_data$count)) + c(fig_S1_data$count / 2, .01))[1:nrow(fig_S1_data)]


pie <- fig_S1_data %>%
 ggplot(aes(x = "", y = count, fill = factor(cause.category.4)))+ 
      geom_bar(width = 1, stat = 'identity') +
      coord_polar("y", start=0) + 
    xlab(NULL) +
    ylab(NULL) + 
    theme_tufte() +
  scale_fill_manual(values = wes_palette("Darjeeling1")[c(1, 4, 2, 3, 5)])+
    theme(axis.text = element_blank(), axis.ticks = element_blank(), legend.title = element_blank())+
    geom_text(aes(y = pos, x = 1.7, label = percent(count/502)),size=3)+
    theme(text = element_text(family = 'sans'))
    
pie
    
ggsave("figures/pie_chart.png", pie, width = 8, height = 5)
  
