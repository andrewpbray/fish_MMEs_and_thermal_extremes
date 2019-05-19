library(ggplot2)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(scales)
library(car)
library(DescTools)
library(spdep)
library(readxl)
library(readr)
library(dplyr)
library(purrr)



Wi_Lakes_Maps <- read_excel("data/raw/Wi_Lakes_Maps.xlsx")


MME_data <- read_csv("data/raw/fish_kill_data_10_24_2018.csv") %>%       
  filter(Min.Kill.Size!="Excludable") %>%
  dplyr::select(-contains('County'), -contains('Station.Name'), -contains('Cause.Desc'), -contains('Site.Seq.No'), -contains('Fishkill.Inv.Seq.No'), - contains('Location.QA.Comment'), -contains('Activity.Desc'), -contains('Recommended.Action.Desc'), -contains('Fish.Kill.Comment'), -contains('Live.Fish.Desc')) %>%
  mutate(Month = Investigation.Start.Month) %>%
  dplyr::select(-contains('Investigation.Start.Month'))

lake_histogram_SI_data <- Wi_Lakes_Maps %>% left_join(MME_data, By = WBIC) %>% mutate(MME = ifelse(is.na(Investigation.Start.Date), 0, 1))


histogram <- lake_histogram_SI_data %>%
  group_by(WBIC) %>% 
  summarise(Depth = median(as.numeric(OFFICIAL_MAX_DEPTH_VALUE))) %>%
  ggplot(aes(x = Depth)) + 
  geom_histogram(binwidth = 5) + 
    ylab('Number of Lakes') +
  theme_tufte()
  
histogram


ggsave("figures/depth_histogram.png", histogram, width = 8, height = 5)



