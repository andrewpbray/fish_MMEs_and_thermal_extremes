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

fig_SI6_data <- Wi_Lakes_Maps %>% left_join(MME_data, By = WBIC) %>% mutate(MME = ifelse(is.na(Investigation.Start.Date), 0, 1))



boxplot <- fig_SI5_data %>%
  filter(!is.na(OFFICIAL_MAX_DEPTH_VALUE)) %>%
  filter(!is.na(MME)) %>%
  group_by(WBIC) %>%
  summarise(Depth = mean(as.numeric(OFFICIAL_MAX_DEPTH_VALUE)), Dieoffs = sum(MME))%>%
  mutate(Dieoffs = ifelse(Dieoffs >= 4, '4+', Dieoffs)) %>%
  ggplot(aes(x=Dieoffs, y = Depth, colour = Dieoffs)) +
  ylim(0,110) +
  geom_boxplot(aes(group = Dieoffs), outlier.alpha = 0.1)+
  guides(colour = FALSE)+
  theme_tufte() +
  theme(text = element_text(family = 'sans'))

boxplot

ggsave("figures/depth_boxplot.png", boxplot, width = 8, height = 5)




