library(tidyverse)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(scales)
library(car)
library(DescTools)
library(spdep)


historical_data <- read_csv('../data/processed/historical_data.csv')

future_data <- read_csv('../data/processed/future_data.csv')

lasso_fit_1 <- read_rds("../models/lasso_fit_1")

predictions_1 <- predict(object=lasso_fit_1, future_data,
                         type = "prob") %>%
  select(2)




compute_quantile <- function(x, q, reps = 1000) {
  x %>%
    map_dfc( ~ rbinom(reps, 1, prob = .x)) %>%
    rowSums() %>%
    quantile(q)
}



fig2_data = future_data


fig2_data$prob <- predictions_1$`1`



fig2_data <- fig2_data %>%
  group_by(year) %>%
  summarize(temp = mean(mean_surf),
            pred_kills = sum(prob),
            lb_kill    = compute_quantile(prob, q = .025, reps = 5000),
            ub_kill    = compute_quantile(prob, q = .975, reps = 5000))


fig2_data = fig2_data%>% 
  mutate(
    early_kills = ifelse(year < 2060, fig2_data$pred_kills, NA), 
    late_kills = ifelse(year > 2060, fig2_data$pred_kills, NA))


fig2_smoothed = fig2_data %>%
  mutate(pred_smooth = loess(pred_kills ~ year, span = 0.6)
  )


full_data <- bind(fig2_data%>%
                    filter(year > 2002),  
                  historical_data%>% 
                    mutate(
                           summerkill = ifelse(is.na(summerkill), 0, summerkill)) %>%
                    group_by(year) %>%
                    filter(year<2014) %>%
                    summarise(actual_kills = sum(summerkill), 
                              temp = mean(mean_surf))
) 





full_data = full_data%>%#bind_rows(tibble(Year = c(2015:2040, 2060:2080))) %>% 
  mutate(
        early_lb = ifelse(year < 2060, lb_kill, NA),
        late_lb = ifelse(year > 2060, lb_kill, NA), 
        early_ub = ifelse(year < 2060, ub_kill, NA),
        late_ub = ifelse(year > 2060, ub_kill, NA))


full_data_smoothed = full_data%>%
  mutate(
    pred_smooth = loess(pred_kills ~year),
    early_lb = loess(early_lb ~year),
    late_lb = loess(late_lb ~year),
    early_ub = loess(early_ub ~year),
    late_ub = loess(late_ub ~year)
  )



plot<- full_data%>%
  ggplot(aes(x=Year,y = pred_kills)) +
  geom_rect(xmin = 2013, xmax = 2041, ymin = 0, ymax = 110, alpha = 0.004)+
  geom_rect(xmin = 2059, xmax = 2081, ymin = 0, ymax = 110, alpha = 0.004)+
  #geom_line(color = 'black') +
  #geom_ribbon(aes(ymax = ub_kill, ymin = lb_kill), alpha = 0.5)+
  geom_smooth(aes(y = actual_kills), span = 0.5, color = 'black', se = FALSE)+
  geom_smooth(aes(y = early_kills), span = 0.5, color = 'black', se = FALSE)+
  geom_smooth(aes(y = late_kills), span = 0.5, color = 'black', se = FALSE)+
  geom_smooth(aes(y = early_lb), span = 0.5, color = 'black', linetype = 'dashed', se = FALSE) +
  geom_smooth(aes(y = late_lb), span = 0.5, color = 'black', linetype = 'dashed', se = FALSE) +
  geom_smooth(aes(y=early_ub),span = 0.5, color = 'black', linetype = 'dashed', se = FALSE)+
  geom_smooth(aes(y=late_ub),span = 0.5, color = 'black', linetype = 'dashed', se = FALSE)+
  geom_bar(stat = 'identity', aes(x = Year, y = y, fill = temp), alpha = 0.5)+
  
  ylab("Total Predicted Summerkills")+
  xlab(NULL) +
  #theme_tufte()+
  theme_calc()+
  theme(text = element_text(family = 'sans'))+
  scale_fill_gradient(low = 'blue', high = 'red', limits = c(9, 16)) +
  #scale_fill_manual( palette = 'Blue-Red', limits = c(9, 16), breaks = waiver())+
  ylim(0,130) 
plot


ggsave("summerkill_time_serise.png", plot, width = 8, height = 5)

