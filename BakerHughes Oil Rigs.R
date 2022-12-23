# Sat Nov 12 12:57:43 2022 ------------------------------
library(tidyverse);library(readxl);library(forecast)
rm(list=ls())

#data <- read_xls("https://rigcount.bakerhughes.com/static-files/dac0d9b2-e5bd-4705-bb18-28afd10d534e")

####Read data####

data <- read_excel("C:/Users/saifd/Downloads/Worldwide Rig Count Oct 2022.xlsx", 
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric"), skip = 3)


####Data wrangling####

df <- data %>%
  filter(!str_detect(`2022`, "Avg.")) %>% 
  add_row(.before = 1, `2022`=colnames(data)[1])

df <- df %>%
  mutate(Date=parse_number(df$`2022`, trim_ws = T)) %>% 
  relocate(Date)

df <- df %>%  
  fill(Date,.direction = c("down")) %>% 
  mutate(Date=paste0(`2022`,"-",Date)) %>%
  na.omit() %>% 
  select(-`2022`) %>% 
  mutate(Date=lubridate::my(Date)) %>% 
  pivot_longer(cols = 2:10, "Region")
  

df %>% 
  pivot_wider(names_from = Region, values_from = value) %>% 
  view()

####Visualization####

ggplot(df, aes(x=Date, y=value, color=as.factor(Region))) +
  geom_line(show.legend = F) +
  facet_wrap(~Region, scales="free_y") +
  labs(title="Active Oil Rig Count", y="", x="") +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        title = element_text(face = "bold.italic", size = 13))


df %>% 
  filter(Region=="Total World") %>% 
  ggplot(aes(x=Date, y=value, color=as.factor(Region))) +
  geom_line(size=1) +
  labs(color="World Oil Rig Count",x="", y="Volume (Oil Rigs)") +
  scale_x_date(date_breaks = "2 years", labels=scales::date_format(format = "%Y")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45)) +
  expand_limits(y=c(800,4800)) +
  scale_y_continuous(breaks = seq(800,4800, 800))



df %>% 
  mutate(Month=lubridate::month(df$Date, label = T),
         Region=as_factor(Region),
         Month=as_factor(Month)) %>%
  group_by(Region, Month) %>% 
  summarise(value=mean(value)) %>%
  ggplot(aes(x=as_factor(Month), y=value, color=Region, group=Region)) +
  geom_line(show.legend = F) +
  facet_wrap(~Region, scales="free") +
  labs(x="", y="Oil Rigs")
  
####Total World Oil Rigs Forecast####

ts <- df %>% 
  filter(Region=="Total World") %>% 
  arrange(Date) %>% 
  select(3) 

ts <- ts %>% 
  ts(start=1975, end=2022,frequency=12)


model <- auto.arima(ts)
  

####Forecast Visualization####

model %>% 
  forecast(h=12) %>% 
  autoplot() +
  autolayer(model$fitted, lty=4) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(800, 6400, 800)) +
  labs(y="", x="") 

####Checking Model Residuals and Summary####

ggtsdisplay(model$residuals, lag.max = 48, points = F)

accuracy(model) %>% 
  broom::fix_data_frame() %>% 
  suppressWarnings()




