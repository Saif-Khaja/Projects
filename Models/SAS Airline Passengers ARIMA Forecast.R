# Sat Dec 18 13:04:39 2022 ------------------------------
library(tidyverse);library(forecast);library(plotly);library(lubridate)
rm(list=ls())

###################################
### READ DATA FROM SAS WEBSITE  ###
##################################

url<-'https://www.sasgroup.net/files/documents/interactive-data/2022/Operational-traffic-data-2004-2022Sep.xlsx'
p1f <- tempfile()
download.file(url, p1f, mode="wb")
p1 <-readxl::read_excel(path = p1f, sheet = 5, range = "A2:N21")
df <- p1

################################
### CLEAN AND TRANSFORM DATA ###
################################

glimpse(df)
colnames(df)[1] <- "Year"

df <- map_df(df, str_remove_all, "[[:blank:]]")
df <- map_df(df, as.numeric)

data <- df %>% 
  pivot_longer(cols = 2:14, names_to = "Month")

data$Month <- as.factor(data$Month) 
levels(data$Month)
data$Month <- fct_relevel(data$Month, month.abb)

data <- data %>% 
  filter(!str_detect(Month, "Total")) %>% 
  mutate(Date=my(paste0(Month,"-",Year))) %>% 
  relocate(Date) %>% 
  na.omit() %>% 
  arrange(Date)

#########################################
### VISUALIZATIONS GGPPLOT AND PLOTLY ###
#########################################

theme_set(theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 16), 
                  axis.text = element_text(color = "black")))

ggplot(data, aes(Date, value)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title="SAS Monthly Air Passengers", X=NULL, y=NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 10)

ggplot(data, aes(Month, value, color=Month)) +
  geom_boxplot(show.legend = F, outlier.colour = NA) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x=NULL, y=NULL) +
  ylim(1500000,2500000) 

data %>% 
  group_by(Month) %>% 
  summarise(value=mean(value)) %>% 
  ggplot(aes(x=as.numeric(Month), y=value)) +
  geom_line() +
  labs(x=NULL, y=NULL) +
  scale_x_continuous(n.breaks = 12)

ggplotly(ggplot(data, aes(as.numeric(Month), value, color=as.factor(Year))) +
  geom_line() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x=NULL, y=NULL, color="Year") +
  scale_y_continuous(n.breaks = 12), dynamicTicks = T)

#######################################
### ARIMA FORECASTING OF PASSENGERS ###
#######################################

ts <- ts(data[,4], frequency = 12, 
            start = min(year(data$Date)), 
            end = max(year(data$Date))) 

model <- Arima(ts, order=c(0,1,0), seasonal = list(order=c(2,1,0), period=12)) 

#############################################
### CHECK MODEL PERFORMANCE AND RESIDUALS ###
#############################################

ggtsdisplay(model$residuals)

summary(model)

accuracy(model)

###############################################
### FORECAST SAS PASSENGERS 12 MONTHS AHEAD ###
###############################################

forecast(model, h = 12, level = c(80,95)) %>% 
  autoplot() +
  labs(x=NULL, y=NULL) +
  scale_x_continuous(n.breaks = (max(year(data$Date))-min(year(data$Date)))) +
  scale_y_continuous(labels = scales::comma_format(), n.breaks = 8)



