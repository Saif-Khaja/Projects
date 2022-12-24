# Sat Dec 10 00:18:17 2022 ------------------------------
library(tidyverse);library(rvest)
rm(list=ls());gc()


Worldwide_Box_Office <- function(start, end){
  
  for (year in start:end){
    
    if(year==start){
      holder <- data.frame()
      df <- data.frame()
      start_time <- Sys.time()
    }
    
    print(paste("Scraping Box Office data for year", year))
    
    holder <- read_html(paste0("https://www.boxofficemojo.com/year/world/",year,"/")) %>% 
      html_element(xpath = "/html/body/div[1]/main/div/div/div[2]") %>% 
      html_table()
    
    colnames(holder)[5] <- "domestic_%"
    colnames(holder)[7] <- "foreign_%"
    
    holder <- holder %>% 
      mutate(Year=year)
    
    df <- rbind(df, holder)
    
    if(year==end){
      rm(holder)
      
      df[,3:7] <- map_df(df[,3:7],parse_number)
      
      end_time <- Sys.time()
      print(paste("Script successfully executed in", round(lubridate::time_length(end_time-start_time, unit = "seconds"),2), "seconds"))
      
      return(df)
    }
  }
}


df <- Worldwide_Box_Office(start = 1978, end = 2022)





box <- df %>% 
  group_by(Year) %>% 
  summarise(Worldwide=sum(Worldwide,na.rm = T),
            Domestic=sum(Domestic, na.rm = T),
            Foreign=sum(Foreign, na.rm = T)) %>% 
  mutate(ForeignProp=Foreign/Worldwide,
         DomesticProp=Domestic/Worldwide) %>% 
  pivot_longer(cols = 2:4, names_to = "BoxOffice") %>% 
  mutate(ForeignProp=round(ForeignProp,4),
         DomesticProp=round(DomesticProp,4))


ggplot(box, aes(x=Year, y=value/100000000, color=factor(BoxOffice))) +
  geom_line(size=0.75) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(y="100 Mil", x="", title="Box Office by Type of Revenue", 
       color="Type", subtitle = "Foreign share of revenue surpassed domestic share after 2002") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold", size = 15)) 


df %>% 
  ggplot(aes(x=Year, y=Worldwide/1000000, color=factor(Year))) +
  geom_boxplot(show.legend = F) +
  labs(y="100 Mil", x="", title="Box Plot Sales by Year", color="Type") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  ylim(0, 300) 




df %>% 
  slice_sample(n = 100) %>% 
  ggplot(aes(x=Rank, y=Worldwide, color=`Release Group`)) +
  geom_point(show.legend = F) +
  geom_text(aes(label=`Release Group`), size=3, show.legend = F) +
  theme_minimal() 
