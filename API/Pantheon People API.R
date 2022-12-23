# Tue Nov 22 23:15:54 2022 - https://pantheon.world/data/api
library(tidyverse);library(httr);library(jsonlite);library(sf);library(mapview);library(plotly)
rm(list=ls())

pantheon_people <- function(Person){
  
  for (i in 1:length(Person)){
    
    if(i==1){
      df <- data.frame()
      start_time <- Sys.time()
    }
    
    print(paste("Getting data for", Person[i]))
    
    
    holder <- GET(paste0("https://api.pantheon.world/person?slug=eq.",Person[i])) %>% 
      content(as = "text") %>% 
      jsonlite::prettify() %>% 
      jsonlite::fromJSON(simplifyDataFrame = T) %>% 
      data.frame() 
    
    df <- rbind(df,holder)
    
    if(i==length(Person)){
      end_time <- Sys.time()
      print(paste("API successfully retrieved the data for", length(Person),"people in", round(lubridate::time_length(end_time-start_time, unit = "second"),2), "seconds"))
      return(tibble(df))
      
    }
  }
}

df <- pantheon_people(Person = c("Adam_Smith", "Michelangelo", "Thomas_Edison",
                                 "Albert_Einstein", "Genghis_Khan", "Aristotle",
                                 "Plato", "Socrates", "Adolf_Hitler",
                                 "Homer","Karl_Marx", "Immanuel_Kant",
                                 "Cleopatra","Elizabeth_II", "Napoleon",
                                 "Julius_Caesar","Archimedes", "Pythagoras",
                                 "Augustus", "Pope_Francis", "Sigmund_Freud",
                                 "Nelson_Mandela", "Muhammad_Ali", "Winston_Churchill"))


ggplotly(ggplot(df, aes(x=reorder(name,name), y=hpi, color=occupation, size=non_en_page_views)) +
  geom_point(show.legend = F) +
  theme_minimal() +
  labs(x="", y="HPI", 
       color="Occupation", title="Pantheon People HPI score") +
  theme(plot.title = element_text(size = 15, face = "bold"), 
        axis.text = element_text(colour = "black")) +
  coord_flip() +
  scale_y_continuous(n.breaks = 10)) 
  


df %>% 
  group_by(occupation) %>% 
  summarise(HPI=mean(hpi)) %>% 
  ggplot(aes(x=reorder(occupation,HPI), y=HPI, fill=occupation)) +
  geom_col(show.legend = F) +
  scale_y_continuous(n.breaks = 10) +
  coord_flip() +
  theme_minimal() +
  labs(y="HPI", x="", title="HPI Score by Occupation") +
  theme(plot.title = element_text(size = 12.5, face = "bold"), 
        axis.text = element_text(colour = "black"))


mapview(df, xcol = "bplace_lat", 
        ycol = "bplace_lon", 
        crs = 4269, grid = FALSE,
        map.types = "Stamen.Toner")


