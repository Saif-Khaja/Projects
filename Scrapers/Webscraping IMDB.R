# Sun Dec 26 14:42:18 2021 ------------------------------
library(rvest);library(tidyverse);library(plotly)
rm(list=ls());gc()
#Box office data
#https://www.boxofficemojo.com/date/2017-12-26/?ref_=bo_da_nav

###################
weblink <- ("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=action&sort=user_rating,desc&start=1&ref_=adv_nxt")
IMDB_Data <- data.frame()

for (page_result in seq(from = 1, to = 1591, by = 50)) {
  link <- paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=action&sort=user_rating,desc&start=", page_result ,"&ref_=adv_nxt")
  page <- read_html(link)
  
  df <- data.frame(
    Movie=page %>% 
      html_nodes(".lister-item-header a") %>% 
      html_text(),
    Year=page %>% 
      html_nodes(".text-muted.unbold") %>% 
      html_text(),
    Runtime=page %>% 
      html_nodes(".runtime") %>% 
      html_text(),
    Rating=page %>% 
      html_nodes(".ratings-imdb-rating strong") %>% 
      html_text(),
    Genre=page %>% 
      html_nodes(".genre") %>% 
      html_text(),
    Votes=page %>% 
      html_nodes(".sort-num_votes-visible span:nth-child(2)") %>% 
      html_text(),
    Cast=page %>% 
      html_nodes(".text-muted+ p") %>% 
      html_text())

  # PG Rating and Gross data is not always 50 in length, which breaks the loop
  # Rated=page %>% 
  #   html_nodes(".certificate") %>% 
  #   html_text()
  
  # Gross <- page %>% 
  #   html_nodes(".ghost~ .text-muted+ span") %>% 
  #   html_text()  
  
  # Gross <- sub("M","", Gross)
  # Gross <-  as.numeric(gsub("\\$", "", Gross))
  
  
  print(paste("Scraping IMDB Page:", page_result))
  
  IMDB_Data <- rbind(IMDB_Data, df)
  
  if (page_result==1551){
    
    rm(list=setdiff(ls(), "IMDB_Data"))
    
    # Data cleaning/wrangling
    IMDB_Data <- IMDB_Data %>% 
      separate(col = Cast, into = c("Director", "Stars"), sep = "[|]") 
    
    IMDB_Data$Votes <- str_remove_all(IMDB_Data$Votes, ",")
    IMDB_Data$Year <- str_remove_all(IMDB_Data$Year, "[()I]")
    IMDB_Data$Runtime <- str_remove_all(IMDB_Data$Runtime, "min")
    IMDB_Data <- map_df(IMDB_Data, str_trim, "both")
    
    # Remove Director: and Stars: String
    IMDB_Data$Director <- str_remove_all(IMDB_Data$Director, pattern = "Director:")
    IMDB_Data$Director <- str_remove_all(IMDB_Data$Director, pattern = "Directors:")
    IMDB_Data$Stars <- str_remove_all(IMDB_Data$Stars, pattern = "Stars:")

    # Recoding columns from characters to numeric
    IMDB_Data[, c(2,3,4,6)] <- map_df(IMDB_Data[, c(2,3,4,6)], as.numeric)
  }
}


########################
#####Data Wrangling#####
########################

Summary <- IMDB_Data %>% 
  group_by(Year) %>% 
  summarise(Rating = round(mean(Rating, na.rm=T), 2),
            Runtime = round(mean(Runtime, na.rm=T), 0),
            Votes = round(sum(Votes, na.rm=T), 0),
            count = n())

########################
###Data visualization###
########################

ggplot(IMDB_Data, aes(x=Runtime, y=Rating, size=Votes)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm', color = "salmon") +
  labs(title = "IMDM Movie Runtime and Rating",
       y = "IMDB Rating") +
  theme_light() +
  theme(panel.border = element_rect(color = "white"))

ggplot(IMDB_Data, aes(x=Votes, fill=factor(Year))) +
  geom_histogram() +
  theme(legend.position = 'none') +
  labs(title = "Histogram of Votes on Release Year") +
  theme_light() +
  theme(panel.border = element_rect(color = "white"))

ggplot(IMDB_Data, aes(x=Runtime, fill = factor(Year))) +
  geom_histogram(color='white') +
  theme_light() +
  theme(panel.border = element_rect(color = "white"))

ggplot(IMDB_Data, aes(x = factor(Year), y = Runtime)) +
  geom_boxplot() 

##############
####Plots#####
##############

ggplot(Summary, aes(x = Year, y = count)) +
  geom_col(color = "white") +
  labs(x=NULL)

ggplot(Summary, aes(x = Year, y = Votes)) +
  geom_col(color = "white") +
  scale_y_continuous(labels = scales::comma)

ggplot(Summary, aes(x = Year, y = Runtime)) +
  geom_col(color = "white")

ggplot(Summary, aes(x = Year, y = count, fill = Year)) +
  geom_col(color = "white") +
  theme_light() +
  scale_x_continuous(breaks = seq(min(IMDB_Data$Year),max(IMDB_Data$Year),2)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8))
