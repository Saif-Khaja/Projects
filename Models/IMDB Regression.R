# Wed Nov 12 19:27:25 2022 ------------------------------
library(tidyverse);library(rvest);library(broom)

# Run the 'Webscraping IMDB' and 'Boxoffice Worldwide' scripts before you run this or else it wont work

colnames(df)[2] <- "Movie"

data <- inner_join(x = df, y = IMDB_Data, by=c("Movie", "Year"))

data <- data %>%
  select(c(-Rank,-Domestic:-`foreign_%`,-Director:-Stars,-Movie))

data$Genre <- as_factor(data$Genre)

# Multiple Linear Regression Model
## Movie Runtime, Votes, Rating, and Genres as predictors for Worldwide boxoffice Revenue 

lm <- lm(Worldwide~., data = data)
summary(lm)

tlm <- lm %>% 
  tidy();tlm

augment(lm)
glance(lm)

tlm[,2:5] <- map_df(tlm[,2:5], round,4)


# Graph of significant beta estimates at 5% level

tlm %>% 
  filter(p.value<0.05,
         !str_detect(term,"(Intercept)")) %>% 
  ggplot(aes(x=reorder(term,estimate), y=estimate, fill=term)) +
  geom_col(show.legend = F) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x=NULL,
       title="IMDB Regression Estimates on Worldwide Boxoffice") +
  theme(axis.text = element_text(color="black"),
        plot.title = element_text(face="bold"))
