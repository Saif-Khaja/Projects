### Scraping Scripts
Scripts in this folder are used in the modelling folder to create forecasting or regression models. 

#### Example
`Webscraping IMDB` and `Boxoffice Worldwide` are two scraping scripts that scrape data on movies. The first one gets data on runtime, votes, IMDB rating, release year etc., while the latter has data on boxoffice revenue top 200 movies in each year. These two datasets are combined using inner_join function that matches the same movie from each dataset. 

This combined dataset is used in the IMDB regression in the Models folder to run a regression model that estimates worldwide boxoffice revenue based on votes, release year, IMDB rating etc. 
