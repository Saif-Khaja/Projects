# Projects
This repository contains a diverse set of projects including scraping, data cleaning, wrangling and forecasting scripts along with API's that I have written in Rstudio. 

The data I get is either through a public API or scraped from various sites. After I have retrieved the data it gets cleaned and transformed. In this process I use a multiple libraries within the `Tidyverse` package such as `dplyr`, `purr`, `stringr` to work with strings, `tidyr` to reshape data, and then finally `ggplot2` or `plotly` to visualize the data.


### API
I write a function to interact with the public API using the GET function from `httr` and then parse the data from JSON format to a dataframe using functions from `jsonlite`. After getting the data I feed the data into my `SQL` database to quickly retrieve the data instead of running the API request again. The USDA API, which I wrote can retrieve almost 3 million rows of data across 64 commodities with indicators such as production, consumption, inventory, import, export, and etc on a global and regional level.

### Scraping
Scraping projects are done by either using the `rvest` library if it a static website else `Rselenium` is used to interact with HTML code on the website. 

### Modelling
I work with a lot of time series data so usually I will forecast the data using an ARIMA model when forecasting few series or a smoothing model if the volume of series are large. To do this I use the `forecast` library and [book](https://otexts.com/fpp3/) written by Rob J Hyndman and others. I also have experience with simple, multiple, and logistic regression model. I have limited experience with alternative models such as Vector Auto Regression (VAR), random forest, classification and regression trees (CART), ensemble models (bootstrapping and bagging).
