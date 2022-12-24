# Projects
This repository contains a diverse set of projects including scraping, data cleaning, wrangling and forecasting scripts along with API's that I have written in Rstudio. The data I get is either through a public API or scraped from various sites. 

### API
I get the data from public API and then feed the data into my SQL database to quickly retrieve the data instead of running the API request again. The USDA API contains almost 3 million rows of data across 64 commodities with indicators such as production, consumption, inventory, import, export, and etc.  

### Scraping
Scraping projects are done by either using the [Rvest](https://cran.r-project.org/web/packages/rvest/rvest.pdf) library if it a static website else [Rselenium](https://towardsdatascience.com/how-to-use-selenium-to-web-scrape-with-example-80f9b23a843a) is used to interact with HTML code on the website. After I have retrieved the data it gets cleaned and transformed. In this process I use a multiple libraries within the Tidyverse package such as Dplyr, Purr, Stringr/Stringi to work with strings, Tidyr to reshape data, and then finally ggplot2 or Plotly to visualize the data.

### Modelling
I work with a lot of time series data so usually I will forecast the data using a ARIMA model for few series or a smoothing model if the volume of series are large. To do this I use the forecast library and [book](https://otexts.com/fpp3/) written by Rob J Hyndman and others . 
