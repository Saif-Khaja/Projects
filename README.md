# Projects
This repository contains a diverse set of projects including scraping, data cleaning, wrangling and forecasting scripts along with API's that I have written in Rstudio.

## Scraping
Scraping projects are done by either using the [Rvest]([url](https://cran.r-project.org/web/packages/rvest/rvest.pdf)) library if it a static website else [Rselenium]([https://towardsdatascience.com/how-to-use-selenium-to-web-scrape-with-example-80f9b23a843a]) is used to interact with HTML code for daton the website. After I have retrieved the data it gets cleaned and transformed. In this process I use a multiple libraries within the Tidyverse package such as Dplyr, Purr, Stringr/Stringi to work with strings, Tidyr to reshape data, and then finally ggplot2 or Plotly to visualize the data.
