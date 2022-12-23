library(tidyverse);library(RSelenium);library(netstat);library(lubridate);library(rvest)
rm(list = ls());gc();dev.off()

####Start the server####
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '107.0.5304.62',
                             verbose = FALSE,
                             port = free_port())


remDr <- rs_driver_object$client


remDr$open()

####Scraping Euribor####

for (Year in 1999:2022){
  
  if (Year==1999){
  start_time <- Sys.time()
  euribor <- data.frame()
  Sys.sleep(2)
  }
  
  print(paste("Scraping data from year:", Year))
                         # https://www.global-rates.com/en/interest-rates/euribor/1999.aspx
  remDr$navigate(paste0("https://www.global-rates.com/en/interest-rates/euribor/",Year,".aspx"))
  
  if (Year==1999){
    remDr$findElement(using = "xpath", "/html/body/div/div[2]/div[1]/div[2]/div[2]/button[1]/p")$clickElement()
    Sys.sleep(2)
  }
  
  remDr$findElement(using = "xpath", "/html/body/form/table[4]/tbody/tr/td/table/tbody/tr[2]/td[2]/table[5]/tbody/tr/td/select")$clickElement()
  remDr$findElement(using = "xpath", "/html/body/form/table[4]/tbody/tr/td/table/tbody/tr[2]/td[2]/table[5]/tbody/tr/td/select/option[15]")$clickElement()
  
  interest_table <- remDr$findElement(using="xpath", "/html/body/form/table[4]/tbody/tr/td/table/tbody/tr[2]/td[2]/table[7]/tbody")
  interest_table_html <- interest_table$getPageSource()
  data <- rvest::read_html(interest_table_html %>% unlist())
  data <- rvest::html_table(data)[[1]]  
  
  data <- read_html(interest_table_html %>% unlist()) %>% 
    html_table() %>% 
    .[[494]]
  
  
  euribor <- rbind(euribor, data)

  if (Year==2022){
    
    rm(data, interest_table, interest_table_html, remDr, rs_driver_object, Year)
    end_time <- Sys.time()
    print(paste("The script has finished in", round(time_length(end_time-start_time, unit = "seconds"),2),"seconds"))
  }
}


####Data wrangling####

df <- euribor
colnames(df)[-1] <- df[1,2:6]
  
df <- df %>% 
  mutate(Date=str_extract_all(X1, "([0-9]{4})", simplify = T)) %>% 
  mutate(Date=as.numeric(Date)) %>% 
  fill(Date, .direction = "down") %>% 
  relocate(Date,.after = X1) %>% 
  mutate(Date=paste0(X1,"-",Date)) %>% 
  mutate(Date=lubridate::my(Date)) %>% 
  select(-1)

str(df)

df[,2:ncol(df)] <- map_df(df[,2:ncol(df)], parse_number)

df <- df %>% 
  na.omit()

####Visualization####

df %>% 
  pivot_longer(cols = 2:6, names_to = "Type") %>% 
  ggplot(aes(x=Date, y=value/100, color =Type)) +
  geom_smooth(method = "lm", alpha=0.1, se = F) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(), n.breaks = 12) +
  scale_x_date(date_breaks = "1 years", labels = scales::date_format(format = "%Y")) +
  labs(x="", y="", title="Euribor Interest Rates - 12 Months") +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 15),
        axis.text.x = element_text(angle = 45)) +
  geom_hline(yintercept = 0, color="grey", alpha=0.75) 
