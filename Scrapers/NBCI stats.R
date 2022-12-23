library(tidyverse);library(RSelenium);library(netstat);library(lubridate)
rm(list = ls());gc();dev.off()

# start the server
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '105.0.5195.52',
                             verbose = FALSE,
                             port = free_port())


remDr <- rs_driver_object$client


NBCI_Selenium <- function(number){
  
  base::suppressMessages(remDr$open()) #SupressMessages virker ikke
  remDr$navigate("https://data.stats.gov.cn/english/easyquery.htm?cn=A01")
  
  remDr$findElement(using = "id", "details-button")$clickElement() #advanced
  
  remDr$findElement(using = "id", "proceed-link")$clickElement() #proceed
  
  Sys.sleep(25)
  
  # industry <- NULL
  # while(is.null(industry)){
  #   industry <- tryCatch({remDr$findElement(using = "xpath", "/html/body/div[4]/div[2]/div/div[1]/div/ul/li/ul/li[2]/a")},
  #                       error = function(e){NULL})
  # }
  remDr$findElement(using = "xpath", "/html/body/div[4]/div[2]/div/div[1]/div/ul/li/ul/li[2]/a")$clickElement() # industry
  
  Sys.sleep(5)
  
  remDr$findElement(using = "xpath", "/html/body/div[4]/div[2]/div/div[1]/div/ul/li/ul/li[2]/ul/li[11]/a/span[1]")$clickElement() #output_of_major_industrial_products
  
  Sys.sleep(5)
  
  #Scraper starter her#
  
  print("Scraping data from National Bureau of China Statistics")
  
  for (i in 1:number){
    
    if (i==1){
      
      start_time <- Sys.time()
      full_data <- tibble()
      
      last_selector <- remDr$findElement(using = "xpath", "/html/body/div[4]/div[2]/div/div[2]/div[1]/div/div/div[2]/div[1]")
      last_selector$clickElement()
      
      input_box <- last_selector <- remDr$findElement(using = "xpath", "/html/body/div[4]/div[2]/div/div[2]/div[1]/div/div/div[2]/div[2]/div[3]/input")
      input_box$clickElement()
      input_box$sendKeysToElement(list("last250", key = "enter"))
      
      submit <- remDr$findElement(using = "xpath", "/html/body/div[4]/div[2]/div/div[2]/div[1]/div/div/div[2]/div[2]/div[3]/div[1]")
      submit$clickElement()
      
    }
    
    data_type <- remDr$findElement(using ="xpath", paste0("/html/body/div[4]/div[2]/div/div[1]/div/ul/li/ul/li[2]/ul/li[11]/ul/li[",i,"]/a"))
    data_type$clickElement()
    
    print(paste("Scraping data for", data_type$getElementText()[[1]], "-", i, "out of", number))

    Sys.sleep(5)
    
    table <- remDr$findElement(using = "xpath", "/html/body/div[4]/div[2]/div/div[2]/div[2]/div[2]/div/div[1]/table")
    table_html <- table$getPageSource()
    
    data <- rvest::read_html(table_html %>% unlist())
    
    
    NBCdata <- rvest::html_table(data)[[1]] %>% 
      t() %>% 
      data.frame() %>% 
      mutate(UniqueId = data_type$getElementText()[[1]]) 
    
    
    NBCdata <- rownames_to_column(NBCdata)
    
    
    colnames(NBCdata) <- c("Date", "Current", "Accumulated", "CurrentGrowth", "AccumulatedGrowth", "AttributeID")
    NBCdata <- NBCdata[2:nrow(NBCdata),]
    row.names(NBCdata)
    
    # NBCdata <- NBCdata %>% 
    #   mutate(Date=row.names(NBCdata)) %>% 
    #   tibble() 
    
    full_data <- rbind(full_data, data.frame(NBCdata))
    
    if (i==number){
      
      end_time <- Sys.time()
      rm(table_html, data, NBCdata)
      print(paste("Script has finished in", lubridate::time_length(round(end_time-start_time,2), unit = "seconds"), "seconds"))
      return(tibble(full_data))

    }
  }
}

NBCI_data <- NBCI_Selenium(number = 20)

NBCI_data$Date <- lubridate::my(NBCI_data$Date)
NBCI_data[,2:5] <- map_df(NBCI_data[,2:5],as.numeric)
NBCI_data$AttributeID <- as.factor(NBCI_data$AttributeID)

NBCI_data1 <- NBCI_data 
NBCI_data <- pivot_longer(NBCI_data, cols = 2:5, names_to = "unit")



NBCI_data %>% 
  filter(unit=="Current") %>% 
  na.omit() %>% 
  ggplot(aes(x=Date, y=log(as.numeric(value)), color = AttributeID)) +
  geom_line(show.legend = F) +
  facet_wrap(~AttributeID, scales="free_y")




NBCdata <- NBCI_data %>% 
  mutate(mean = Accumulated/2) %>% 
  relocate(Date, Current, mean)

NBCdata <- NBCdata %>% 
  mutate(Current=ifelse(is.na(Current), mean, Current)) %>% 
  fill(Current, .direction = "down")




