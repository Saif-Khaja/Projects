library(tidyverse);library(RSelenium);library(netstat);library(rvest)
rm(list = ls());gc();dev.off()

# start the server
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '105.0.5195.52',
                             verbose = FALSE,
                             port = free_port())

remDr <- rs_driver_object$client

BoxOfficeMojo <- function(){

  for(i in 1:12){
    
    if(i == 1){
      
      df <- tibble()
      BoxOffice <- tibble()
      
      remDr$open()
      Sys.sleep(0.5)
      remDr$navigate("https://www.boxofficemojo.com/month/january/?grossesOption=calendarGrosses")
      Sys.sleep(0.5)
    }
    
    DropDownMenu <- remDr$findElement(using = "xpath", "/html/body/div[1]/main/div/div/div[1]/div[2]/span/form/span/span/span")
    DropDownMenu$clickElement()
    
    print(paste("Getting data for month", month.name[i]))
    
    Sys.sleep(1)
    
    Month <- remDr$findElement("xpath", paste0("/html/body/div[3]/div/div/ul/li[",i,"]/a"))
    Month$clickElement()
    
    data_table <- remDr$findElement(using = "id", "table")
    
    html_data_table <- data_table$getPageSource()
    
    page <- read_html(html_data_table %>%  unlist())
    
    df <- html_table(page)[[2]] %>% 
      mutate(Month=month.name[i])
    
    BoxOffice <- rbind(BoxOffice, df)
    
    if(i==12){
      
      print("Retrieved all box office data")
      
      return(BoxOffice)
      
      rm(df, html_data_table, page, Month, i, DropDownMenu)
      
    }
  }
}

BoxOfficeMojo <- BoxOfficeMojo()

BoxOffice <- BoxOfficeMojo %>% 
  select(-7:-9)

BoxOffice[, c(2,3,5,7,8)] <- map_df(BoxOffice[, c(2,3,5,7,8)], str_remove_all, pattern = "[,$%]") %>% map_df(as.numeric)

BoxOffice$Month <- factor(BoxOffice$Month, levels = month.name)
fct_count(BoxOffice$Month)

ggplot(BoxOffice, aes(x=Year, y = `Cumulative Gross`, color=Month)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method="lm", se = FALSE, show.legend = FALSE) +
  facet_grid(~Month, scales="free_y") +
  theme_light() +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text.y = element_text(angle = 90))



plotly::ggplotly(ggplot(BoxOffice, aes(x=Year, y = `Cumulative Gross`/1000000, color=Month)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(y="Cumulative Gross in million USD", 
       x=""))

BoxOffice %>% 
  filter(Year>2000, 
         `Cumulative Gross` > 1000000000) %>% 
  ggplot(aes(x=Year, y = `Cumulative Gross`, color=`#1 Release`)) +
  geom_point(show.legend = F, size = 0.5) +
  geom_text(aes(label=`#1 Release`), show.legend = F, size = 2.5)
  



BoxOffice %>% 
  group_by(Year) %>% 
  summarise(mean_revenue = mean(`Cumulative Gross`)/1000000) %>% 
  arrange(desc(mean_revenue)) %>% 
  ggplot(aes(x=Year, y = mean_revenue)) +
  geom_col(fill = "gray1") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(y= "Mean Revenue mil $", x = "",
       title = "Mean Revenue by Year") +
  theme_light() +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(panel.border = element_rect(color = "white")) +
  scale_x_continuous(n.breaks = 10)


BoxOffice %>% 
  group_by(Year) %>% 
  summarise(mean_revenue = mean(`Cumulative Gross`)/1000000) %>% 
  ggplot(aes(x=Year, y = mean_revenue)) +
  geom_line(size = 0.75) +
  tidyquant::geom_ma(n = 3, linetype=1, color = "gray1")


