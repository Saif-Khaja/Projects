# Fri Apr 22 01:32:27 2022 ------------------------------
library('tidyverse');library('httr');library('jsonlite');library('plotly');
rm(list=ls());dev.off()

########################################################################
#####                USDA API GET Commmodity Codes                 #####
########################################################################

Get_Commodity_Codes <- function(Commodity_Name, Commodity_Code){
  
  request <- GET("https://apps.fas.usda.gov/PSDOnlineDataServices/api/LookupData/GetCommodities",
                 add_headers(API_KEY = askpass::askpass()))
  
  content(request, as = "text") %>% 
    fromJSON() %>% 
    tibble() 
  
}

Commodity_code <- Get_Commodity_Codes()

########################################################################
#####         USDA API Get Commodity Data By Country and Year      #####
########################################################################

USDA_API_Country <- function(Commodity_Code, StartYear, EndYear){
  
  for (i in 1:length(Commodity_Code)){  
    
    print(paste(paste0(i,"."),"Getting country data for", 
                str_trim(left_join(x=data.frame(CommodityCode=Commodity_Code[i]), y=Commodity_code, by="CommodityCode")[,2], side = "both"), 
                paste0("(",Commodity_Code[i],")")))
    
    for (Year in ifelse(missing(StartYear), 1960, StartYear):ifelse(missing(EndYear), format(Sys.Date(), "%Y"), EndYear)){ #Fix så man ikke behøver specificere start/end year
      
      if(i==1 && Year==StartYear){
        
        start_time <- Sys.time()
        USDA_data <- tibble()
        
      }
      
      print(paste("Retrieving data from", Year)) 

      request <- try({GET("https://apps.fas.usda.gov/PSDOnlineDataServices/api/CommodityData/GetCommodityDataByYear?", query = list(
        CommodityCode = Commodity_Code[i],           
        MarketYear = Year), 
        add_headers(API_KEY = askpass::askpass()))}, silent = TRUE)
      
      
      if (request$status_code==404){
        #Hvis "request" henter en 404 kode, altså en råvare hvor der ikke er data for det år, så
        #skip iteration og gå til næste år hvor der er data.  
        #Det er kun sugar hvor vi ikke får 2023 data pga. denne her løsning.
        next

      }
      
      USDA_data <- rbind(USDA_data, #EVT skift tilbage til den gamle metode. Test efficiency
                         tibble(data.frame(content(request, as = "text") %>% 
                                             fromJSON())))

      
      if(i==length(Commodity_Code) && Year==EndYear){
        
        end_time <- Sys.time()
        print(paste("USDA API sucessfully retrieved data in",round(lubridate::time_length(end_time-start_time, unit = "seconds"),2),"seconds"))
        
        return(tibble(USDA_data))
        
        #remove(Data)
        #remove(request)
      }
    }
  }
}

CountryData <- USDA_API_Country(Commodity_Code = rev(c("0711100", "0410000")), StartYear = 2022, EndYear = 2023)

########################################################################
######           USDA API Get World Commodity Data By Year         #####
########################################################################

USDA_API_World <- function(Commodity_Code, StartYear, EndYear){
  
  for (i in 1:length(Commodity_Code)){
    
    print(paste(paste0(i,"."),"Getting world data for", 
                str_trim(left_join(x=data.frame(CommodityCode=Commodity_Code[i]), y=Commodity_code, by="CommodityCode")[,2], side = "both"), 
                paste0("(",Commodity_Code[i],")")))
    
    for (Year in ifelse(missing(StartYear), 1960, StartYear):ifelse(missing(EndYear), format(Sys.Date(), "%Y"), EndYear)){ #Virker ikke hvorfor?
      
      if(i==1 && Year==StartYear){
        
        start_time <- Sys.time() 
        USDA_data <- tibble()
        
      }
      
      print(paste("Retrieving data from", Year))
      
      request <- try({GET("https://apps.fas.usda.gov/PSDOnlineDataServices/api/CommodityData/GetWorldCommodityDataByYear?", query = list(
        CommodityCode = Commodity_Code[i],           
        MarketYear = Year),
        add_headers(API_KEY = askpass::askpass()))}, silent = TRUE)
      
      if (request$status_code==404){
        #Hvis "request" henter en 404 kode, altså en råvare hvor der ikke er data for det år, så
        #skip iteration og gå til næste år/råvare hvor der er data.  
        next
      }
      
      USDA_data <- rbind(USDA_data, #EVT skift tilbage til den gamle metode. Test efficiency
                         tibble(data.frame(content(request, as = "text") %>% 
                                             fromJSON())))
      
      
      if(i==length(Commodity_Code) && Year==EndYear){
        
        end_time <- Sys.time()
        print(paste("USDA API sucessfully retrieved data in",round(lubridate::time_length(end_time-start_time, unit = "seconds"),2),"seconds"))
        
        return(tibble(USDA_data))
        
        #remove(Data)
        #remove(request)
      }
    }
  }
}

WorldData <- USDA_API_World(Commodity_Code = c("0410000", "0711100"), StartYear = 2000, EndYear = 2023)


