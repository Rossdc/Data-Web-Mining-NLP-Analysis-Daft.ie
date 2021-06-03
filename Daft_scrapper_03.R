########################################################

# DAFT WEB SCRAPPER and FILTERING

########################################################
# working directory 
setwd("C:\\Users\\foamy\\Desktop\\College\\data_mining\\Project")

# packages 
library(rvest)
library(dplyr)
library(tidyverse)
library(stringr)
library(stringi)
library(ggplot2)
library(ggpubr)

# Creating functions that will use the URL of all house listings and find the below values within each listing URL ...
# ... Grabbing and writing the data below 
house_size <- function(ex){ # house size sqM
  ex <- read_html(ex)
  size <- ex %>% html_nodes('.Zimjn~ .Zimjn+ .Zimjn') %>% html_text() %>% paste(collapse = '')
  return(size)
}


bedrooms <- function(b){ #Bedrooms number
  b <- read_html(b)
  bed <- b %>% html_nodes('.Zimjn:nth-child(1)') %>% html_text() %>% paste(collapse = '')
  return(bed)
}


bath <- function(ba){ # Bathrooms number
  ba <- read_html(ba)
  baths <- ba %>% html_nodes('.Zimjn:nth-child(2)') %>% html_text() %>% paste(collapse = '')
  return(baths)
}

energy <- function(en){ # House energy comsumption
  en <- read_html(en)
  nrg <- en %>% html_nodes('.WvFZH') %>% html_text(word(1)) %>% paste(collapse = '')
  return(nrg)
}


list_desc <- function(ld){ # Listing description
  ld <- read_html(ld)
  descrip <- ld %>% html_nodes('.kDFIyQ') %>% html_text(trim = TRUE) %>% paste(collapse = '')
  return(descrip)
}

seller <- function(se){ # Seller data
  se <- read_html(se)
  sell <- se %>% html_nodes('.hfxRoi') %>% html_text(trim = TRUE) %>% paste(collapse = '')
  return(sell)
}

# Property views
view <- function(v){ 
  v <- read_html(v)
  views_count <- v %>% html_nodes('.fCcbRt+ .fCcbRt .pBjQg') %>% html_text() %>% paste(collapse = '')
  return(views_count)
}


# Empty dataframe to write too
emp_data <- data.frame()



# --------------
# --------------
# --------------
                  # web scrapping
                  # Please run all vectors / variables individually BEFORE running the loop
# --------------
# --------------
# --------------

# Placing the web scraper into a loop which will scrape multiple pages and place the scrapped data into
# the above mentioned empty dataframe
for(i in seq(0,460,20)) {
web <- paste0('https://www.daft.ie/property-for-sale/ireland?salePrice_to=300000&location=dublin-city&location=meath&location=wicklow&location=wexford&location=cork&location=galway&pageSize=20&from=', i)
html <- read_html(web)

list_title <- html %>% html_nodes('.knPImU') %>% html_text() # Titles
list_title

price <- html %>% html_nodes('.pJtsY .gDBFnc') %>% html_text() # Listing price
price


house_type <- html %>% html_nodes('.bcaKbv') %>% html_text()
house_type

listing_url <- html %>% html_nodes('.itNYNv a') %>% html_attr('href') %>% paste('http://www.daft.ie', ., sep='') # Fidning the URL of each listing
listing_size <- sapply(listing_url, house_size) # using the listing URL and the function house_size to scrape the size info

beds <- sapply(listing_url, bedrooms) # using the listing URL and the function bedrooms to scrape the size info
baths = sapply(listing_url, bath)
sellers = sapply(listing_url, seller)
views = sapply(listing_url, view)
views

energy_rating <- sapply(listing_url, energy) # using the listing URL and the function energy to scrape the size info

description <- sapply(listing_url, list_desc)# using the listing URL and the function date to scrape the size info

# --------------
# --------------
                # End of webscrapping 
# --------------
# --------------

## Cleaning the data
# removing unwanted characters
description = gsub('[\r\n]', '', description)
energy_rating = gsub('[kWh/m2/yr]', '', energy_rating)
beds = gsub('[Bed]', '', beds)
baths = gsub('[Bath]', '', baths)
price = gsub('[AMV € : ,]', '', price)
listing_size = gsub('[m²]', '', listing_size)



#  writing of all the above vectors / veriables to a the empty dataframe
emp_data <- rbind(emp_data, data.frame(sellers[1:18], views[1:18], list_title[1:18], price[1:18],
                                       baths[1:18], beds[1:18],
                                       house_type[1:18], energy_rating[1:18], listing_size[1:18],
                                       description[1:18])) 

# Printing the web page so we know what page we are scrapping
print(web)
# Putting the script to sleep so the website wont kick us off
Sys.sleep(0.9)

}

## Updating the column names
colnames(emp_data) = c('seller','property views', 'listing_title', 'price', 'bathrooms', 'bedrooms', 'house_type', ' energy_rating(Kwh)', 'listing_size(m2)', 'description')

# saving the dataset
write.csv(emp_data, 'daft_datav03.csv', 
          row.names = F)

## Reviewing the data
head(emp_data)

