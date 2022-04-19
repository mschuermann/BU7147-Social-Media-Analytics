#Scraping Product Reviews from Amazon

#import libraries
library(rvest)
library(stringr)
library(xml2)
library(tidyverse)


scrape_amazon <- function(ASIN, page_num){
  
  #amazon_uk
  #url_reviews <- paste0("https://www.amazon.co.uk/product-reviews/",ASIN,"/?pageNumber=",page_num)
  #amazon.com
  #url_reviews <- paste0("https://www.amazon.com/product-reviews/",ASIN,"/?pageNumber=",page_num)
  #amazon.in
  #url_reviews <- paste0("https://www.amazon.in/product-reviews/",ASIN,"/?pageNumber=",page_num)
  
  #amazon.com.au
  url_reviews <- paste0("https://www.amazon.com.au/product-reviews/",ASIN,"/?pageNumber=",page_num)
  
  doc <- read_html(url_reviews) # Assign results to `doc`
  
  # Review Title
  doc %>% 
    html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
    html_text() -> review_title1
  
  review_title <- str_squish(review_title1)
  
  # Review Text
  doc %>% 
    html_nodes("[class='a-size-base review-text review-text-content']") %>%
    html_text() -> review_text1
  
  review_text <- str_squish(review_text1)
  
  # Number of stars in review
  doc %>%
    html_nodes("[data-hook='review-star-rating']") %>%
    html_text() -> review_star1
  
  review_star <- str_squish(review_star1)
  
  # Date 
  
  doc %>%
    html_nodes("[data-hook='review-date']") %>%
    html_text() -> review_date1
  review_date <- str_squish(review_date1)
  
  # Return a tibble
 tibble(review_title,
         review_text,
         review_star,
         review_date,
         page = page_num) %>% return()
}


ASIN <- "B09MW17JQY" # Specify ASIN
page_range <- 1 # Let's say we want to scrape pages 1 to 10

match_key <- tibble(n = page_range,
                    key = sample(page_range,length(page_range)))

lapply(page_range, function(i){
  j <- match_key[match_key$n==i,]$key
  
  message("Getting page ",i, " of ",length(page_range), "; Actual: page ",j) # Progress bar
  
  Sys.sleep(3) # Take a three second break
  
  if((i %% 3) == 0){ # After every three scrapes... take another two second break
    
    message("Taking a break...") # Prints a 'taking a break' message on your console
    
    Sys.sleep(2) # Take an additional two second break
  }
  scrape_amazon(ASIN = ASIN, page_num = j) # Scrape
}) -> output_list


write.csv(output_list,"C:\\Users\\Shruthi\\OneDrive\\Documents\\Semester 2\\Social Media Analytics\\Group Assignment\\Samsung_Australia.csv",row.names = FALSE)
