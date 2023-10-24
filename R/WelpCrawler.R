WelpCrawler <- function(yurl, start_page = NULL, end_init = NULL, filename = NULL){
  
  library(rvest)
  library(tidyverse)
  library(stringr)
  library(writexl)
  
  end_page <- read_html(yurl) %>%
    html_elements(xpath = "//div[starts-with(@class, 'pagination-link')]") %>%
    html_elements(xpath = "//div[contains(@aria-label, 'Page')]") %>%
    html_text() %>%
    as.numeric() %>%
    max()
  
  if(!is.null(end_init)){
    last_page = end_init
  } else{
    last_page = end_page
  }
  
  nxtpage_init <- 10
  
  if(is.null(start_page) || start_page == 1){
    page_seq <- seq(from=0,to=(last_page * 10)-10, by=10)
  } else{
    page_seq <- seq( from=(nxtpage_init*start_page)-10, to=(last_page * 10)-10, by=10 )
  }
  
  review_dates_all <- data.frame()
  review_datesnR_all <- data.frame()
  review_ratings_all <- data.frame()
  review_comments_all <- data.frame()
  
  welp_reviews <- data.frame()
  
  for (i in page_seq) {
    if(i == 0){
      page0 <- read_html(yurl)
    } else {
      page0 <- read_html(paste0(yurl, '?start=', i))
    }
    
    review_dates <- page0 %>%
      html_elements(xpath = "//*[@class=' css-chan6m']") %>%
      html_text() %>%
      .[str_detect(., "^\\w{3}[\\s]\\d+[\\W][\\s]\\d{4}")] %>%
      date_reformat(form = "R") 
    
    review_datesnR <- page0 %>%
      html_elements(xpath = "//*[@class=' css-chan6m']") %>%
      html_text() %>%
      .[str_detect(., "^\\w{3}[\\s]\\d+[\\W][\\s]\\d{4}")] %>%
      date_reformat() 
    
    review_comments_complete <- page0 %>% 
      html_elements(xpath = "//*[@class= ' raw__09f24__T4Ezm']") %>%
      html_text() %>%
      .[c(4:(nrow(review_dates)+3))] %>%
      as.data.frame()
    
    review_ratings <- page0 %>% 
      html_elements(xpath = "//div[starts-with(@class, 'arrange-unit__09f24__rqHTg css-1qn0b6x')]") %>%
      html_elements(xpath = ".//div[contains(@aria-label, 'star rating')]") %>%
      html_attr('aria-label') %>%
      str_remove_all(' star rating') %>%
      as.numeric() %>% 
      .[c(2:(nrow(review_dates)+1))] %>%
      as.data.frame()
    
    review_dates_all <- rbind(review_dates, review_dates_all)
    review_datesnR_all <- rbind(review_datesnR, review_datesnR_all)
    review_comments_all <- rbind(review_comments_complete, review_comments_all)
    review_ratings_all <- rbind(review_ratings, review_ratings_all)
  
    
  }
  
  welp_reviews <- cbind(review_dates_all, review_datesnR_all, review_comments_all, review_ratings_all)
  
  colnames(welp_reviews) <- c("Date", "Date_nonstandard", "Comments", "Star_Ratings")
  
  if(!is.null(filename)){
    write_xlsx(welp_reviews,path = paste0(filename,".xlsx"))
  } else{
    #nothing
  }
  
  return(welp_reviews)
  
}

yard_house_base <- WelpCrawler(yurl = yh_url, start_page = 1, end_init = 8)
yard_house_pg9_19 <- WelpCrawler(yurl = yh_url, start_page = 9, end_init = 19)
yard_house_pg20_30 <- WelpCrawler(yurl = yh_url, start_page = 20, end_init = 30)
yard_house_pg31_41 <- WelpCrawler(yurl = yh_url, start_page = 31, end_init = 41)
yard_house_pg42_52 <- WelpCrawler(yurl = yh_url, start_page = 42, end_init = 52)
yard_house_pg53_57 <- WelpCrawler(yurl = yh_url, start_page = 53, end_init = 57)

write_xlsx(list(yard_house_base, 
                yard_house_pg9_19,
                yard_house_pg20_30,
                yard_house_pg31_41,
                yard_house_pg42_52,
                yard_house_pg53_57),
           path = "yard_house.xlsx")

# V2

WelpCrawler <- function(yurl, start_page = NULL, end_init = NULL, filename = NULL){
  
  library(rvest)
  library(tidyverse)
  library(stringr)
  library(writexl)
  
  end_page <- read_html(yurl) %>%
    html_elements(xpath = "//div[starts-with(@class, 'pagination-link')]") %>%
    html_elements(xpath = "//div[contains(@aria-label, 'Page')]") %>%
    html_text() %>%
    as.numeric() %>%
    max()
  
  if(!is.null(end_init)){
    last_page = end_init
  } else{
    last_page = end_page
  }
  
  nxtpage_init <- 10
  
  if(is.null(start_page)){
    page_seq <- seq(from=0,to=(last_page * 10)-10, by=10)
  } else{
    page_seq <- seq( from=(nxtpage_init*start_page)-10, to=(last_page * 10)-10, by=10 )
  }
  
  review_dates_all <- data.frame()
  review_datesnR_all <- data.frame()
  review_dates_clean_all <- data.frame()
  review_ratings_all <- data.frame()
  review_comments_all <- data.frame()
  
  welp_reviews <- data.frame()
  
  for (i in page_seq) {
    if(i == 0){
      page0 <- read_html(yurl)
    } else {
      page0 <- read_html(paste0(yurl, '?start=', i))
    }
    
    review_dates <- page0 %>%
      html_elements(xpath = "//*[@class=' css-chan6m']") %>%
      html_text() %>%
      .[str_detect(., "^\\w{3}[\\s]\\d+[\\W][\\s]\\d{4}")] %>%
      date_reformat(form = "R") 
    
    review_dates_clean <- page0 %>% 
      html_elements(xpath = "//*[@class= ' css-10n911v']") %>%
      html_text() %>%
      as.data.frame()
    
    review_datesnR <- page0 %>%
      html_elements(xpath = "//*[@class=' css-chan6m']") %>%
      html_text() %>%
      .[str_detect(., "^\\w{3}[\\s]\\d+[\\W][\\s]\\d{4}")] %>%
      date_reformat() 
    
    review_comments_complete <- page0 %>% 
      html_elements(xpath = "//*[@class= ' raw__09f24__T4Ezm']") %>%
      html_text() %>%
      .[c(4:(nrow(review_dates)+3))] %>%
      as.data.frame()
    
    review_ratings <- page0 %>% 
      html_elements(xpath = "//div[starts-with(@class, 'arrange-unit__09f24__rqHTg css-1qn0b6x')]") %>%
      html_elements(xpath = ".//div[contains(@aria-label, 'star rating')]") %>%
      html_attr('aria-label') %>%
      str_remove_all(' star rating') %>%
      as.numeric() %>% 
      .[c(2:(nrow(review_dates)+1))] %>%
      as.data.frame()
    
    review_dates_all <- rbind(review_dates, review_dates_all)
    review_dates_clean_all <- rbind(review_dates_clean, review_dates_clean_all)
    review_datesnR_all <- rbind(review_datesnR, review_datesnR_all)
    review_comments_all <- rbind(review_comments_complete, review_comments_all)
    review_ratings_all <- rbind(review_ratings, review_ratings_all)
    
    
  }
  
  welp_reviews <- cbind(review_dates_all, 
                        review_datesnR_all, 
                        review_dates_clean_all, 
                        review_comments_all, 
                        review_ratings_all)
  
  colnames(welp_reviews) <- c("Date", "Date_nonstandard", "Date_duplicate_indicator", "Comments", "Star_Ratings")
  
  if(!is.null(filename)){
    write_xlsx(welp_reviews,path = paste0(filename,".xlsx"))
  } else{
    #nothing
  }
  
  return(welp_reviews)
  
}

yard_house_base <- WelpCrawler(yurl = yh_url, end_init = 8)
yard_house_pg9_19 <- WelpCrawler(yurl = yh_url, start_page = 9, end_init = 19)
yard_house_pg20_30 <- WelpCrawler(yurl = yh_url, start_page = 20, end_init = 30)
yard_house_pg31_41 <- WelpCrawler(yurl = yh_url, start_page = 31, end_init = 41)
yard_house_pg42_52 <- WelpCrawler(yurl = yh_url, start_page = 42, end_init = 52)
yard_house_pg53_57 <- WelpCrawler(yurl = yh_url, start_page = 53, end_init = 57)

write_xlsx(list(yard_house_base, 
                yard_house_pg9_19,
                yard_house_pg20_30,
                yard_house_pg31_41,
                yard_house_pg42_52,
                yard_house_pg53_57),
           path = "yard_house.xlsx")


# V3-Glue fun 
WelpCrawler <- function(yurl, start_page = NULL, end_init = NULL, filename = NULL){
  
  library(rvest)
  library(tidyverse)
  library(stringr)
  library(writexl)
  library(glue)
  
  end_page <- read_html(yurl) %>%
    html_elements(xpath = "//div[starts-with(@class, 'pagination-link')]") %>%
    html_elements(xpath = "//div[contains(@aria-label, 'Page')]") %>%
    html_text() %>%
    as.numeric() %>%
    max()
  
  if(!is.null(end_init)){
    last_page = end_init
  } else{
    last_page = end_page
  }
  
  nxtpage_init <- 10
  
  if(is.null(start_page)){
    page_seq <- seq(from=0,to=(last_page * 10)-10, by=10)
  } else{
    page_seq <- seq( from=(nxtpage_init*start_page)-10, to=(last_page * 10)-10, by=10 )
  }
  
  review_dates_all <- data.frame()
  review_datesnR_all <- data.frame()
  review_ratings_all <- data.frame()
  review_comments_all <- data.frame()
  
  welp_reviews <- data.frame()
  
  for (i in page_seq) {
    if(i == 0){
      page0 <- read_html(yurl)
    } else {
      page0 <- read_html(glue(yurl, '?start=', i))
    }
    
    review_dates <- page0 %>%
      html_elements(xpath = "//*[@class=' css-chan6m']") %>%
      html_text() %>%
      .[str_detect(., "^\\w{3}[\\s]\\d+[\\W][\\s]\\d{4}")] %>%
      date_reformat(form = "R") 
    
    review_datesnR <- page0 %>%
      html_elements(xpath = "//*[@class=' css-chan6m']") %>%
      html_text() %>%
      .[str_detect(., "^\\w{3}[\\s]\\d+[\\W][\\s]\\d{4}")] %>%
      date_reformat() 
    
    review_comments_complete <- page0 %>% 
      html_elements(xpath = "//*[@class= ' raw__09f24__T4Ezm']") %>%
      html_text() %>%
      .[c(4:(nrow(review_dates)+3))] %>%
      as.data.frame()
    
    review_ratings <- page0 %>% 
      html_elements(xpath = "//div[starts-with(@class, 'arrange-unit__09f24__rqHTg css-1qn0b6x')]") %>%
      html_elements(xpath = ".//div[contains(@aria-label, 'star rating')]") %>%
      html_attr('aria-label') %>%
      str_remove_all(' star rating') %>%
      as.numeric() %>% 
      .[c(2:(nrow(review_dates)+1))] %>%
      as.data.frame()
    
    review_dates_all <- rbind(review_dates, review_dates_all)
    review_datesnR_all <- rbind(review_datesnR, review_datesnR_all)
    review_comments_all <- rbind(review_comments_complete, review_comments_all)
    review_ratings_all <- rbind(review_ratings, review_ratings_all)
    
    
  }
  
  welp_reviews <- cbind(review_dates_all, review_datesnR_all, review_comments_all, review_ratings_all)
  
  colnames(welp_reviews) <- c("Date", "Date_nonstandard", "Comments", "Star_Ratings")
  
  if(!is.null(filename)){
    write_xlsx(welp_reviews,path = paste0(filename,".xlsx"))
  } else{
    #nothing
  }
  
  return(welp_reviews)
  
}

yard_house_base <- WelpCrawler(yurl = yh_url, end_init = 8)
yard_house_pg9_19 <- WelpCrawler(yurl = yh_url, start_page = 9, end_init = 19)
yard_house_pg20_30 <- WelpCrawler(yurl = yh_url, start_page = 20, end_init = 30)
yard_house_pg31_41 <- WelpCrawler(yurl = yh_url, start_page = 31, end_init = 41)
yard_house_pg42_52 <- WelpCrawler(yurl = yh_url, start_page = 42, end_init = 52)
yard_house_pg53_57 <- WelpCrawler(yurl = yh_url, start_page = 53, end_init = 57)

write_xlsx(list(yard_house_base, 
                yard_house_pg9_19,
                yard_house_pg20_30,
                yard_house_pg31_41,
                yard_house_pg42_52,
                yard_house_pg53_57),
           path = "yard_house.xlsx")


# Shiny app V- Date issue

WelpCrawler <- function(yurl, start_page = NULL, end_init = NULL, filename = NULL){
  
  library(rvest)
  library(tidyverse)
  library(stringr)
  library(writexl)
  
  end_page <- read_html(yurl) %>%
    html_elements(xpath = "//div[starts-with(@class, 'pagination-link')]") %>%
    html_elements(xpath = "//div[contains(@aria-label, 'Page')]") %>%
    html_text() %>%
    as.numeric() %>%
    max()
  
  if(!is.null(end_init)){
    last_page = end_init
  } else{
    last_page = end_page
  }
  
  nxtpage_init <- 10
  
  if(is.null(start_page) || start_page == 1){
    page_seq <- seq(from=0,to=(last_page * 10)-10, by=10)
  } else{
    page_seq <- seq( from=(nxtpage_init*start_page)-10, to=(last_page * 10)-10, by=10 )
  }
  
  review_datesnR_all <- data.frame()
  review_ratings_all <- data.frame()
  review_comments_all <- data.frame()
  
  welp_reviews <- data.frame()
  
  for (i in page_seq) {
    if(i == 0){
      page0 <- read_html(yurl)
    } else {
      page0 <- read_html(paste0(yurl, '?start=', i))
    }
    
    review_datesnR <- page0 %>%
      html_elements(xpath = "//*[@class=' css-chan6m']") %>%
      html_text() %>%
      .[str_detect(., "^\\w{3}[\\s]\\d+[\\W][\\s]\\d{4}")] %>%
      date_reformat() 
    
    review_comments_complete <- page0 %>% 
      html_elements(xpath = "//*[@class= ' raw__09f24__T4Ezm']") %>%
      html_text() %>%
      .[c(4:(nrow(review_dates)+3))] %>%
      as.data.frame()
    
    review_ratings <- page0 %>% 
      html_elements(xpath = "//div[starts-with(@class, 'arrange-unit__09f24__rqHTg css-1qn0b6x')]") %>%
      html_elements(xpath = ".//div[contains(@aria-label, 'star rating')]") %>%
      html_attr('aria-label') %>%
      str_remove_all(' star rating') %>%
      as.numeric() %>% 
      .[c(2:(nrow(review_dates)+1))] %>%
      as.data.frame()
    
    review_datesnR_all <- rbind(review_datesnR, review_datesnR_all)
    review_comments_all <- rbind(review_comments_complete, review_comments_all)
    review_ratings_all <- rbind(review_ratings, review_ratings_all)
    
    
  }
  
  welp_reviews <- cbind(review_datesnR_all, review_comments_all, review_ratings_all)
  
  colnames(welp_reviews) <- c("Date", "Comments", "Star_Ratings")
  
  if(!is.null(filename)){
    write_xlsx(welp_reviews,path = paste0(filename,".xlsx"))
  } else{
    #nothing
  }
  
  return(welp_reviews)
  
}

yard_house_base <- WelpCrawler(yurl = yh_url, end_init = 2)
