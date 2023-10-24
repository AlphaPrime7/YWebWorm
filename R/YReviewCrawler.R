bk_url <- Sys.getenv('broadmore_url')
bk_pg2 <- paste0(bk_url,"?start=10") 
yh_url <- Sys.getenv('yardhouse_url')
yh_url <- paste0(yh_url,"?start=80") 

# USAGE: USE WITH CAUTION AND UNDER LAWS AND REGULATIONS

YReviewCrawler <- function(yurl, start_page = NULL, end_init = NULL, filename=NULL){
  
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

  
  review_comments_all <- c()
  review_people_all <- c()
  review_states_all <- c()
  
  for (i in page_seq) {
    if(i == 0){
      page0 <- read_html(yurl)
    } else {
      page0 <- read_html(paste0(yurl, '?start=', i))
    }
    
    review_comments <- page0 %>% 
      html_elements(xpath = "//*[@class= 'comment__09f24__D0cxf css-qgunke']") %>%
      html_text()
    
    review_states <- page0 %>% 
      html_elements(xpath = "//*[@class= ' css-174a15u']") %>%
      html_text() %>%
      .[c(2:(length(review_comments)+1))] %>%
      str_remove('.* ') %>%
      str_extract("\\w{2}")
    
    review_people <- page0 %>% 
      html_elements(xpath = "//*[@class= ' css-174a15u']") %>%
      html_text() %>%
      .[c(2:(length(review_comments)+1))] %>%
      str_remove(' .*')
    
    
    review_comments_all <- append(review_comments_all, review_comments)
    
    review_states_all <- append(review_states_all, review_states)
    
    review_people_all <- append(review_people_all, review_people)
    
  }
  
  yreviews <- data.frame('Reviewer' = review_people_all,
                         'comments' = review_comments_all, 
                         'state'=review_states_all)
  
  if(!is.null(filename)){
    write_xlsx(yreviews,path = paste0(filename,".xlsx"))
  } else{
    #nothing
  }
  
  return(yreviews)
  
}

#pg1-7 done
yard_house_base <- YReviewCrawler(yurl = yh_url, end_init = 8)
yard_house_pg9_19 <- YReviewCrawler(yurl = yh_url, start_page = 9, end_init = 19)
yard_house_pg20_30 <- YReviewCrawler(yurl = yh_url, start_page = 20, end_init = 30)
yard_house_pg31_41 <- YReviewCrawler(yurl = yh_url, start_page = 31, end_init = 41)
yard_house_pg42_52 <- YReviewCrawler(yurl = yh_url, start_page = 42, end_init = 52)
yard_house_pg53_57 <- YReviewCrawler(yurl = yh_url, start_page = 53, end_init = 57)

write_xlsx(list(yard_house_base, 
                yard_house_pg9_19,
                yard_house_pg20_30,
                yard_house_pg31_41,
                yard_house_pg42_52,
                yard_house_pg53_57),
           path = "yard_house.xlsx")
