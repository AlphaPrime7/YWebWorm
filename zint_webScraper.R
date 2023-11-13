library(rvest)
library(tidyverse)
library(stringr)
library(writexl)

#iMBD Target:

hollywood <- Sys.getenv('imdb')

#mov name
mov_names <- hollywood %>% 
  read_html() %>%
  html_elements(xpath = "//*[@class='ipc-title__text']") %>%
  html_text() %>%
  .[c(2:251)] %>%
  str_remove("\\d+[\\W][\\s]") %>%
  as.data.frame()

colnames(mov_names) <- c("Movie_Titles")

#ratings
<span aria-label="IMDb rating: 9.3" 
class="ipc-rating-star ipc-rating-star--base ipc-rating-star--imdb ratingGroup--imdb-rating"> 
class="ipc-icon ipc-icon--star-inline" 


  


