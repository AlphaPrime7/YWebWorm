library(rvest)
library(tidyverse)
library(stringr)
library(writexl)

z_url <- Sys.getenv('zurl')
z_url <- Sys.getenv('zurl_two')
z_url <- Sys.getenv('zurl_detail')

#iMBD Target:

hollywood <- Sys.getenv('imdb')

#IMDB Scraper
mov_names <- hollywood %>% 
  read_html() %>%
  html_elements(xpath = "//*[@class='ipc-title__text']") %>%
  html_text() %>%
  .[c(2:251)] %>%
  str_remove("\\d+[\\W][\\s]") %>%
  as.data.frame()

colnames(mov_names) <- c("Movie_Titles")

  


