library(shiny)
library(shinythemes)
library(rvest)
library(stringr)

ui <- fluidPage(

    shinythemes::themeSelector(),
    titlePanel("Welp Restaurant Fun Scraper in R"),

    sidebarLayout(
        sidebarPanel(
          textInput("url", "Welp URL to scrape", ""),
          selectInput(
            inputId='param',
            label='Welp Reviews',
            choices=c('people', 'dates', 'reviews', 'stars', "summary"),
            selected='none'
          ), 
        ),

        mainPanel(
          tableOutput('table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
  output$table <- renderTable({
    req(input$url)
    review_comments <- read_html(input$url) %>% 
      html_elements(xpath = "//*[@class= 'comment__09f24__D0cxf css-qgunke']") %>%
      html_text() %>%
      as.data.frame()
    
    if(input$param == 'reviews'){
      req(input$url)
      review_comments <- read_html(input$url) %>% 
        html_elements(xpath = "//*[@class= 'comment__09f24__D0cxf css-qgunke']") %>%
        html_text() %>%
        as.data.frame()
      colnames(review_comments) <- c("Reviews")
      print(review_comments)
    }
    
    if(input$param == 'dates'){
      req(input$url)
      review_dates <- read_html(input$url) %>%
        html_elements(xpath = "//*[@class= ' css-chan6m']") %>%
        html_text() %>%
        .[str_detect(., "^\\w{3}[\\s]\\d+[\\W][\\s]\\d{4}")] %>%
        date_reformat()
      colnames(review_dates) <- c("Dates")
      print(review_dates)
    }
    
    if(input$param == 'people'){
      req(input$url)
      review_people <- read_html(input$url) %>% 
        html_elements(xpath = "//*[@class= ' css-174a15u']") %>%
        html_text() %>%
        .[c(1:nrow(review_comments)+1)] %>%
        str_remove(' .*') %>%
        as.data.frame()
      colnames(review_people) <- c("People")
      print(review_people)
    }
    
    if(input$param == 'stars'){
      req(input$url)
      review_ratings <- read_html(bk_url) %>% 
        html_elements(xpath = "//div[starts-with(@class, 'arrange-unit__09f24__rqHTg css-1qn0b6x')]") %>%
        html_elements(xpath = ".//div[contains(@aria-label, 'star rating')]") %>%
        html_attr('aria-label') %>%
        str_remove_all(' star rating') %>%
        as.numeric() %>% 
        .[c(2:(nrow(review_dates)+1))] %>%
        as.data.frame()
      colnames(review_ratings) <- c("Ratings")
      print(review_ratings)
    }
    
    if(input$param == 'summary'){
      req(input$url)
      review_ratings <- read_html(bk_url) %>% 
        html_elements(xpath = "//div[starts-with(@class, 'arrange-unit__09f24__rqHTg css-1qn0b6x')]") %>%
        html_elements(xpath = ".//div[contains(@aria-label, 'star rating')]") %>%
        html_attr('aria-label') %>%
        str_remove_all(' star rating') %>%
        as.numeric() %>% 
        .[c(2:(nrow(review_dates)+1))] %>%
        as.data.frame()
      colnames(review_ratings) <- c("Ratings")
      
        review_people <- read_html(input$url) %>% 
          html_elements(xpath = "//*[@class= ' css-174a15u']") %>%
          html_text() %>%
          .[c(1:nrow(review_comments)+1)] %>%
          str_remove(' .*') %>%
          as.data.frame()
        colnames(review_people) <- c("People")
        
        review_dates <- read_html(input$url) %>%
          html_elements(xpath = "//*[@class= ' css-chan6m']") %>%
          html_text() %>%
          .[str_detect(., "^\\w{3}[\\s]\\d+[\\W][\\s]\\d{4}")] %>%
          date_reformat()
        colnames(review_dates) <- c("Dates")
        
        review_comments <- read_html(input$url) %>% 
          html_elements(xpath = "//*[@class= 'comment__09f24__D0cxf css-qgunke']") %>%
          html_text() %>%
          as.data.frame()
        colnames(review_comments) <- c("Reviews")
  
        
        fundf <- cbind(review_people, review_dates, review_comments, review_ratings)
        print(fundf)
        
      }
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
