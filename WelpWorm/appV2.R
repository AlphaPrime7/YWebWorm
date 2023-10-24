library(shiny)
library(shinythemes)
library(rvest)
library(stringr)
library(writexl)

ui <- fluidPage(
  
  shinythemes::themeSelector(),
  titlePanel("Welp Restaurant Fun Scraper in R"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("start", "Start Page-NULL if start is page1", value = NULL, min = 2, step = 1),
      numericInput("end_page", "End Page", value = NULL, min = 1, step = 1),
      textInput("url", "Welp URL to scrape", ""),
      selectInput(
        inputId='param',
        label='Welp Reviews',
        choices=c("review_data"),
        selected='none'
      ), 
      downloadButton("download", "Download .xlsx"), width = 3), 
    
    mainPanel(
      tableOutput('table')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table <- renderTable({
    
    if(input$param == "review_data"){
      req(input$url)
      fundf <- WelpCrawler(input$url, start_page = input$start, end_init = input$end_page)
      print(fundf)
    }
 
  })
  
  rev_input <- reactive({
    req(input$url)
    fundf <- WelpCrawler(input$url, start_page = input$start, end_init = input$end_page)
     })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("Reviews", ".xlsx")},
    content = function(file){
      write_xlsx(rev_input(), file) })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
