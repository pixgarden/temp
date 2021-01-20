#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('rsconnect')
library(rsconnect)
library(pdftools)
library(shiny)
library(stringr)

## Only run examples in interactive R sessions
    
    ui <- fluidPage(
        sidebarLayout(
            sidebarPanel(
                fileInput("file1", "Choose this File", accept = ".pdf")
            ),
            mainPanel(
                titlePanel("Extracts"),
                textOutput("date1"),
                textOutput("date2"),
                textOutput("currency"),
                textOutput("price"),
                textOutput("price2"),
                titlePanel("RAW"),
                tableOutput("contents")
            )
        )
    )
    
    server <- function(input, output) {
        

        
        pdfDATA <- reactive({
            
            file <- input$file1
            ext <- tools::file_ext(file$datapath)
            
            req(file)
            validate(need(ext == "pdf", "Please upload a pdf file"))
            
            pdf_text(file$datapath)
        })
        
        output$contents <- renderTable({pdfDATA()})
        
        output$date1 <- renderText({ 
            
            paste("DATE CHECKING >",str_extract_all(pdfDATA()[1], "([0-9][0-9]\\/[0-9][0-9]\\/20[0-9][0-9])")[[1]][1])
        })
        
        output$date2 <- renderText({ 
            
            paste("DATE CHECKOUT >",str_extract_all(pdfDATA()[1], "([0-9][0-9]\\/[0-9][0-9]\\/20[0-9][0-9])")[[1]][2])
            })
    
        
        
        
        output$price <- renderText({ 
            
            paste("PRICE  >",str_match(pdfDATA()[1], "Monthly Budget\\: ([A-Z]+) (.*)\\nTemp")[,3])
        })
        
        output$currency <- renderText({ 
            
            paste("CURRENCY  >",str_match(pdfDATA()[1], "Monthly Budget\\: ([A-Z]+) (.*)\\nTemp")[,2])
        })
        
        
        
        output$price2 <- renderText({ 
            
            paste("PRICE divided by 30 >",as.numeric(gsub("\\,", "", str_match(pdfDATA()[1], "Monthly Budget\\: ([A-Z]+) (.*)\\nTemp")[,3]))/30)
        })
        
        }
    
    
    
    shinyApp(ui, server)

