#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(dplyr)
library(lubridate)
library(hflights)
library(stringi)
library(ggplot2)

ui <- fluidPage(
    tagList(tags$head(tags$title("Airline Delays")), h1(textOutput("title"))),    
    
    sidebarLayout(
      sidebarPanel(
        uiOutput("userPanel")
      ),
      mainPanel(
        plotOutput("box")
      )
    )
  )

server = function(input, output, session) {
  #' Get the current user's username
  user <- reactive({
    session$user
  })
  
  #' Get the current user's group
  groups <- reactive({
    session$groups
  })
  
  output$userPanel <- renderUI({
    HTML(paste0("Logged in as <code>", user(), 
                "</code> with groups <code>", paste(groups(), collapse=', ') ,"</code>."))
  })
  
  filteredData <- reactive({
    data = hflights
    data$date= ymd(paste0(data$Year, stri_pad_left(data$Month,2,"0"), stri_pad_left(data$DayofMonth,2,"0")))
    
    if('Admin' %in% groups()){
      data=filter(data, Month>=1)
    }else{
      data=filter(data, Month>=9)
    }
    
    as.data.frame(data)
  })
  
  #' Print a boxplot of the selected data.
  output$box <- renderPlot({    
    ggplot(filteredData(), aes(factor(Month), DepDelay)) +
      geom_violin(fill='darkblue') +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

