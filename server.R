
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  output$babyPlot <- renderPlot({
    input$dueDate
    input$dateOfInterest
    input$parentalLeave
    isolate({
    result <- babyTiming(ExpectedDate = input$dueDate,DateofInterest=input$dateOfInterest,VacationDuration = input$parentalLeave)
    grid.arrange(result$p1, result$p2, ncol=1)
    })
  })
  
  output$messages <- renderUI({
    result <- data.frame(result=unlist(
      babyTiming(
        ExpectedDate = input$dueDate,
        DateofInterest=input$dateOfInterest,
        VacationDuration = input$parentalLeave)$messages))
    
    list(
      strong(paste0(result$result[1],":")),
      tags$ul(
        tags$li(result$result[2]),
        tags$li(result$result[3]),
        tags$li(result$result[4])
      )
    )
    
    #lapply(result$result,function(x){
    #  list(
    #    p(x)
    #  )
    #})
    
  })

})
