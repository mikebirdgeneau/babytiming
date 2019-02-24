
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Hughes Baby Predictor"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width=3,
      dateInput("dueDate","Due Date:",value=as.Date("2019-03-11")),
      dateInput("dateOfInterest","Date of Interest:",value=Sys.Date())#,
      #numericInput("parentalLeave",label = "Days Off:",min = 1,max = 90,step = 1,value = 14)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("babyPlot"),
      uiOutput("messages")
    )
  )
))
