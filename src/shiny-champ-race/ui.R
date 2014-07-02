library(shiny)
 
shinyUI(pageWithSidebar(
   
  # Application title
  headerPanel("F1 Driver Championship Scenarios 2012"),
   
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("alo", 
                "ALO race pos in United States Grand Prix:", 
                min = 1, 
                max = 11, 
                value = 1),
    sliderInput("vet", 
                "VET race pos in United States Grand Prix:", 
                min = 1, 
                max = 11, 
                value = 2)
  ),
   
  # Show a plot of the generated model
  mainPanel(
    plotOutput("distPlot")
  )
))
