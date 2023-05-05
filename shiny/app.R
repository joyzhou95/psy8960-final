library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title & author name
  titlePanel("PSY 8960 Final Project Shiny Project: Joy Zhou"),

  # Sidebar with the select inputs
  sidebarLayout(
    #Set select inputs to be in the sidebar
    sidebarPanel(
      #Set up selectInput for gender, errorband, and competion time to allow users to filter their data based on their selections
      selectInput("variables", "Select the variable that you want to examine",
                  choices = c("Monthly pay", 
                              "Turnover status", 
                              "Overall job satisfaction")),
      selectInput("dataset", "Select a subset of the overall data grouped by", 
                  choices = c("Department",
                              "Gender",
                              "Job role")),
    ),
    
    # Show the generated distribution and table in the main panel
    mainPanel(
      # Use plotOutput and tableOutput to create non-interactive distribution and table output 
      plotOutput("fig"),
      tableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
