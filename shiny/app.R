library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title & author name
  titlePanel("PSY 8960 Final Week Shiny Project: Joy Zhou"),

  # Sidebar with the select inputs
  sidebarLayout(
    #Set select inputs to be in the sidebar
    sidebarPanel(
      #Set up selectInput for gender, errorband, and competion time to allow users to filter their data based on their selections
      selectInput("variables", "Select the variable that you want to examine",
                  choices = c("Monthly pay", 
                              "Turnover status", 
                              "Overall job satisfaction")),
      selectInput("department", "Select the department that you want to examine", 
                  choices = c("Human Resources", 
                              "Research & Development", 
                              "Sales")),
      selectInput("gender", "Select the gender that you want to examine", 
                  choices = c("Male",
                              "Female")),
      selectInput("job role", "Select the job role that you want to examine", 
                  choices = c("Healthcare Representative",
                              "Human Resources", 
                              "Laboratory Technician", 
                              "Manager", 
                              "Manufacturing Director",
                              "Research Director", 
                              "Research Scientist", 
                              "Sales Executive",
                              "Sales Representative"))
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

    output$fig <- renderPlot({
      # Import the saved RDS dataset that is appropriate for shiny, did not need to specify path as the current wd is the same as the wd of app.R
      ion_skiny_tbl <- read_rds('./ion_tbl.RDS')
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# Load library for deploying app
library(rsconenct)

# Deploy and name the app 
rsconnect::deployApp(appName = "shinny_week8")
