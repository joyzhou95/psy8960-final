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
                  selected = "None",
                  choices = c("None",
                              "MonthlyIncome", 
                              "Attrition", 
                              "JobSatisfaction")),
      selectInput("department", "Select the department that you want to examine",
                  selected = "All",
                  choices = c("All",
                              "Human Resources", 
                              "Research & Development", 
                              "Sales")),
      selectInput("education", "Select the education field that you want to examine",
                  selected = "All",
                  choices = c("All",
                              "Human Resources", 
                              "Life Sciences", 
                              "Marketing", 
                              "Medical", 
                              "Technical Degree",
                              "Other")),
      selectInput("gender", "Select the gender that you want to examine", 
                  selected = "All",
                  choices = c("All",
                              "Male",
                              "Female")),
      selectInput("job_role", "Select the job role that you want to examine", 
                  selected = "All",
                  choices = c("All",
                              "Healthcare Representative",
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

server <- function(input, output) {
  # Import the saved RDS dataset that is appropriate for shiny, did not need to specify path as the current wd is the same as the wd of app.R
  ion_skiny_tbl <- readRDS("ion_tbl.RDS")
  
  output$fig <- renderPlot({
    
    if (input$department != "All"){
      ion_skiny_tbl <- ion_skiny_tbl %>%
        filter(Department == input$department)}
    
    if (input$education != "All"){
      ion_skiny_tbl <- ion_skiny_tbl %>%
        filter(EducationField == input$education)}
    
    if (input$gender != "All"){
      ion_skiny_tbl <- ion_skiny_tbl %>%
        filter(Gender == input$gender)}
    
    if (input$job_role != "All"){
      ion_skiny_tbl <- ion_skiny_tbl %>%
        filter(JobRole == input$job_role)}
    
    
    if (input$variables == "Attrition"){
      ion_skiny_tbl %>%
        ggplot(aes_string(input$variables)) + 
        geom_bar() 
    } else if (input$variables != "None"){
      ion_skiny_tbl %>%
        ggplot(aes_string(input$variables)) + 
        geom_histogram() 
    } else {
      NULL
    }
  })
  

}


# Run the application 
shinyApp(ui = ui, server = server)

# Load library for deploying app
#library(rsconnect)

# Deploy and name the app 
#rsconnect::deployApp(appName = "people_dashboard")
