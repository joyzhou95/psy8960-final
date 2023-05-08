#Load required packages
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
      #Set up selectInput for outcomes, department, education field, gender, and job roles
      # to allow users to filter data based on their selections
      selectInput("variables", "Select the outcome that you want to examine",
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
    
    # Show the generated distribution and descriptive table in the main panel
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
    # Filter data based on users' selections 
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
    
    # Generate distribution plots using barplots or histograms based on the selection of the outcome 
    if (input$variables == "Attrition"){
      ion_skiny_tbl %>%
        ggplot(aes_string(input$variables)) + 
        geom_bar() + 
        labs(y = "Frequency")
    } else if (input$variables != "None"){
      ion_skiny_tbl %>%
        ggplot(aes_string(input$variables)) + 
        geom_histogram() + 
        labs(y = "Frequency")
    # When none of the outcome was selected, don't display anything 
    } else {
      NULL
    }
  })
  
  output$table <- renderTable({
    
    # Convert the attrition variable to numeric
    ion_skiny_tbl_t <- ion_skiny_tbl %>%
      mutate(Attrition = recode(Attrition, 
                                "Yes" = 0,
                               "No" = 1))
    
    # Create an empty list
    filter_list <- c()
    
    # Append variables to the list based on users' selections
    if (input$department != "All"){
      filter_list <- append(filter_list, "Department")}
    
    if (input$education != "All") {
      filter_list <- append(filter_list, "EducationField")}
    
    if (input$gender != "All") {
      filter_list <- append(filter_list, "Gender")}
    
    if (input$job_role != "All"){
      filter_list <- append(filter_list, "JobRole")} 
    
    # Group the dataset by the list of variables
    ion_skiny_tbl_t <- ion_skiny_tbl_t %>%
      group_by_at(filter_list)
    
    # Calculate the means and sds of the outcome selected by users
    if (input$variables != "None"){
      ion_skiny_tbl_t %>%
        summarise(Mean = mean(.data[[input$variables]]),
                  SD = sd(.data[[input$variables]])) 
    # When none of the outcome was selected, don't display anything 
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
