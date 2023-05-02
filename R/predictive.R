# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

# Data Import and Cleaning 
ion_tbl <- read_csv("../data/ion_final_tbl.csv")

ion_tbl_recode <- ion_tbl %>%
  mutate(Attrition = recode(Attrition, "Yes" = 0, "No" = 1),
         BusinessTravel = recode(BusinessTravel, "Non-Travel" = 0,"Travel_Rarely" = 1,
                                 "Travel_Frequently" = 2),
         Department = recode(Department, "Human Resources" = 0, "Research & Development" = 1, 
                             "Sales" = 2),
         EducationField = recode(EducationField, "Human Resources" = 0, "Life Sciences" = 1, 
                                 "Marketing" = 2, "Medical" = 3, "Technical Degree" = 4,
                                 "Other" = 5),
         Gender = recode(Gender, "Male" = 0, "Female" = 1),
         JobRole = recode(JobRole, "Healthcare Representative" = 0, "Human Resources" = 1, 
                          "Laboratory Technician" = 2, "Manager" = 3, "Manufacturing Director" = 4,
                          "Research Director" = 5, "Research Scientist" = 6, "Sales Executive" = 7,
                          "Sales Representative" = 8),
         MaritalStatus = recode(MaritalStatus, "Single" = 0, "Married" = 1, "Divorced" = 2),
         Over18 = recode(Over18, "Y" = 0),
         OverTime = recode(OverTime, "Yes" = 0, "No" = 1)
         )
