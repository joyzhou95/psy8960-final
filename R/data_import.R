# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Data Import and Cleaning

## Read in the two datasets using the appropriate separator
review_tbl <- read_delim("../data/satisfaction_reviews.csv", delim = ".", col_names = F)
dataset_tbl <- read_delim("../data/dataset.csv", delim = "+")

## Assign column names to the review dataset
colnames(review_tbl) <- c("satisfaction_txt", "dissatisfaction_txt", "employee_id")

## Order the review dataset by the employee_id column to prepare it for merging 
review_tbl_ordered <- review_tbl %>%
  arrange(employee_id)

## Given that the dataset file was already ordered by employee_id, I am just adding the id column 
## so that it can be merged with the review data
dataset_tbl_num <- dataset_tbl %>%
  mutate(employee_id = 1:nrow(dataset_tbl))

## Merge the two datasets by employee id
ion_final_tbl <- dataset_tbl_num %>%
  left_join(review_tbl_ordered, by = "employee_id")

## Save the merged dataset 
write_csv(ion_final_tbl, "../data/ion_final_tbl.csv")
