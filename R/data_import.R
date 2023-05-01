# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Data Import and Cleaning
review_tbl <- read_delim("../data/satisfaction_reviews.csv", delim = ".", col_names = F)
dataset_tbl <- read_delim("../data/dataset.csv", delim = "+")

colnames(review_tbl) <- c("satisfaction", "dissatisfaction", "employee_id")

review_tbl_ordered <- review_tbl %>%
  arrange(employee_id)

dataset_tbl_num <- dataset_tbl %>%
  mutate(employee_id = 1:nrow(dataset_tbl))

ion_final_tbl <- dataset_tbl_num %>%
  left_join(review_tbl_ordered, by = "employee_id")

write_csv(ion_final_tbl, "../data/ion_final_tbl.csv")
