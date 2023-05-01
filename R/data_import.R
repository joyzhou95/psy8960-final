# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

review_tbl <- read_delim("../data/satisfaction_reviews.csv", delim = ".", col_names = F)
dataset_tbl <- read_delim("../data/dataset.csv", delim = "+")

colnames(review_tbl) <- c("satisfaction", "dissatisfaction", "employee_id")
