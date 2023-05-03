# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# Data Import and Cleaning 
ion_tbl <- read_csv("../data/ion_final_tbl.csv")

# Analysis 
## Test of H1
pay_perf_cor <- ion_tbl %>%
  cor_test(vars = c("MonthlyIncome", "PerformanceRating"))

# Visualization 
## Visualization of H1 
(ggplot(ion_tbl, aes(MonthlyIncome, PerformanceRating)) +
    geom_point(position = "jitter") + 
    geom_smooth(method = "lm", se = F) + 
    labs(x = "Monthly Income", y = "Performance Ratings", 
         title = "The Relationship Between Monthly Pay and Performance Ratings ")) %>%
  ggsave(filename = "../figs/H1.png", units = "px", width = 1920, height = 1080)

# Publication
## Publication Results for H1 

