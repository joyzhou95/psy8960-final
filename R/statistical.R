# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# Data Import and Cleaning 
ion_tbl <- read_csv("../data/ion_final_tbl.csv")

# Analysis 
## Test of H1
