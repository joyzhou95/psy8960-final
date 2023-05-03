# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# Data Import and Cleaning 
ion_tbl1 <- read_csv("../data/ion_final_tbl.csv")

ion_tbl <- ion_tbl1 %>%
  mutate(Gender = recode(Gender, "Male" = 0, "Female" = 1))


# Analysis 
## Test of H1
pay_perf_cor <- ion_tbl %>%
  cor_test(vars = c("MonthlyIncome", "PerformanceRating"))

## Test of H2
pay_depart_anova <- ion_tbl %>%
  anova_test(
    formula = MonthlyIncome ~ Department,
    detailed =T
  )

## Test of H3
h3_reg <- lm(YearsAtCompany ~ RelationshipSatisfaction + RelationshipSatisfaction * Gender,
             data = ion_tbl) 
summary(h3_reg)




# Visualization 
  
## Visualization of H1 
(ggplot(ion_tbl, aes(MonthlyIncome, PerformanceRating)) +
    geom_point(position = "jitter") + 
    geom_smooth(method = "lm", se = F) + 
    labs(x = "Monthly Income", y = "Performance Ratings", 
         title = "The Relationship Between Monthly Pay and Performance Ratings") + 
  theme(plot.title = element_text(hjust = 0.5))) %>%
  ggsave(filename = "../figs/H1.png", units = "px", width = 1920, height = 1080)



## Visualization of H2
(ggplot(ion_tbl, aes( MonthlyIncome, Department)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(x = "Monthly Income", y = "Departments", 
       title = "Monthly Pay by Departments") + 
  theme(plot.title = element_text(hjust = 0.5))) %>%
  ggsave(filename = "../figs/H2.png", units = "px", width = 1920, height = 1080)



## Visualization of H3
tenure_pred <- predict.lm(h3_reg)
ion_tbl_pred <- ion_tbl %>%
  add_column(tenure_pred = tenure_pred)

(ggplot(ion_tbl_pred, aes(x = YearsAtCompany, y = tenure_pred)) + 
  geom_point(position = "jitter") + 
  geom_smooth(method = "lm", se = F) + 
  labs(x = "Tenure", y = "Predicted Tenure", 
       title = "The Relationship Between Observed and Predicted Tenure") + 
  theme(plot.title = element_text(hjust = 0.5))) %>%
  ggsave(filename = "../figs/H3.png", units = "px", width = 1920, height = 1080)




# Publication

## Publication Results for H1 
paste0(
  "The pearson correlation between monthly income and performance ratings was r = ", 
  str_replace(
    format(round(pay_perf_cor$cor, 2), 
           nsmall = 2), 
    "^(-?)0", 
    str_match(pay_perf_cor$cor, 
              '^(-?)0+')[,2]),
  ", p = ",
  str_remove(
    format(
      round(pay_perf_cor$p, 2), 
      nsmall = 2),
    "^0"),
  ". This test was ",
  ifelse(pay_perf_cor$p > 0.05, "not", ""),
  " statistically significant."
)




## Publication results of H2
h2_anova_tbl <- tibble(
  "Source of Variation" = c("Department", 
                            "Error", 
                            "Total"),
  "Sum of Squares" = c(pay_depart_anova$SSn, 
                       pay_depart_anova$SSd, 
                       sum(pay_depart_anova$SSn, pay_depart_anova$SSd)),
  "Degree of Freedom" = c(pay_depart_anova$DFn, 
                          pay_depart_anova$DFd,
                          sum(pay_depart_anova$DFn + pay_depart_anova$DFd)),
  "Mean Squares" = c(pay_depart_anova$SSn/pay_depart_anova$DFn, 
                     pay_depart_anova$SSd/pay_depart_anova$DFd, 
                     NA),
  "F value" = c(format(
    round(pay_depart_anova$F, 2), 
    nsmall = 2), 
    NA, 
    NA),
  "p value" = c(str_remove(
    format(
      round(pay_perf_cor$p, 2), 
      nsmall = 2),
    "^0"), 
    NA, 
    NA)
)

write_csv(h2_anova_tbl, "../out/H2.csv")

paste0(
  "The ANOVA test indicated that there was ",
  ifelse(pay_depart_anova$p > 0.05, "not ", ""),
  "a statistically significant difference in monthly income among different departments (",
  "F(", 
  pay_depart_anova$DFn, 
  ", ",
  pay_depart_anova$DFd,
  ") = ",
  format(round(pay_depart_anova$F, 2), nsmall = 2),
  ", p = ",
  str_remove(
    format(
      round(pay_depart_anova$p, 2), 
      nsmall = 2),
    "^0"),
  ")."
)






