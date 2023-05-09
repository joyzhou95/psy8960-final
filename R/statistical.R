# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)


# Data Import and Cleaning 

## Read in the dataset saved in Part 1
ion_tbl <- read_csv("../data/ion_final_tbl.csv")




# Analysis 

## Test of H1

### Calculate the correlation between monthly income and performance ratings 
pay_perf_cor <- ion_tbl %>%
  cor_test(vars = c("MonthlyIncome", "PerformanceRating"))
pay_perf_cor



## Test of H2

### Conduct ANOVA analyses to test whether monthly income varies by departments,
### set the detailed argument to True to get the statistics we need for constructing an ANOVA table
pay_depart_anova <- ion_tbl %>%
  anova_test(
    formula = MonthlyIncome ~ Department,
    detailed =T
  )
pay_depart_anova



## Test of H3

### Conduct regression analyses predicting tenure using relationship satisfaction, gender,
### and the interaction between the two
h3_reg <- lm(YearsAtCompany ~  RelationshipSatisfaction + Gender + RelationshipSatisfaction * Gender,
             data = ion_tbl) 

### Save the model output summary
h3_reg_out <- summary(h3_reg)

### Obtain the model predicted tenure values 
tenure_pred <- predict(h3_reg)

### Add the model predicted tenure to the original dataset to prepare for plotting 
ion_tbl_pred <- ion_tbl %>%
  add_column(tenure_pred = tenure_pred)





# Visualization 
  
## Visualization of H1 

### Create a scatterplot with a best fitted line visualizing the correlation 
### between monthly income and performance ratings, then save it in the figs folder
(ggplot(ion_tbl, aes(MonthlyIncome, PerformanceRating)) +
    geom_point(position = "jitter") + 
    geom_smooth(method = "lm", se = F) + 
    labs(x = "Monthly Income", y = "Performance Ratings", 
         title = "The Relationship Between Monthly Pay and Performance Ratings") + 
    theme(plot.title = element_text(hjust = 0.5))) %>%
    ggsave(filename = "../figs/H1.png", units = "px", width = 1920, height = 1080)



## Visualization of H2

### Create a boxplot visualizing monthly pay by departments and save it in the figs folder
(ggplot(ion_tbl, aes(MonthlyIncome, Department)) + 
  geom_boxplot() + 
  ### Flipped the coordinates so that Department is on the x-axis, which makes the plot easier to interpret
  coord_flip() + 
  labs(x = "Monthly Income", y = "Departments", 
       title = "Monthly Pay by Departments") + 
  theme(plot.title = element_text(hjust = 0.5))) %>%
  ggsave(filename = "../figs/H2.png", units = "px", width = 1920, height = 1080)



## Visualization of H3

### Create the scatterplot and the best-fitting line between observed and predicted values of tenure
### then save it in the figs folder
(ggplot(ion_tbl_pred, aes(x = RelationshipSatisfaction, y = tenure_pred, group = Gender,
                          color = Gender)) + 
  geom_point(position = "jitter") + 
  geom_smooth(method = "lm", se = F) + 
  labs(x = "Relationship Satisfaction", y = "Predicted Tenure", 
       title = "The Relationship Between Relationship Satisfaction and 
       Tenure by Gender") + 
  theme(plot.title = element_text(hjust = 0.5))) %>%
  ggsave(filename = "../figs/H3.png", units = "px", width = 1920, height = 1080)




# Publication

## Publication Results for H1 

### Interpret the correlation results and format the dynamically generated numbers 
paste0(
  "The pearson correlation between monthly income and performance ratings is r = ", 
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
  ". This test is ",
  ifelse(pay_perf_cor$p > 0.05, "not", ""),
  " statistically significant.",
  "Therefore, hypothesis 1 is ",
  ifelse(pay_perf_cor$p > 0.05, "not ", ""),
  "supported."
)



## Publication results of H2

### Generate an ANOVA table for H2 and format the numbers 
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
      round(pay_depart_anova$p, 2), 
      nsmall = 2),
    "^0"), 
    NA, 
    NA)
)

### Save the generated ANOVA table in the output folder 
write_csv(h2_anova_tbl, "../out/H2.csv")

### Interpret the ANOVA results and format the dynamically generated numbers
paste0(
  "The ANOVA test indicates that there is ",
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
  "). ",
  "Therefore, hypothesis 2 is ",
  ifelse(pay_depart_anova$p > 0.05, "not ", ""),
  "supported."
)



## Publication results of H3

### Create a regression results table with all the numbers formatted
h3_reg_table <- tibble(
  Variables = c("Intercept", 
                "Relationship Satisfaction", 
                "Gender",
                "Relationship Satisfaction X Gender"),
  Coefficients = format(
    round(
      h3_reg_out$coefficients[,1], 
      2),
    nsmall = 2),
  "t-value" = format(
    round(
      h3_reg_out$coefficients[,3], 
      2), 
    nsmall = 2),
  "p-value" = str_remove(
    format(
      round(h3_reg_out$coefficients[,4], 2), 
      nsmall = 2),
    "^0")
)

### Save the generated table in the output folder
write_csv(h3_reg_table, "../out/H3.csv")

### Interpret the regression results and format the dynamically generated numbers
paste0(
  "The interaction between relationship satisfaction and gender did ",
  ifelse(h3_reg_out$coefficients[,4][4] > 0.05, "not ", ""),
  "significantly predict tenure (b = ",
  format(
    round(
      h3_reg_out$coefficients[,1][4], 
      2),
    nsmall = 2),
  ", p = ",
  str_remove(
    format(
      round(h3_reg_out$coefficients[,4][4], 2), 
      nsmall = 2),
    "^0"),
  "). ",
  "Therefore, hypothesis 3 is ",
  ifelse(h3_reg_out$coefficients[,4][4] > 0.05, "not ", ""),
  "supported."
)





