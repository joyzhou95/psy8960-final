# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(doParallel)
library(caret)
library(tictoc)


# Data Import and Cleaning 
ion_tbl <- read_csv("../data/ion_final_tbl.csv")

ion_reviews <- ion_tbl %>%
  drop_na(satisfaction_txt,dissatisfaction_txt) %>%
  select(satisfaction_txt, dissatisfaction_txt, employee_id)

## NPL cleaning satis
ion_corpus_original_satis <- VCorpus(VectorSource(ion_reviews$satisfaction_txt))

ion_corpus_satis <- ion_corpus_original_satis %>%
  tm_map(content_transformer(replace_abbreviation)) %>%
  tm_map(content_transformer(replace_contraction)) %>%
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(removeWords, c("good", "great", "amazing", "nice", "incredible", "awsome",
                        "excellent", "best")) %>%
  tm_map(removeWords, "[^[:alnum: ]]") %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(lemmatize_words))

compare_them <- function(corpus_1, corpus_2){
  sample_num <- sample(1:length(corpus_1), 1)
  compare <- list(corpus_1[[sample_num]]$content, corpus_2[[sample_num]]$content)
  compare
}

compare_them(ion_corpus_original_satis, ion_corpus_satis)

bi_token <- function(x){
  NGramTokenizer(x, Weka_control(min = 1, max = 2))
}

ion_dtm_satis <- DocumentTermMatrix(
  ion_corpus_satis, 
  control = list(tokenizer = bi_token)
)

ion_matrix_satis <- as.matrix(ion_dtm_satis)

ion_slim_dtm_satis <- removeSparseTerms(ion_dtm_satis, .90)

ion_slim_matrix_satis <- as_tibble(as.matrix(ion_slim_dtm_satis))

# Check variable variance
ion_slim_matrix_satis %>%
  summarise_if(is.numeric, var)

colnames(ion_slim_matrix_satis) <- paste0(colnames(ion_slim_matrix_satis), "-satis")

ion_reviews_satis <- bind_cols(ion_reviews, ion_slim_matrix_satis)

## NPL cleaning dissatis
ion_corpus_original_dis <- VCorpus(VectorSource(ion_reviews$dissatisfaction_txt))

ion_corpus_dis <- ion_corpus_original_dis %>%
  tm_map(content_transformer(replace_abbreviation)) %>%
  tm_map(content_transformer(replace_contraction)) %>%
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(removeWords, c("hard", "bad", "terrible", "difficult", "many", "much", "sometimes", "can", 
                        "get", "will", "like","google", "large", "long", "big", "lot", "cons", 
                        "really", "things")) %>%
  tm_map(removeWords, "[^[:alnum: ]]") %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(lemmatize_words))


compare_them(ion_corpus_original_dis, ion_corpus_dis)

ion_dtm_dis <- DocumentTermMatrix(
  ion_corpus_dis, 
  control = list(tokenizer = bi_token)
)

ion_matrix_dis <- as.matrix(ion_dtm_dis)

ion_slim_dtm_dis <- removeSparseTerms(ion_dtm_dis, .96)

ion_slim_matrix_dis <- as_tibble(as.matrix(ion_slim_dtm_dis))

# Check variable variance
ion_slim_matrix_dis %>%
  summarise_if(is.numeric, var)

colnames(ion_slim_matrix_dis) <- paste0(colnames(ion_slim_matrix_dis), "-dis")

ion_reviews_nlp <- bind_cols(ion_reviews_satis, ion_slim_matrix_dis)


# Append the text variables to the original dataset
ion_tbl_com <- ion_tbl %>%
  left_join(ion_reviews_nlp, by = "employee_id") %>%
  select(-satisfaction_txt.x, -dissatisfaction_txt.x, 
         -satisfaction_txt.y, -dissatisfaction_txt.y,
         -employee_id)


ion_tbl_recode <- ion_tbl_com %>%
  mutate(
    Attrition = recode(Attrition, "Yes" = 0, "No" = 1),
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
    OverTime = recode(OverTime, "Yes" = 0, "No" = 1))

variance <- ion_tbl_recode %>%
  summarise_if(is.numeric, var)

ion_tbl_final <- ion_tbl_recode %>%
  select(-EmployeeCount, -Over18, -StandardHours)

## Non-text dataset
ion_tbl_num <- ion_tbl_final %>%
  select(!ends_with(c("satis", "dis")))


######### Predictive Modeling 

train_cases <- sample(1:nrow(ion_tbl_final), .75*nrow(ion_tbl_final))

ion_train_tbl <- ion_tbl_final[train_cases, ]
ion_test_tbl <- ion_tbl_final[-train_cases, ]

training_folds <- createFolds(ion_train_tbl$Attrition,
                              k=10)

local_cluster <- makeCluster(7)
registerDoParallel(local_cluster)

tic()
model1n <- train(
  Attrition ~ .,
  ion_train_tbl, 
  method="lm",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model1n <- toc()
model1n

hocv_cor_1n <- cor(
  predict(model1n, ion_test_tbl, na.action=na.pass),
  ion_test_tbl$Attrition
)^2

tic()
model2n <- train(
  Attrition ~ .,
  ion_train_tbl, 
  method="glmnet",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model2n <- toc()
model2n

hocv_cor_2n <- cor(
  predict(model2n, ion_test_tbl, na.action=na.pass),
  ion_test_tbl$Attrition
) ^ 2

tic()
model3n <- train(
  Attrition ~ .,
  ion_train_tbl, 
  method="ranger",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model3n <- toc()
model3n

hocv_cor_3n <- cor(
  predict(model3n, ion_test_tbl, na.action=na.pass),
  ion_test_tbl$Attrition
) ^ 2

tic()
model4n <- train(
  Attrition ~ .,
  ion_train_tbl, 
  method="xgbTree",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model4n <- toc()
model4n

hocv_cor_4n <- cor(
  predict(model4n, ion_test_tbl, na.action=na.pass),
  ion_test_tbl$Attrition
) ^ 2



summary(resamples(list(model1n, model2n, model3n, model4n)))
resample_sum <- summary(resamples(list(model1n, model2n, model3n, model4n)))
dotplot(resamples(list(model1n, model2n, model3n, model4n)))

###Comparing with/without text data

train_cases_num <- sample(1:nrow(ion_tbl_num), .75*nrow(ion_tbl_num))

ion_num_train_tbl <- ion_tbl_num[train_cases_num, ]
ion_num_test_tbl <- ion_tbl_num[-train_cases_num, ]

training_folds <- createFolds(ion_num_train_tbl$Attrition,
                              k=10)

tic()
model3_num <- train(
  Attrition ~ .,
  ion_num_train_tbl, 
  method="ranger",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model3_num <- toc()
model3_num

hocv_cor_3num <- cor(
  predict(model3_num, ion_num_test_tbl, na.action=na.pass),
  ion_num_test_tbl$Attrition
) ^ 2

stopCluster(local_cluster)
registerDoSEQ()

summary(resamples(list(model3n, model3_num)))
resample_sum_text <- summary(resamples(list(model3n, model3_num)))
dotplot(resamples(list(model3n, model3_num)))

# Publication 
model_comp_tbl <- tibble(
  Models = c("lmn","glmnetn","rangern","xgbTreen"),
  cv_rsq = str_remove(round(
    resample_sum$statistics$Rsquared[,"Mean"],2
  ),"^0"),
  ho_rsq = str_remove(c(
    format(round(hocv_cor_1n,2),nsmall=2),
    format(round(hocv_cor_2n,2),nsmall=2),
    format(round(hocv_cor_3n,2),nsmall=2),
    format(round(hocv_cor_4n,2),nsmall=2)
  ),"^0"),
  RMSE = round(
    resample_sum$statistics$RMSE[, "Mean"], 2
    ),
  "Computation Time" = c(
    round(toc_model1n$toc - toc_model1n$tic, 2), 
    round(toc_model2n$toc - toc_model2n$tic, 2),
    round(toc_model3n$toc - toc_model3n$tic, 2),
    round(toc_model4n$toc - toc_model4n$tic, 2)
) )


## Incremental validity of text variables
text_comp_tbl <- tibble(
  Models = c("rangern with text data","rangern without text data"),
  cv_rsq = str_remove(
    round(resample_sum_text$statistics$Rsquared[,"Mean"], 2), 
    "^0"),
  ho_rsq = str_remove(c(
    format(round(hocv_cor_3n, 2), nsmall = 2),
    format(round(hocv_cor_3num, 2), nsmall = 2)),
  "^0"))








