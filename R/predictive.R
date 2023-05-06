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
         -employee_id) %>%
  mutate(Attrition = as.factor(recode(Attrition, "Yes" = 0, "No" = 1)))

variance <- ion_tbl_com %>%
  summarise_if(is.numeric, var)

ion_tbl_final <- ion_tbl_com %>%
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
model_glm <- train(
  Attrition ~ .,
  ion_train_tbl, 
  method = "glmnet",
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model_glm <- toc()
model_glm

p_glm <- predict(model_glm, ion_test_tbl, na.action=na.pass)

glm_test_acc <- confusionMatrix(p_glm, ion_test_tbl[["Attrition"]])

tic()
model_rf <- train(
  Attrition ~ .,
  ion_train_tbl, 
  method="ranger",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model_rf <- toc()
model_rf

p_rf <- predict(model_rf, ion_test_tbl, na.action=na.pass)

rf_test_acc <- confusionMatrix(p_rf, ion_test_tbl[["Attrition"]])

tic()
model_xgb <- train(
  Attrition ~ .,
  ion_train_tbl, 
  method="xgbTree",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model_xgb <- toc()
model_xgb

p_xgb <- predict(model_xgb, ion_test_tbl, na.action=na.pass)

xgb_test_acc <- confusionMatrix(p_xgb, ion_test_tbl[["Attrition"]])


summary(resamples(list(model_glm, model_rf, model_xgb)))
resample_sum <- summary(resamples(list(model_glm, model_rf, model_xgb)))
dotplot(resamples(list(model_glm, model_rf, model_xgb)), metric = "Accuracy")

###Comparing with/without text data

train_cases_num <- sample(1:nrow(ion_tbl_num), .75*nrow(ion_tbl_num))

ion_num_train_tbl <- ion_tbl_num[train_cases_num, ]
ion_num_test_tbl <- ion_tbl_num[-train_cases_num, ]

training_folds <- createFolds(ion_num_train_tbl$Attrition,
                              k=10)

model_xgb_num <- train(
  Attrition ~ .,
  ion_num_train_tbl, 
  method="xgbTree",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)

model_xgb_num

p_xgb_num <- predict(model_xgb_num, ion_num_test_tbl, na.action=na.pass)

xgb_num_test_acc <- confusionMatrix(p_xgb_num, ion_num_test_tbl[["Attrition"]])

stopCluster(local_cluster)
registerDoSEQ()

summary(resamples(list(model_xgb, model_xgb_num)))
resample_sum_text <- summary(resamples(list(model_xgb, model_xgb_num)))
dotplot(resamples(list(model_xgb, model_xgb_num)), metrix = "Accuracy")

# Publication 
model_comp_tbl <- tibble(
  Models = c("glmnet","ranger","xgbTree"),
  cv_accuracy = str_remove(round(
    resample_sum$statistics$Accuracy[,"Mean"],2
  ),"^0"),
  ho_accuracy = str_remove(c(
    format(round(glm_test_acc$overall[1],2),nsmall=2),
    format(round(rf_test_acc$overall[1],2),nsmall=2),
    format(round(xgb_test_acc$overall[1],2),nsmall=2)
  ),"^0"),
  Specificity = str_remove(c(
    format(round(glm_test_acc$byClass[2],2),nsmall=2),
    format(round(rf_test_acc$byClass[2],2),nsmall=2),
    format(round(xgb_test_acc$byClass[2],2),nsmall=2)
  ),"^0"),
  Sensitivity = str_remove(c(
    format(round(glm_test_acc$byClass[1],2),nsmall=2),
    format(round(rf_test_acc$byClass[1],2),nsmall=2),
    format(round(xgb_test_acc$byClass[1],2),nsmall=2)
  ),"^0"),
  "Computation Time" = c(
    round(toc_model_glm$toc - toc_model_glm$tic, 2), 
    round(toc_model_rf$toc - toc_model_rf$tic, 2),
    round(toc_model_xgb$toc - toc_model_xgb$tic, 2)
) )

write_csv(model_comp_tbl, "../out/Model Comparison Table.csv")

## Incremental validity of text variables
text_comp_tbl <- tibble(
  Models = c("rangern with text data","rangern without text data"),
  cv_accuracy = str_remove(
    round(resample_sum_text$statistics$Accuracy[,"Mean"], 2), 
    "^0"),
  ho_accuracy = str_remove(c(
    format(round(xgb_test_acc$overall[1],2),nsmall=2),
    format(round(xgb_num_test_acc$overall[1],2),nsmall=2)
  ),"^0"),
  Specificity = str_remove(c(
    format(round(xgb_test_acc$byClass[2],2),nsmall=2),
    format(round(xgb_num_test_acc$byClass[2],2),nsmall=2)
  ),"^0"),
  Sensitivity = str_remove(c(
    format(round(xgb_test_acc$byClass[1],2),nsmall=2),
    format(round(xgb_num_test_acc$byClass[1],2),nsmall=2)
  ), "^0"))

write_csv(text_comp_tbl, "../out/Text Data Comparison Table.csv")






