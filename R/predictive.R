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

## Read in the dataset created in Part 1
ion_tbl <- read_csv("../data/ion_final_tbl.csv")


## Conduct natural language processing (NLP) analyses to convert reviews into variables that could be used in machine learning models

 
ion_reviews <- ion_tbl %>%
  ## Drop NA values in the review columns
  drop_na(satisfaction_txt,dissatisfaction_txt) %>%
  ## Create a dataset containing only the reviews 
  select(satisfaction_txt, dissatisfaction_txt, employee_id)


## NLP for the satisfactory reviews

## Convert the satisfactory review column into a corpus object 
ion_corpus_original_satis <- VCorpus(VectorSource(ion_reviews$satisfaction_txt))

## Pre-process the review texts following standard steps
ion_corpus_satis <- ion_corpus_original_satis %>%
  tm_map(content_transformer(replace_abbreviation)) %>%
  tm_map(content_transformer(replace_contraction)) %>%
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  ## The final slim matrix output contained some words are of high frequency yet do not provide much useful information
  ## I thus decided to remove from the texts 
  tm_map(removeWords, c("good", "great", "amazing", "nice", "incredible", "awsome",
                        "excellent", "best")) %>%
  tm_map(removeWords, "[^[:alnum: ]]") %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(lemmatize_words))

## Create a function that compares the original corpus and the pre-processed corpus
compare_them <- function(corpus_1, corpus_2){
  sample_num <- sample(1:length(corpus_1), 1)
  compare <- list(corpus_1[[sample_num]]$content, corpus_2[[sample_num]]$content)
  compare
}

## Run the function several times to examine the results of preprocessing 
compare_them(ion_corpus_original_satis, ion_corpus_satis)

## Generate a function for creating bi-gram objects  
bi_token <- function(x){
  NGramTokenizer(x, Weka_control(min = 1, max = 2))
}

## Convert the corpus object into a matrix that contains one- and two-words variables 
ion_dtm_satis <- DocumentTermMatrix(
  ion_corpus_satis, 
  control = list(tokenizer = bi_token)
)

## Check how many word variables are generated 
ion_matrix_satis <- as.matrix(ion_dtm_satis)

## The original dataset contains 1470 observations and I do not want to input too many variables into the 
## machine learning model so I only kept the top 10ish terms 
ion_slim_dtm_satis <- removeSparseTerms(ion_dtm_satis, .90)

## Convert dtm into tibble
ion_slim_matrix_satis <- as_tibble(as.matrix(ion_slim_dtm_satis))

## Check variable variance to make sure the words had some variance across observations
ion_slim_matrix_satis %>%
  summarise_if(is.numeric, var)

## Add a suffix to the words to label them as words from the satisfactory reviews 
colnames(ion_slim_matrix_satis) <- paste0(colnames(ion_slim_matrix_satis), "-satis")

## Append the words into the original review dataset 
ion_reviews_satis <- bind_cols(ion_reviews, ion_slim_matrix_satis)



#### Follow the same steps above to conduct NLP for dissatisfactory reviews 
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

ion_slim_matrix_dis %>%
  summarise_if(is.numeric, var)

colnames(ion_slim_matrix_dis) <- paste0(colnames(ion_slim_matrix_dis), "-dis")

ion_reviews_nlp <- bind_cols(ion_reviews_satis, ion_slim_matrix_dis)



## Append all the text variables to the original master dataset
ion_tbl_com <- ion_tbl %>%
  left_join(ion_reviews_nlp, by = "employee_id") %>%
  ## Remove the review columns and employee id so they are not included in the machine learning model 
  select(-satisfaction_txt.x, -dissatisfaction_txt.x, 
         -satisfaction_txt.y, -dissatisfaction_txt.y,
         -employee_id)

## Check the variance of all variables
variance <- ion_tbl_com %>%
  summarise_if(is.numeric, var)

## Remove variables that had zero variance, as they would have zero predictive validity of the outcome
ion_tbl_final <- ion_tbl_com %>%
  select(-EmployeeCount, -Over18, -StandardHours)

## Create a separate dataset that does not contain any of the text variables for later analyses 
ion_tbl_num <- ion_tbl_final %>%
  select(!ends_with(c("satis", "dis")))






# Analysis 

## Split the dataset into train and test sets following a 75/25 split
train_cases <- sample(1:nrow(ion_tbl_final), .75*nrow(ion_tbl_final))

ion_train_tbl <- ion_tbl_final[train_cases, ]
ion_test_tbl <- ion_tbl_final[-train_cases, ]

## Create the number of folds and holdout samples for models
training_folds <- createFolds(ion_train_tbl$Attrition, k=10)

## Make clusters for parallelization 
local_cluster <- makeCluster(7)
registerDoParallel(local_cluster)


## Build the glmnet model for classification 
tic()
model_glm <- train(
  Attrition ~ .,
  ion_train_tbl, 
  method = "glmnet",
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
## Record computation time 
toc_model_glm <- toc()
model_glm

## Get the predicted values from the model
p_glm <- predict(model_glm, ion_test_tbl, na.action = na.pass)

## Obtain statistics of accuracy, specificity and sensitivity
glm_test_acc <- confusionMatrix(p_glm, as.factor(ion_test_tbl[["Attrition"]]))

## Follow the same steps above for building the other two models 
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

p_rf <- predict(model_rf, ion_test_tbl, na.action = na.pass)

rf_test_acc <- confusionMatrix(p_rf, as.factor(ion_test_tbl[["Attrition"]]))

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

xgb_test_acc <- confusionMatrix(p_xgb, as.factor(ion_test_tbl[["Attrition"]]))

## Compare the performance of the three models 
summary(resamples(list(model_glm, model_rf, model_xgb)))
resample_sum <- summary(resamples(list(model_glm, model_rf, model_xgb)))
dotplot(resamples(list(model_glm, model_rf, model_xgb)), metric = "Accuracy")


## Ended up choosing the elastic net model as it has higher sensitivity, specificity and accuracy in the test set 
## as well as shorter computation time compared to the other two models. Especially given that our goal for this 
## project is to maximize predictive validity in new samples, I decided to go with the elastic net model. 


## Follow the same steps above to build the elastic net model without text data 
train_cases_num <- sample(1:nrow(ion_tbl_num), .75*nrow(ion_tbl_num))

ion_num_train_tbl <- ion_tbl_num[train_cases_num, ]
ion_num_test_tbl <- ion_tbl_num[-train_cases_num, ]

training_folds <- createFolds(ion_num_train_tbl$Attrition, k=10)

model_glm_num <- train(
  Attrition ~ .,
  ion_num_train_tbl, 
  method="glmnet",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)

model_glm_num

p_glm_num <- predict(model_glm_num, ion_num_test_tbl, na.action=na.pass)

glm_num_test_acc <- confusionMatrix(p_glm_num, as.factor(ion_num_test_tbl[["Attrition"]]))

## Stop parallel processing
stopCluster(local_cluster)
registerDoSEQ()

## Compare the performance of the model with and without text data  
summary(resamples(list(model_glm, model_glm_num)))
resample_sum_text <- summary(resamples(list(model_glm, model_glm_num)))
dotplot(resamples(list(model_glm, model_glm_num)), metrix = "Accuracy")

# Publication 
## Create a model comparison table containing accuracy in the train and test sets, specificity, 
## sensitivity, and computation time
model_comp_tbl <- tibble(
  Models = c("glmnet","ranger","xgbTree"),
  cv_accuracy = str_remove(format(
    round(resample_sum$statistics$Accuracy[,"Mean"],2), nsmall = 2), 
  "^0"),
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

## Save the table in the output folder
write_csv(model_comp_tbl, "../out/Model Comparison Table.csv")



### Questions:
## What characteristics of how you created the final model likely made the biggest impact in maximizing its performance? How do you know? 

## I think the choice of model building that had the largest impact on the its performance is the selection of the modeling method.
## Looking at the model comparison table, the elastic net model had the highest accuracy for predicting the test sample (.90) and the lowest accuracy
## for predicting the training sample (.88) compared to the other two models, and it also has the highest specificity (.49) and sensitivity (.97). 
## These results are likely due to elastic net model's balance between the lasso and ridge penalty, which penalizes model complexity, reduces overfitting 
## and are good at handling multicollinearity, which is likely an issue in the current dataset as many of the variables are correlated with each other 
## (e.g., age and job level, monthly rate and monlyth income). Additionally, its penalty for model complexity allows the elastic net model to outperform
## other models in making out-of-sample predictions. 


## Create a table examining the incremental validity of text variables
text_comp_tbl <- tibble(
  Models = c("glmnet with text data","glmnet without text data"),
  cv_accuracy = str_remove(
    format(round(resample_sum_text$statistics$Accuracy[,"Mean"], 2), nsmall = 2), 
    "^0"),
  ho_accuracy = str_remove(c(
    format(round(glm_test_acc$overall[1],2),nsmall=2),
    format(round(glm_num_test_acc$overall[1],2),nsmall=2)
  ),"^0"),
  Specificity = str_remove(c(
    format(round(glm_test_acc$byClass[2],2),nsmall=2),
    format(round(glm_num_test_acc$byClass[2],2),nsmall=2)
  ),"^0"),
  Sensitivity = str_remove(c(
    format(round(glm_test_acc$byClass[1],2),nsmall=2),
    format(round(glm_num_test_acc$byClass[1],2),nsmall=2)
  ), "^0"))

## Save the table in the output folder
write_csv(text_comp_tbl, "../out/Text Data Comparison Table.csv")



### Question: 
## What is the incremental predictive accuracy gained by including text data in your model versus not including text data?

## Including text data in the model increased specificity, sensitivity, and predictive accuracy in the test set, which suggests that the text data 
## probably added some noise in the model so that it performs better in out-of-sample predictions.
## However, the difference between the two models is quite small, and rerunning the models yielded quite different results,
## so it is hard to draw a firm conclusion regarding the incremental predictive validity of text data. 




