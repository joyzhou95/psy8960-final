# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(doParallel)
library(ldatuning)
library(topicmodels)
library(tidytext)

# Data Import and Cleaning 
ion_tbl <- read_csv("../data/ion_final_tbl.csv")

ion_tbl_recode <- ion_tbl %>%
  mutate(Attrition = recode(Attrition, "Yes" = 0, "No" = 1),
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
         OverTime = recode(OverTime, "Yes" = 0, "No" = 1)
         )

## NPL cleaning satis
ion_reviews <- ion_tbl %>%
  drop_na(satisfaction_txt,dissatisfaction_txt) %>%
  select(satisfaction_txt, dissatisfaction_txt)

ion_corpus_original_satis <- VCorpus(VectorSource(ion_reviews$satisfaction_txt))

ion_corpus_satis <- ion_corpus_original_satis %>%
  tm_map(content_transformer(replace_abbreviation)) %>%
  tm_map(content_transformer(replace_contraction)) %>%
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
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

ion_slim_dtm_satis <- removeSparseTerms(ion_dtm_satis, .997)

tokenCounts <- apply(ion_slim_dtm_satis, 1, sum)
ion_slim_dtm_satis_complete <- ion_slim_dtm_satis[tokenCounts > 0, ]

## LDA satisfaction reviews 
cluster <- makeCluster(7)
registerDoParallel(cluster)

# Run the LDA model with cleaned datamframe to estimate the number of topics in the current dataset  
ion_tuning_satis <- FindTopicsNumber(
  ion_slim_dtm_satis_complete,
  topics = seq(2,10,1),
  metrics = c("Griffiths2004",
              "CaoJuan2009",
              "Arun2010",
              "Deveaud2014"),
  verbose = T
)

# Plot the LDA model output to find the appropriate number of topics
FindTopicsNumber_plot(ion_tuning_satis)

# Stop clustering 
stopCluster(cluster)
registerDoSEQ()


lda_results_satis <- LDA(ion_slim_dtm_satis_complete, 5)

lda_betas_satis <- tidy(lda_results_satis, matrix="beta")

lda_betas_satis %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta) %>%
  View

lda_gammas_satis <- tidy(lda_results_satis, matrix="gamma")

lda_highest_gamma <- lda_gammas_satis %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  slice(1) %>%
  ungroup %>%
  mutate(document = as.numeric(document)) %>%
  arrange(document)







