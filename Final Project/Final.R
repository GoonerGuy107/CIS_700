library(dplyr)
library(tidytext)
library(tm)
library(randomForest)
library(gmodels)
library(tokenizers)
library(data.table)
library(stopwords)
library(ggplot2)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(SuperLearner)
library(mlbench)
library(caret)
library(caretEnsemble)
library(e1071)
library(C50)

# Reading in the Data
metaData <- read.csv("/Users/kylepeppe/Documents/Masters/Masters_Spring_20/CIS_700/Project/Project_Docs/metadata.csv", stringsAsFactors=F)

# Removing un-needed columns
trimData <- metaData %>% select(abstract, title) %>%
  mutate(index = as.factor(1:57366))

# Removing un-needed columns but keeping date
trimDataPublishTime <- metaData %>% select(publish_time) %>%
  mutate(index = as.factor(1:57366))

# Removing un-needed columns but keeping journal name
trimDataJournal <- metaData %>% select(journal) %>%
  mutate(index = as.factor(1:57366))

# Keeping abstract and publish_time
trimDataAbstract_PublishTime <- metaData %>% select(abstract, publish_time) %>%
  mutate(index = as.factor(1:57366))

# Now just the abstract column
m1 <- data.table(trimData[rowSums(is.na(trimData)) == 0,]) %>% 
  select(-title)

# Split each word from the abstract into separate rows
m2 <- m1 %>% unnest_tokens(output = word , input = abstract)

# Removing some of the more basic words that aren't needed/relevant
stop_words <- data.table(stopwords(language = "en",source = "smart")) %>% 
  mutate(word = V1) %>% 
  select(-V1)

# Placing individual words from abstract in new data set
m3 <- anti_join(m2, stop_words, by = 'word')

# Removing Instances where the column value is a number
m4 <- m3[-grep('^\\d+$', m3$word),]

# Make all the words in the Column in upper case
m4$word <- toupper(m4$word)

# Returning count for all the words
totalWordCount <- m4 %>%
  count(word, sort = TRUE)

# Returning count for the journals
journalCount <- trimDataJournal %>%
  count(journal, sort = TRUE)

# Moving treatwords into new data set
treatmentWords <- m4 %>% 
  filter(word == "CHLOROQUINE" 
         | word == "HYDROXYCHLOROQUINE" | word == "UMIFENOVIR"
         | word == "THALIDMOIDE" | word == "PLASMA" | word == "THYMOSIN"
         | word == "OSELTAMIVIR" |word == "METHYLPREDNISOLONE"
         | word == "INTERFERON" | word == "BALOXAVIR" | word == "DARUNAVIR"
         | word == "BLOCKING" | word == "TOCILIZUMAB" |word == "INTRAVENOUS"
         | word == "FAVIPIRAVIR" | word == "LOPINAVIR" | word == "NEBULIZATION"
         | word == "MARBOXIL" | word == "COBICISTAT" | word == "ANTIBODY"
         | word == "IMMUNOGLOBULIN" | word == "RITONAVIR" | word == "ZIDOVUDINE")

# Getting Counts for possible treatments
treatmentWordCtrs <- m4 %>% 
  group_by(word) %>% 
  count() %>%
  filter(word == "CHLOROQUINE" | word == "HYDROXYCHLOROQUINE"
         | word == "UMIFENOVIR"
         | word == "THALIDMOIDE" | word == "PLASMA" | word == "THYMOSIN"
         | word == "OSELTAMIVIR" |word == "METHYLPREDNISOLONE"
         | word == "INTERFERON" | word == "BALOXAVIR" | word == "DARUNAVIR"
         | word == "BLOCKING" | word == "TOCILIZUMAB" |word == "INTRAVENOUS"
         | word == "FAVIPIRAVIR" | word == "LOPINAVIR" | word == "NEBULIZATION"
         | word == "MARBOXIL" | word == "COBICISTAT" | word == "ANTIBODY"
         | word == "IMMUNOGLOBULIN" | word == "RITONAVIR" | word == "ZIDOVUDINE")

# Bar graph for the total words that are shown over 12,000 times
m4 %>%
  count(word, sort = TRUE) %>%
  filter(n > 12000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Bar graph for the treatment words
treatmentWords %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Bar graph for the publish dates
trimDataPublishTime %>%
  count(publish_time, sort = TRUE) %>%
  filter(n > 150) %>%
  mutate(publish_time = reorder(publish_time, n)) %>%
  ggplot(aes(publish_time, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Bar graph for the journals
trimDataJournal %>%
  count(journal, sort = TRUE) %>%
  filter(n > 200 & n < 2000) %>%
  mutate(journal = reorder(journal, n)) %>%
  ggplot(aes(journal, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Splitting up the words from Abstract into column for each words
singleWords_Date <- trimDataAbstract_PublishTime %>% 
  unnest_tokens(output = word , input = abstract)

# Clearing out "Stop Words" from the Word Column
clean_singleWordsWDate <- anti_join(singleWords_Date, stop_words, by = 'word')

# Removing Instances where the column value is a number
final_cleanSingleWordsWDate <- clean_singleWordsWDate[-grep('^\\d+$', clean_singleWordsWDate$word),]

# Make all the words in the Column in upper case
final_cleanSingleWordsWDate$word <- toupper(final_cleanSingleWordsWDate$word)

# Moving treatment words into new data set with date
treatmentWordsWDate <- final_cleanSingleWordsWDate %>% 
  filter(word == "CHLOROQUINE" 
         | word == "HYDROXYCHLOROQUINE" | word == "UMIFENOVIR"
         | word == "THALIDMOIDE" | word == "PLASMA" | word == "THYMOSIN"
         | word == "OSELTAMIVIR" |word == "METHYLPREDNISOLONE"
         | word == "INTERFERON" | word == "BALOXAVIR" | word == "DARUNAVIR"
         | word == "BLOCKING" | word == "TOCILIZUMAB" |word == "INTRAVENOUS"
         | word == "FAVIPIRAVIR" | word == "LOPINAVIR" | word == "NEBULIZATION"
         | word == "MARBOXIL" | word == "COBICISTAT" | word == "ANTIBODY"
         | word == "IMMUNOGLOBULIN" | word == "RITONAVIR" | word == "ZIDOVUDINE")

# Clearing out index value
treatmentWordsWDate <- treatmentWordsWDate %>%
  select(-index)

# Slimming down the time range (using PA's shutdown date)
treatmentWordsWDate2 <- treatmentWordsWDate %>% 
  filter(publish_time >= "2020-03-19")

# Bar graph for the treatment words - after PA shutdown
treatmentWordsWDate2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Creating the function to split data set (train to test)
create_train_test <- function(treatmentWordsWDate2, size = 0.8, train = TRUE) {
  n_row = nrow(treatmentWordsWDate2)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (treatmentWordsWDate2[train_sample, ])
  } else {
    return (treatmentWordsWDate2[-train_sample, ])
  }
}

# Putting the train and test data sets into variables
data_train <- create_train_test(treatmentWordsWDate2, 0.8, train = TRUE)
data_test <- create_train_test(treatmentWordsWDate2, 0.8, train = FALSE)

# Fitting then moving out the decision tree
fit <- rpart(word~., data = data_test, method = 'class')
rpart.plot(fit, extra = 106)

# Predicting word
predict_unseen <- predict(fit, data_test, type = 'class')

# Creating the prediction table then displaying the prediction table
table_mat <- table(data_test$word, predict_unseen)
table_mat

# Calculating the accuracy
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

# Setting variables to use for below 4 Ensemble Training
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"

# C5.0 Ensemble - test
set.seed(seed)
fit.c50 <- train(word~., data=data_test, method="C5.0", 
                 metric=metric, trControl=control)

# Stochastic Gradient Boosting - test
set.seed(seed)
fit.gbm <- train(word~., data=data_test, method="gbm", 
                 metric=metric, trControl=control, verbose=FALSE)

# Summarizing results and plotting it - test
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)
splom(boosting_results)

# C5.0 Ensemble - train
set.seed(seed)
fit.c502 <- train(word~., data=data_train, method="C5.0", 
                  metric=metric, trControl=control)

# Stochastic Gradient Boosting - train
set.seed(seed)
fit.gbm2 <- train(word~., data=data_train, method="gbm", 
                  metric=metric, trControl=control, verbose=FALSE)

# Summarizing results and plotting it - train
boosting_results2 <- resamples(list(c5.0=fit.c502, gbm=fit.gbm2))
summary(boosting_results2)
dotplot(boosting_results2)
splom(boosting_results2)

# Bagged CART - test
set.seed(seed)
fit.treebag <- train(word~., data=data_test, method="treebag", 
                     metric=metric, trControl=control)

# Random Forest - test
set.seed(seed)
fit.rf <- train(word~., data=data_test, method="rf", 
                metric=metric, trControl=control)

# Summarizing results and plotting - test
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)
splom(bagging_results)

# Bagged CART - train
set.seed(seed)
fit.treebag2 <- train(word~., data=data_train, method="treebag", 
                     metric=metric, trControl=control)

# Random Forest - train
set.seed(seed)
fit.rf2 <- train(word~., data=data_train, method="rf", 
                metric=metric, trControl=control)

# Summarizing results and plotting - train
bagging_results2 <- resamples(list(treebag=fit.treebag2, rf=fit.rf2))
summary(bagging_results2)
dotplot(bagging_results2)
splom(bagging_results2)

