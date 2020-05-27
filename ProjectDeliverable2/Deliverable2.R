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

# Bar graph for the publish dates
trimDataJournal %>%
  count(journal, sort = TRUE) %>%
  filter(n > 200 & n < 2000) %>%
  mutate(journal = reorder(journal, n)) %>%
  ggplot(aes(journal, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

