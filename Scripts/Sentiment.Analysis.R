# Libraries and datasets ####

## Machine learning####
library(keras)
library(tensorflow)
library(tidymodels)
## Data manipulation ####
library(tidyverse)
library(splitstackshape) 
## EDA ####
library(tm)
library(ggwordcloud)
## Text mining ####
library(tidytext)
## Turn off scientific displays ####
options(scipen=999) 

Data <- read.csv("Data/Tweets2.csv")


# Data wrangling & EDA ####

# Create a copy
RawData <- Data

# Split the column user_location to Location
Data <- cSplit(Data, 'user_location', sep=",", type.convert=FALSE)

# Create a corpus based in the Location
LocationCorpus <- Corpus(VectorSource(Data$user_location_1))
LocationDTM <-  TermDocumentMatrix(LocationCorpus)

# Create a DTM and count per each word and create a DF
LocationM <- as.matrix(LocationDTM)
count <- sort(rowSums(LocationM),decreasing=TRUE)
DF <- data.frame(word = names(count),freq=count)

# generate wordcloud
set.seed(1234)
# par(mar = rep(0, 4))
# png("wordcloud_packages.png", width=12,height=8, units='in', res=300)
# wordcloud(words = DF$word, freq = DF$freq, min.freq = 1,
#           max.words=20, random.order=FALSE, rot.per=0.15,scale=c(4,.5),
#           colors=brewer.pal(4, "Spectral"))


ggplot(data = DF, 
       aes(label = word, size = freq, col = as.character(freq))) + 
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9)+
  scale_size_area(max_size = 14)+
  scale_color_brewer(palette = "Paired", direction = -1)+
  theme_void()

# Text analysis ####
## Bag of Words ####

set.seed(2345)
vaccine.tweets.split <- Data %>%
  filter(nchar(text)>= 15) %>%
  initial_split()

# Fetch training and testing data
training.data <- training(vaccine.tweets.split)
testing.data <- testing(vaccine.tweets.split)



### Tokenisation ####
TidyData <- Data %>%
  select(text) %>%
  unnest_tokens(output = word, input = text)

# Anti join stop words found in TidyDatabased on the  stop_words lexicon
TidyData %<>%
  anti_join(stop_words, by = "word")



TidyData %<>%
  mutate(word = trimws(gsub("[^\\s]*[0-9][^\\s]*", "", word, perl = T))) %>%
  filter(str_length(word) > 1) 

TidyData %<>%
  mutate(word = word %>% str_remove_all("[^[:alnum:]]") ) %>% # alnum = Alphanumeric characters. 
  filter(str_length(word) > 1)


TidyData %>%
  count(word, sort = TRUE) %>% # Count "word". "sort = TRUE" means that it will sort the words in descending order of number words.
  head(20)

# 