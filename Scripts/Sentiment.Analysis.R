# Libraries and datasets ####

library(tidyverse)
library(splitstackshape) 
library(tm)
library(ggwordcloud)
library(reticulate)
library(tensorflow)
library(keras)
library(tidymodels)
library(tidytext)
library(textrecipes)

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



# Tokenise and filter the max number of words for use in analysis
#Set max words and max length
max_words <- 2e4
max_length <- 30

tweets.vaccine.rec <- recipe(~text, data = training.data) %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens=100) %>%
  step_sequence_onehot(text, sequence_length = 100)

tweets.vaccine.rec <- prep(tweets.vaccine.rec)
training.v2 <- bake(tweets.vaccine.rec, new_data = NULL, composition = "matrix")
class(training.v2)


# LSTM Model
lstm.model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words+1, output_dim = 32) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")
lstm.model


