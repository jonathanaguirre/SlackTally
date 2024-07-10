# Load required libraries
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidytext)
library(caret)
library(tm)
library(topicmodels)
library(wordcloud)
library(tm)
#################################################
# Read the file as plain text
data <- paste(readLines("questions.txt"), collapse = " ")

# Tokenize the text into words using tidytext
# data_words <- data_frame(text = data) %>%
#  unnest_tokens(word, text)
#Please use as tibble is the new standard: 
data_words <- tibble(text = data) %>% unnest_tokens(word, text)

# View the first few words
head(data_words)
#################################################
# Count the occurrences of each word
word_counts <- data_words %>%
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# View the word counts
head(word_counts)
#################################################
# Perform sentiment analysis
sentiments <- data_words %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(sentiment) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# View the sentiments
print(sentiments)

# Plot the sentiments
ggplot(sentiments, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Analysis",
       x = "Sentiment",
       y = "Count")
#################################################
# Load the data
# Assuming questions are in a data frame called 'data' with a column 'questions'
# Text Preprocessing and Feature Extraction
# Convert the text data into a document-term matrix
dtm <- DocumentTermMatrix(Corpus(VectorSource(data$questions)))

# Topic Modeling
# Fit a Latent Dirichlet Allocation (LDA) model to group questions into topics
lda <- LDA(dtm, k = 5) # k is the number of topics
topics <- topics(lda)

# Sentiment Analysis
# Analyze the sentiment of the questions
sentiments <- get_sentiments("bing")
sentiment_scores <- data %>% unnest_tokens(word, questions) %>% inner_join(sentiments)

# (Optional) Question Classification
# If you have labeled data, you can train a classifier
# model <- train(category ~ ., data = labeled_data, method = "rf")

# (Optional) Automated Responses with GPT-4
# This would involve using an API to access GPT-4 and generate responses
#################################################
# Assuming your text data is in a variable called 'text_data'
corpus <- Corpus(VectorSource(data$questions))
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing=TRUE)
word_table <- data.frame(word=names(word_freqs), freq=word_freqs)

# Create the word cloud
wordcloud(words=word_table$word, freq=word_table$freq, min.freq=1, scale=c(3,0.5), colors=brewer.pal(8, "Dark2"))
#################################################
# Sentiment analysis
sentiments <- data %>%
  unnest_tokens(word, questions) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(sentiment) %>%
  summarize(n = n())

# Create bar plot
ggplot(sentiments, aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Sentiment Analysis of Student Questions", x="Sentiment", y="Number of Words")
#################################################
# Assuming 'topics' contains the topic assignment for each question from earlier LDA analysis
topic_counts <- table(topics)
topic_table <- data.frame(topic=names(topic_counts), count=as.numeric(topic_counts))

# Create bar plot
ggplot(topic_table, aes(x=topic, y=count, fill=topic)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Topic Distribution of Student Questions", x="Topic", y="Number of Questions")
#################################################
# Load required libraries
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidytext)
library(caret)
library(tm)
library(topicmodels)
library(wordcloud)
library(tm)
#################################################
# Read the file as plain text
data <- paste(readLines("questions.txt"), collapse = " ")

# Tokenize the text into words using tidytext
# data_words <- data_frame(text = data) %>%
#  unnest_tokens(word, text)
#Please use as tibble is the new standard: 
data_words <- tibble(text = data) %>% unnest_tokens(word, text)

# View the first few words
head(data_words)
#################################################
# Count the occurrences of each word
word_counts <- data_words %>%
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# View the word counts
head(word_counts)
#################################################
# Perform sentiment analysis
sentiments <- data_words %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(sentiment) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# View the sentiments
print(sentiments)

# Plot the sentiments
ggplot(sentiments, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Analysis",
       x = "Sentiment",
       y = "Count")
#################################################
# Load the data
# Assuming questions are in a data frame called 'data' with a column 'questions'
# Text Preprocessing and Feature Extraction
# Convert the text data into a document-term matrix
dtm <- DocumentTermMatrix(Corpus(VectorSource(data$questions)))

# Topic Modeling
# Fit a Latent Dirichlet Allocation (LDA) model to group questions into topics
lda <- LDA(dtm, k = 5) # k is the number of topics
topics <- topics(lda)

# Sentiment Analysis
# Analyze the sentiment of the questions
sentiments <- get_sentiments("bing")
sentiment_scores <- data %>% unnest_tokens(word, questions) %>% inner_join(sentiments)

# (Optional) Question Classification
# If you have labeled data, you can train a classifier
# model <- train(category ~ ., data = labeled_data, method = "rf")

# (Optional) Automated Responses with GPT-4
# This would involve using an API to access GPT-4 and generate responses
#################################################
# Assuming your text data is in a variable called 'text_data'
corpus <- Corpus(VectorSource(data$questions))
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing=TRUE)
word_table <- data.frame(word=names(word_freqs), freq=word_freqs)

# Create the word cloud
wordcloud(words=word_table$word, freq=word_table$freq, min.freq=1, scale=c(3,0.5), colors=brewer.pal(8, "Dark2"))
#################################################
# Sentiment analysis
sentiments <- data %>%
  unnest_tokens(word, questions) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(sentiment) %>%
  summarize(n = n())

# Create bar plot
ggplot(sentiments, aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Sentiment Analysis of Student Questions", x="Sentiment", y="Number of Words")
#################################################
# Assuming 'topics' contains the topic assignment for each question from earlier LDA analysis
topic_counts <- table(topics)
topic_table <- data.frame(topic=names(topic_counts), count=as.numeric(topic_counts))

# Create bar plot
ggplot(topic_table, aes(x=topic, y=count, fill=topic)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Topic Distribution of Student Questions", x="Topic", y="Number of Questions")
#################################################
