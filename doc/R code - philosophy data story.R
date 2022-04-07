library(tidyverse)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(wordcloud)
library(tm)
library(topicmodels)
library(RColorBrewer)


d=read.csv("philosophy_data.csv")
plato = subset(d, school == "plato")
aristotle=subset(d, school=="aristotle")

#freq and wordcloud for plato
tidy_plato <- plato %>% 
  unnest_tokens(word, sentence_str) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  filter(n > 300)

# Create a bar plot using word_counts with x = word
ggplot(tidy_plato, aes(x = reorder(word,n), y = n)) +
  geom_col() +
  # Flip the plot coordinates
  coord_flip()

wordcloud(words =tidy_plato$word, freq=tidy_plato$n, max.words =100, 
          colors=brewer.pal(8, "Dark2"))

#freq and wordcloud for aristotle
tidy_aristotle <- aristotle %>% 
  unnest_tokens(word, sentence_str) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  filter(n > 500)

# Create a bar plot using word_counts with x = word
ggplot(tidy_aristotle, aes(x = reorder(word,n), y = n)) +
  geom_col() +
  # Flip the plot coordinates
  coord_flip()

wordcloud(words =tidy_aristotle$word, freq=tidy_aristotle$n, max.words =100, 
          colors=brewer.pal(8, "Dark2"))

##SENTIMENT ANALYSIS PLATO

tidy_plato <- plato %>% 
  unnest_tokens(word, sentence_str) %>%
  anti_join(stop_words) %>%
  count(word)
sentiment_plato= tidy_plato %>%
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment %in% c("negative", "positive", "fear","trust","anger"))

# Count the sentiments in sentiment_plato
sentiment_plato %>% 
  count(sentiment) %>% 
  # Arrange the sentiment counts in descending order
  arrange(desc(n))

sentiment_plato %>%
  group_by(sentiment) %>%
  # Take the top 10 words for each sentiment
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word,n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

##SENTIMENT ANALYSIS ARISTOTLE

tidy_aristotle <- aristotle %>% 
  unnest_tokens(word, sentence_str) %>%
  anti_join(stop_words) %>%
  count(word)
sentiment_aristotle= tidy_aristotle %>%
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment %in% c("negative", "positive", "fear","trust","anger"))

# Count the sentiments in sentiment_aristotle
sentiment_aristotle %>% 
  count(sentiment) %>% 
  # Arrange the sentiment counts in descending order
  arrange(desc(n))

sentiment_aristotle %>%
  group_by(sentiment) %>%
  # Take the top 10 words for each sentiment
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word,n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()







## TEXT MODELLING PLATO
tidy_plato <- plato %>% 
  unnest_tokens(word, sentence_str) %>%
  anti_join(stop_words) 

dtm_plato = tidy_plato %>%
  count(word, id) %>% 
  # Cast the word counts by tweet into a DTM
  cast_dtm(id, word, n)

lda_out <- LDA(
  dtm_plato,
  k = 3,
  method = "Gibbs",
  control = list(seed = 42))

glimpse(lda_out)

# Tidy the matrix of word probabilities
lda_topics_plato <- lda_out %>% 
  tidy(matrix = "beta")

# Arrange the topics by word probabilities in descending order
lda_topics_plato %>% 
  arrange(desc(beta))  

word_probs_plato <- lda_topics_plato %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

# Plot word probs, color and facet based on topic
ggplot(
  word_probs_plato, 
  aes(term2, beta, fill = as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#TEXT MODELLING ARISTOTLE
tidy_aristotle <- aristotle %>% 
  unnest_tokens(word, sentence_str) %>%
  anti_join(stop_words) 

dtm_aristotle = tidy_aristotle %>%
  count(word, id) %>% 
  # Cast the word counts by tweet into a DTM
  cast_dtm(id, word, n)

lda_out_1 <- LDA(
  dtm_aristotle,
  k = 3,
  method = "Gibbs",
  control = list(seed = 42))

glimpse(lda_out_1)

# Tidy the matrix of word probabilities
lda_topics_aristotle <- lda_out_1 %>% 
  tidy(matrix = "beta")

# Arrange the topics by word probabilities in descending order
lda_topics_aristotle %>% 
  arrange(desc(beta))  

word_probs_aristotle <- lda_topics_aristotle %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>%
  mutate(term3 = fct_reorder(term, beta))

# Plot word probs, color and facet based on topic
ggplot(
  word_probs_aristotle, 
  aes(term3, beta, fill = as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
