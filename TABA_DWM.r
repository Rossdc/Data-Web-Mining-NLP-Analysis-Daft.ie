library(dplyr)
library(tidyverse)
library(ggfortify)
library(tidyr)
library(ggplot2)
library(GGally)
library(hrbrthemes)
library(broom)
library(reshape2)
library(ggpubr)
library(zoo)
library(tm)

df <- read.csv("daft_datav03.csv", header = T, sep = ",")

#Pre-processing
df[!complete.cases(df),]
numericonly <- sapply(df, is.numeric)
df[numericonly] <- lapply(df[numericonly], na.aggregate)

#Tidy Analysis
df_text <- tibble(line = 1:245, text = df$description)

library(tidytext)

#Tidying data - removing stop words
df_text <- df_text %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Graph of most popular words
df_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(.,aes(n, word)) + coord_flip() +
  geom_col() +
  labs(y = NULL, x = "Count") + theme_bw()

#Merging df with sentiments
bing_word_counts <- df_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#Graphing positive vs negative words - need to remove sink, rail, alarm, complex and floored
#Seperate
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL) + theme_bw()

#Combined
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 15) %>% 
  ungroup() %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col() +
  labs(x = "Contribution to sentiment",
       y = NULL) + theme_bw()

#Bigram to provide context

#Creating bigram
df_bigrams  <- tibble(line = 1:245, text = df$description)
df_bigrams <- df_bigrams %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#counting
df_bigrams %>%
  count(bigram, sort = TRUE)

#Getting rid of stop words
bigrams_separated <- df_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#Filtering
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

#Re-tidying if needed for future use
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

#Analysis via Filtering
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

#Manual Negative Words - Filtering

negative_words <- c("poor", "rust", "required", "planning", "pyrite", "refurbishment")

bigrams_separated %>%
  filter(word1 %in% negative_words) %>%
  count(word1, word2, sort = TRUE)

# filtering and graphing for only  common combinations --- Markov chain
library(igraph)
library(ggraph)

#Creating Arrow
arrowcounts <- grid::arrow(type = "closed", length = unit(.15, "inches"))

bigram_counts %>%
  filter(n > 25) %>%
  graph_from_data_frame() %>% ggraph(.,, layout = "fr") +
  geom_edge_link(show.legend = FALSE,
                 arrow = arrowcounts, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#Corpus - Need to include category variables - which ones to include?
corpus_df <- Corpus(VectorSource(df_text$word))

#Wordmap
library(wordcloud)

#Document Term Matrix
wordc1 <- TermDocumentMatrix(corpus_df)
a <- as.matrix(wordc1)
b <- sort(rowSums(a),decreasing=TRUE)
c <- data.frame(word = names(b),freq=b)
wordcloud(words = c$word, freq = c$freq[(c$freq > 40)], min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#House Type
df_House_Type <- tibble(House_Type = df$house_type, text = df$description)

House_Type_Words <- df_House_Type %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(House_Type, word, sort = TRUE)

total_words <- df_House_Type %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  group_by(House_Type) %>% 
  count(word, sort = TRUE) %>%
  summarize(total = sum(n))

#Tidied
df_House_Type <- left_join(House_Type_Words, total_words)

#Removing property rows
df_House_Type <- df_House_Type[- grep("property", df_House_Type$word),]

#Most popular Words by Property Type
df_House_Type %>%
  group_by(House_Type) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = House_Type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~House_Type, scales = "free_y") +
  labs(x = "Most Common Words",
       y = NULL) + theme_bw()

#Inverse Documentary Frequency

house_tf_idf <- df_House_Type %>%
  bind_tf_idf(word, House_Type, n)

house_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#These words are, as measured by tf-idf, 
#the most important to each listing by house type and most readers would likely agree. 
#What measuring tf-idf has done here is show us that realtors used similar language across their listings, 
#and what distinguishes one house type listing from the rest within the collection of listings 
#are the the names of places and house types
house_tf_idf %>%
  group_by(House_Type) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = House_Type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~House_Type, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#Correlation between Variables

correalation_matrix <- data.frame(round(cor(df[sapply(df, is.numeric)], use="complete.obs", method = "kendall"), digits = 3))

#Correlation Plot
corrplot::corrplot(cor(correalation_matrix, method = "kendall"), method = "number", type = "upper", tl.col="black", tl.cex=0.8, tl.srt=70, bg = "lightgrey")

#Manual Line of Best Fit
lineofbestfit <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

#Correlogram
p1 <- ggpairs(correalation_matrix, lower = list(continuous = wrap(lineofbestfit, method = "lm")),
              diag = list(continuous = wrap("barDiag", colour = "blue", bins = 3)),
              upper = list(continuous = wrap("cor", size = 5)), title = "Correlogram")
p1

#LDA
#Using House Type as categorical variable

df_House_Type2 <- df_House_Type %>%
  rename(text = "word") %>%
        cast_dtm(House_Type, text, n)

lda_housetype <- LDA(df_House_Type2, k = 8, control = list(seed = 1234))
prob <- tidy(lda_housetype, matrix = "beta")

#top 5 terms within each topic.
top_terms <- prob %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() 

top_terms %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


#NAIVE BAYES - House Types
df_House_Type3 <- tibble(House_Type = df$house_type, text = df$description)

#Removing certain housetypes
df_House_Type3 <- df_House_Type3[!df_House_Type3$House_Type == "Duplex", ]
df_House_Type3 <- df_House_Type3[!df_House_Type3$House_Type == "Townhouse", ]
df_House_Type3 <- df_House_Type3[!df_House_Type3$House_Type == "Bungalow", ]

#Randomising data
df_House_Type3$House_Type <- sample(df_House_Type3$House_Type)
df_House_Type3$description <- sample(df_House_Type3$text)

#Getting quantities of each house type
table(df_House_Type3$House_Type)

#Making house type a factor
df_House_Type3$House_Type <- as.factor(df_House_Type3$House_Type)

#Creating corpus
ht_corpus <- VCorpus(VectorSource(df_House_Type3$text))

#Cleaning corpus
ht_corpus_clean <- ht_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords()) %>%
  tm_map(removePunctuation) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)

#Creating document term matrix
ht_dtm <- DocumentTermMatrix(ht_corpus_clean)

.70 * 228

#Creating train and test data frames
ht_dtm_train <- ht_dtm[1:160, ]
ht_dtm_test <- ht_dtm[161:228, ]

#Assigning house type
ht_train_labels <- df_House_Type3[1:160, ]$House_Type
ht_test_labels <- df_House_Type3[161:228, ]$House_Type

#Checking distribution of house types
ht_train_labels %>%
  table %>%
  prop.table

ht_test_labels %>%
  table %>%
  prop.table

#Selecting only top 5 frequent terms
ht_dtm_train <- ht_dtm_train %>%
  findFreqTerms(5) %>%
  ht_dtm_train[ , .]

ht_dtm_test <- ht_dtm_test %>%
  findFreqTerms(5) %>%
  ht_dtm_test[ , .]

#Getting counts, and converting to yes/no
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

ht_dtm_train <- ht_dtm_train %>%
  apply(MARGIN = 2, convert_counts)
ht_dtm_test <- ht_dtm_test %>%
  apply(MARGIN = 2, convert_counts)

#Creating classifier
ht_classifier <- naiveBayes(ht_dtm_train, ht_train_labels)

#Predicting output
ht_pred <- predict(ht_classifier, ht_dtm_test)

#Creating Model and getting output
ct <- CrossTable(ht_pred, ht_test_labels, prop.chisq = FALSE, chisq = FALSE, 
           prop.t = FALSE,
           dnn = c("Predicted", "Actual"))

ct$t

#Trying to improve accuracy of model
ht_classifier2 <- naiveBayes(ht_dtm_train, ht_train_labels, laplace = 1)
ht_pred2 <- predict(ht_classifier2, ht_dtm_test)
CrossTable(ht_pred2, ht_test_labels, prop.chisq = FALSE, chisq = FALSE, 
           prop.t = FALSE,
           dnn = c("Predicted", "Actual"))