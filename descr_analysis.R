library(tidyverse)
library(ggpubr)

blogs <- readLines("data/en_US/en_US.blogs.txt", skipNul=TRUE)
news <- readLines("data/en_US/en_US.news.txt", skipNul=TRUE)
twitter <- readLines("data/en_US/en_US.twitter.txt", skipNul=TRUE)

#DESCRIPTIVE STATISTICS
#number of lines
blogs_len <- length(blogs)
news_len <- length(news)
twitter_len <- length(twitter)

#character count
blogs_nc <- sum(str_length(blogs))
news_nc <- sum(str_length(news))
twitter_nc <- sum(str_length(twitter))

#word count
blogs_nw <- sum(str_count(blogs, "\\w+"))
news_nw <- sum(str_count(news, "\\w+"))
twitter_nw <- sum(str_count(twitter, "\\w+"))

#avg chars per line
blogs_ac <- blogs_nc / blogs_len
news_ac <- news_nc / news_len
twitter_ac <- twitter_nc / twitter_len

#avg words per line
blogs_aw <- blogs_nw / blogs_len
news_aw <- news_nw / news_len
twitter_aw <- twitter_nw / twitter_len

#avg length of word
blogs_wl <- blogs_nc / blogs_nw
news_wl <- news_nc / news_nw
twitter_wl <- twitter_nc / twitter_nw

#summary
summary.table <- data.frame(file.name = c("blogs", "news", "twitter"),
           num.lines = c(blogs_len, news_len ,twitter_len),
           num.char = c(blogs_nc, news_nc, twitter_nc),
           num.words = c(blogs_nw, news_nw, twitter_nw),
           avg.char = c(blogs_ac, news_ac, twitter_ac),
           avg.words = c(blogs_aw, news_aw, twitter_aw),
           avg.wordlength = c(blogs_wl, news_wl, twitter_wl))

#character histograms
blogs.char.hist <- ggplot(data.frame(characters=str_length(blogs)), aes(x=characters)) + 
  geom_histogram(bins=50)

twitter.char.hist <- ggplot(data.frame(characters=str_length(twitter)), aes(x=characters)) + 
  geom_histogram(bins=50)

news.char.hist <- ggplot(data.frame(characters=str_length(news)), aes(x=characters)) + 
  geom_histogram(bins=50)

all.char.hist <- ggarrange(blogs.char.hist, twitter.char.hist, news.char.hist,
          labels = c("blogs", "tweets", "news"),
          ncol = 3, nrow = 1, hjust = -3)

#word histograms
blogs.word.hist <- ggplot(data.frame(words=str_count(blogs, "\\w+")), aes(x=words)) + 
  geom_histogram(bins=50)

twitter.word.hist <- ggplot(data.frame(words=str_count(twitter, "\\w+")), aes(x=words)) + 
  geom_histogram(bins=50)

news.word.hist <- ggplot(data.frame(words=str_count(news, "\\w+")), aes(x=words)) + 
  geom_histogram(bins=50)

all.word.hist <- ggarrange(blogs.word.hist, twitter.word.hist, news.word.hist,
                           labels = c("blogs", "tweets", "news"),
                           ncol = 3, nrow = 1, hjust = -3)

#make one smaller subsampled dataset
data <- c(sample(news, news_len*0.05), sample(blogs, blogs_len*0.05), sample(twitter, twitter_len*0.05))

#tokenize dataset
tokens <- data %>%
  str_to_lower() %>% 
  corpus() %>% 
  tokens(remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE)

#make bi- and trigram vectors
bigrams <- tokens_ngrams(tokens, n=2, concatenator = " ")
trigrams <- tokens_ngrams(tokens, n=3, concatenator = " ")

#make frequency tables and sort them accordingly
unigrams.freq <- textstat_frequency(dfm(tokens))
bigrams.freq <- textstat_frequency(dfm(bigrams))
trigrams.freq <- textstat_frequency(dfm(trigrams))

unigrams.freq$feature <- with(unigrams.freq, reorder(feature, -frequency))
bigrams.freq$feature <- with(bigrams.freq, reorder(feature, -frequency))
trigrams.freq$feature <- with(trigrams.freq, reorder(feature, -frequency))

#plot 50 most common ngrams
uni.freq.plot <- ggplot(unigrams.freq[1:50], aes(x = feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Unigrams")

bi.freq.plot <- ggplot(bigrams.freq[1:50], aes(x = feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Bigrams")

tri.freq.plot <- ggplot(trigrams.freq[1:50], aes(x = feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Trigrams")