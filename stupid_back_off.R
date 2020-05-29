library(tidyverse)
library(quanteda)

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

#make lookup tables
trigrams.lookup <- data.frame(str_split_fixed(trigrams.freq$feature, " ", 3))
colnames(trigrams.lookup) <- c("word1", "word2", "target")
trigrams.lookup$word1 <- as.character(trigrams.lookup$word1)
trigrams.lookup$word2 <- as.character(trigrams.lookup$word2)
trigrams.lookup$target <- as.character(trigrams.lookup$target)

bigrams.lookup <- data.frame(str_split_fixed(bigrams.freq$feature, " ", 2))
colnames(bigrams.lookup) <- c("word1", "target")
bigrams.lookup$word1 <- as.character(bigrams.lookup$word1)
bigrams.lookup$target <- as.character(bigrams.lookup$target)

unigrams.lookup <- data.frame(unigrams.freq$feature)
colnames(unigrams.lookup) <- c("target")
unigrams.lookup$target <- as.character(unigrams.lookup$target)

#returns predicted next word of input string
next.word <- function(string){
  no.tri <- function(string){
    bi.temp <- filter(bigrams.lookup, word1 == string)
    
    pred <- if_else(dim(bi.temp)[1]>0,
                    bi.temp$target[1],
                    unigrams.lookup$target[1]
    )
    
    return(pred)
  }
  
  split <- str_split(string, " ")[[1]]
  n <- length(split)
  first <- nth(split, n-1)
  second <- nth(split, n)
  
  tri.temp <- filter(trigrams.lookup, word1 == first & word2 == second)
  
  pred <- if_else(dim(tri.temp)[1]>0,
                  tri.temp$target[1],
                  no.tri(second))
  
  return(pred)
}
