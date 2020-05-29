library(data.table)
library(quanteda)
library(dplyr)
library(stringr)

#returns tokens of corpus consisting of subsamples of size q of original datasets
make.tokens <- function(q){
  stopifnot(q>0, q<1)
  corpus <- c(sample(news, length(news)*q), sample(blogs, length(blogs)*q), sample(twitter, length(twitter)*q))
  tokens <- tokens(corpus, remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE)
  
  return(tokens)
}


#returns data.table with ngrams and respective counts
ngram.frequency <- function(n, tokens){
  ngram.stats <- tokens %>% 
    tokens_ngrams(n=n, concatenator = " ") %>% 
    dfm() %>% 
    textstat_frequency()
  ngram.freq <- data.table(ngram=ngram.stats$feature, freq=ngram.stats$frequency)
  
  return(ngram.freq)
}


#returns data.frame of observed trigrams starting with prediction input and respective counts
observed.trigrams <- function(string, trigrams){
  obs.tri <- data.frame(ngrams=vector(mode = 'character', length = 0),
                        freq=vector(mode = 'integer', length = 0))
  rows <- str_which(trigrams$ngram, paste0("^", string))
  
  if(length(rows)>0){
    obs.tri <- trigrams[rows,]
  }
  
  return(obs.tri)
}


#returns data.frame of observed trigrams starting with prediction input and respective probabilities
observed.trigrams.prob <- function(obs.trigrams, bigrams, string, disc.tri=0.5){
  if(nrow(obs.trigrams)<1) return(NULL)
  bigram.count <- filter(bigrams, ngram==string)$freq
  obs.trigrams.probs <- mutate(obs.trigrams, freq=((freq - disc.tri) / bigram.count))
  colnames(obs.trigrams.probs) <- c("ngram", "prob")
  
  return(obs.trigrams.probs)
}


#returns a vector with the endings of unobserved trigrams
unobserved.endings <- function(obs.trigrams, unigrams){
  obs.endings <- str_split_fixed(obs.trigrams$ngram, " ", 3)[,3]
  unobs.endings <- unigrams[!(unigrams$ngram %in% obs.endings), ]$ngram
  
  return(unobs.endings)
}


#returns the total probability mass discounted from all observed bigrams to unobserved bigrams
bigrams.alpha <- function(string, unigrams, bigrams, disc.bi=0.5){
  unigram <- str_split(string, " ")[[1]][2]
  unigram <- unigrams[unigrams$ngram==unigram]
  bigrams.startwith.unigram <- bigrams[str_detect(bigrams$ngram, paste0("^", unigram$ngram))]
  if(nrow(bigrams.startwith.unigram)<1) return(0)
  alpha <- 1 - (sum(bigrams.startwith.unigram$freq - disc.bi)/unigram$freq)
  
  return(alpha)
}


#returns the total probability mass discounted from all observed trigrams to unobserved trigrams
trigrams.alpha <- function(string, bigrams, obs.trigrams, disc.tri=0.5){
  bigram <- bigrams[bigrams$ngram==string]
  if(nrow(obs.trigrams)<1) return(1)
  alpha <- 1 - (sum(obs.trigrams$freq - disc.tri)/bigram$freq)
  
  return(alpha)
}


#returns a vector of backed-off bigrams where the first word of the bigram is the last word of the prediction input
backedoff.bigrams <- function(string, unobs.endings){
  input.ending <- str_split(string, " ")[[1]][2]
  bigrams <- paste(input.ending, unobs.endings)
  
  return(bigrams)
}


#returns a vector of all observed backed-off bigrams
observed.backedoff.bigrams <- function(string, unobs.endings, bigrams){
  bo.bigrams <- backedoff.bigrams(string, unobs.endings)
  obs.bo.bigrams <- bigrams[bigrams$ngram %in% bo.bigrams,]
  
  return(obs.bo.bigrams)
}


#returns a vector of all unobserved backed-off bigrams
unobserved.backedoff.bigrams <- function(string, unobs.endings, obs.bo.bigrams){
  bo.bigrams <- backedoff.bigrams(string, unobs.endings)
  unobs.bo.bigrams <- bo.bigrams[!(bo.bigrams %in% obs.bo.bigrams$ngram)]
  
  return(unobs.bo.bigrams)
}


#returns a data.frame with the probabilities of all observed backed-off bigrams
observed.bigrams.prob <- function(obs.bo.bigrams, unigrams, disc.bi=0.5){
  startwords <- str_split_fixed(obs.bo.bigrams$ngram, " ", 2)[, 1]
  startwords <- unigrams[unigrams$ngram %in% startwords, ]
  obs.bigrams.probs <- (obs.bo.bigrams$freq - disc.bi) / startwords$freq
  obs.bigrams.probs <- data.frame(ngram=obs.bo.bigrams$ngram, prob=obs.bigrams.probs)
  
  return(obs.bigrams.probs)
}


#returns a data.frame with the probabilities of all unobserved backed-off bigrams
unobserved.bigrams.prob <- function(unobs.bo.bigrams, unigrams, alpha.bi){
  unobs.bigrams.endings <- str_split_fixed(unobs.bo.bigrams, " ", 2)[, 2]
  unobs.bigrams.probs <- unigrams[unigrams$ngram %in% unobs.bigrams.endings, ]
  unobs.bigrams.probs <- data.frame(ngram=unobs.bo.bigrams, prob=(alpha.bi * unobs.bigrams.probs$freq / sum(unobs.bigrams.probs$freq)))
  
  return(unobs.bigrams.probs)
}


#returns a data.frame with the probabilities of all unobserved trigrams
unobserved.trigrams.prob <- function(string, obs.bigram.probs, unobs.bigram.probs, alpha.tri){
  bigram.probs <- rbind(obs.bigram.probs, unobs.bigram.probs)
  bigram.probs <- bigram.probs[order(-bigram.probs$prob), ]
  sum.bigram.probs <- sum(bigram.probs$prob)
  startword <- str_split(string, " ")[[1]][1]
  unobs.trigrams <- paste(startword, bigram.probs$ngram, sep=" ")
  unobs.trigrams.probs <- alpha.tri * bigram.probs$prob / sum.bigram.probs
  unobs.trigrams.probs <- data.frame(ngram=unobs.trigrams, prob=unobs.trigrams.probs)
  
  return(unobs.trigrams.probs)
}

#wrapper around all functions to make the ngrams out of subsamples of size q
train.backoff <- function(q=0.05){
  if(!exists("blogs")) blogs <- readLines("data/en_US/en_US.blogs.txt", skipNul=TRUE)
  if(!exists("news")) news <- readLines("data/en_US/en_US.news.txt", skipNul=TRUE)
  if(!exists("twitter")) twitter <- readLines("data/en_US/en_US.twitter.txt", skipNul=TRUE)
  
  tokens <- make.tokens(q)
  
  unigrams <- ngram.frequency(1, tokens)
  bigrams <- ngram.frequency(2, tokens)
  trigrams <- ngram.frequency(3, tokens)
  
  ngrams <- list(unigrams=unigrams, bigrams=bigrams, trigrams=trigrams)
  
  ngrams <<- ngrams 
}

#wrapper around all functions to get three most likely words, taking in string and ngram list produced by train.backoff()
predict.backoff <- function(string, ngrams.list=ngrams, bi.val=0.5, tri.val=0.5){
  disc.bi <- bi.val
  disc.tri <- tri.val
  #predict.this <- paste(str_split(string, " ")[[1]][-1:-(str_count(sentence, "\\w+")-2)], collapse = " ")
  split <- str_split(string, " ")[[1]]
  n <- length(split)
  first <- nth(split, n-1)
  second <- nth(split, n)
  predict.this <- paste(first, second)
  
  unigrams <- ngrams.list$unigrams
  bigrams <- ngrams.list$bigrams
  trigrams <- ngrams.list$trigrams
  
  observedTrigrams <- observed.trigrams(predict.this, trigrams)
  observedTrigramsProbs <- observed.trigrams.prob(observedTrigrams, bigrams, predict.this, disc.tri)
  
  unobservedEndings <- unobserved.endings(observedTrigrams, unigrams)
  
  alphaBi <- bigrams.alpha(predict.this, unigrams, bigrams, disc.bi)
  
  backedoffBigrams <- backedoff.bigrams(predict.this, unobservedEndings)
  observerdBackedoffBigrams <- observed.backedoff.bigrams(predict.this, unobservedEndings, bigrams)
  unobserverdBackedoffBigrams <- unobserved.backedoff.bigrams(predict.this, unobservedEndings, observerdBackedoffBigrams)
  
  observedBigramsProbs <- observed.bigrams.prob(observerdBackedoffBigrams, unigrams, disc.bi)
  unobservedBigramsProbs <- unobserved.bigrams.prob(unobserverdBackedoffBigrams, unigrams, alphaBi)
  bigramsProbs <- rbind(observedBigramsProbs, unobservedBigramsProbs)
  
  alphaTri <- trigrams.alpha(predict.this, bigrams, observedTrigrams, disc.tri)
  
  unobservedTrigramsProbs <- unobserved.trigrams.prob(predict.this, observedBigramsProbs, unobservedBigramsProbs, alphaTri)
  
  TrigramProbs <- rbind(observedTrigramsProbs, unobservedTrigramsProbs)
  TrigramProbs <- TrigramProbs[order(-TrigramProbs$prob),]
  
  pred1 <- str_split(TrigramProbs$ngram[1], " ")[[1]][3]
  pred2 <- str_split(TrigramProbs$ngram[2], " ")[[1]][3]
  pred3 <- str_split(TrigramProbs$ngram[3], " ")[[1]][3]
  
  return(c(pred1, pred2, pred3))
}

