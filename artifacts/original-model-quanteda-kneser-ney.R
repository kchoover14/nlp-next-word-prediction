# DISPLAY ARTIFACT -- NOT TESTED OR VERIFIED TO RUN END-TO-END
# Round 2 version of the Kneser-Ney smoothing model -- stop words retained.
# Round 2 finding: predictions dominated by stop words, low utility in practice.
# Annotations use three categories:
# BUG -- code that was broken and never worked
# REVISED -- code that worked at the time but has been updated
# NOTE -- explanatory context or design decisions

library(tidyverse); library(tidytext)
library(stringr); library(data.table)
library(ggplot2); library(ggraph)
library(quanteda); library(quanteda.textplots)
library(textplot); library(wordcloud)


######################## DATA PREPARATION
# 40% sample from SwiftKey corpus (upgraded from 20% in 25June version)

blogs   <- readLines("data/SwiftKey/en_US/en_US.blogs.txt", skipNul=TRUE, encoding="UTF-8")
news    <- readLines("data/SwiftKey/en_US/en_US.news.txt", skipNul=TRUE, encoding="UTF-8")
twitter <- readLines("data/SwiftKey/en_US/en_US.twitter.txt", skipNul=TRUE, encoding="UTF-8")

blogs   <- tibble(text = blogs)
news    <- tibble(text = news)
twitter <- tibble(text = twitter)

set.seed(2021)
samplesize <- .40

blogs.sample   <- blogs %>% sample_n(., nrow(blogs)*samplesize)
news.sample    <- news %>% sample_n(., nrow(news)*samplesize)
twitter.sample <- twitter %>% sample_n(., nrow(twitter)*samplesize)

text.sample <- bind_rows(
    mutate(blogs.sample, source="blogs"),
    mutate(news.sample, source="news"),
    mutate(twitter.sample, source="twitter"))
text.sample$source <- as.factor(text.sample$source)
text.sample <- tibble(text.sample)

rm(blogs, news, twitter, blogs.sample, news.sample, twitter.sample, samplesize)


######################## CLEAN TEXT DATA

text.clean <- text.sample %>%
    mutate(text=str_replace_all(text, "[^[:alnum:][:space:]']", "")) %>%
    mutate(text=str_replace_all(text, "[[:digit:]]", "")) %>%
    mutate(text=str_replace_all(text, " ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")) %>%
    mutate(text=str_replace_all(text, " www\\.[^ ]*", ""))
text.clean <- rename(text.clean, word = text)
rm(text.sample)


######################## REMOVE BAD WORDS
# NOTE: Round 2 -- stop words retained; only bad words removed

badwords <- read.csv(
    "data/base-list-of-bad-words_csv-file_2021_01_18.csv",
    skip=12, skipNul=TRUE, stringsAsFactors=FALSE, header=FALSE,
    col.names=c("word", "v1", "v2", "v3"),
    na.strings="-------------------------------------------------------")
badwords <- data.frame(badwords)
badwords <- select(badwords, "word")

text.clean <- text.clean %>% anti_join(badwords)

rm(badwords); gc()


######################## BUILD CORPUS AND TOKENS
# NOTE: 25June version included stemming (tokens_wordstem) which was
# deliberately dropped -- stemming broke prediction output since stemmed
# words are not readable English

# BUG: text.corpus was missing entirely in the 25June version causing
# an immediate error on tokens() -- fixed in 6july version
text.corpus <- corpus(text.clean, text_field="word")

text.tokens <- tokens(text.corpus)
text.tokens <- tokens_tolower(text.tokens)
text.tokens <- tokens(text.tokens,
    remove_numbers = TRUE,
    remove_url     = TRUE,
    remove_symbols = TRUE)


######################## CREATE NGRAMS AND DFM

ngram1 <- tokens_ngrams(text.tokens, n=1)
ngram2 <- tokens_ngrams(text.tokens, n=2)
ngram3 <- tokens_ngrams(text.tokens, n=3)
ngram4 <- tokens_ngrams(text.tokens, n=4)

ngram1.dfm <- dfm(ngram1)
ngram2.dfm <- dfm(ngram2)
ngram3.dfm <- dfm(ngram3)
ngram4.dfm <- dfm(ngram4)

uni  <- dfm_trim(ngram1.dfm, min_docfreq=.3)
bi   <- dfm_trim(ngram2.dfm, min_docfreq=.3)
tri  <- dfm_trim(ngram3.dfm, min_docfreq=.3)
quad <- dfm_trim(ngram4.dfm, min_docfreq=.3)

rm(text.clean, text.corpus, text.tokens,
   ngram1, ngram2, ngram3, ngram4,
   ngram1.dfm, ngram2.dfm, ngram3.dfm, ngram4.dfm)
gc()


######################## CREATE WORD COUNT TABLES

sumsU <- colSums(uni)
sumsB <- colSums(bi)
sumsT <- colSums(tri)
sumsQ <- colSums(quad)

uni.words <- data.table(
    word_1 = names(sumsU),
    count  = sumsU)

bi.words <- data.table(
    word_1 = sapply(strsplit(names(sumsB), "_", fixed=TRUE), '[[', 1),
    word_2 = sapply(strsplit(names(sumsB), "_", fixed=TRUE), '[[', 2),
    count  = sumsB)

tri.words <- data.table(
    word_1 = sapply(strsplit(names(sumsT), "_", fixed=TRUE), '[[', 1),
    word_2 = sapply(strsplit(names(sumsT), "_", fixed=TRUE), '[[', 2),
    word_3 = sapply(strsplit(names(sumsT), "_", fixed=TRUE), '[[', 3),
    count  = sumsT)

# BUG: word_4 uses sumsT instead of sumsQ -- copy-paste error
# assigns trigram words to the fourth column of the quadgram table
quad.words <- data.table(
    word_1 = sapply(strsplit(names(sumsQ), "_", fixed=TRUE), '[[', 1),
    word_2 = sapply(strsplit(names(sumsQ), "_", fixed=TRUE), '[[', 2),
    word_3 = sapply(strsplit(names(sumsQ), "_", fixed=TRUE), '[[', 3),
    word_4 = sapply(strsplit(names(sumsT), "_", fixed=TRUE), '[[', 3),  # BUG: should be sumsQ
    count  = sumsQ)

setkey(uni.words, word_1)
setkey(bi.words, word_1, word_2)
setkey(tri.words, word_1, word_2, word_3)
setkey(quad.words, word_1, word_2, word_3, word_4)

saveRDS(uni.words,  "artifacts/model-quanteda-kneser-ney-uni")
saveRDS(bi.words,   "artifacts/model-quanteda-kneser-ney-bi")
saveRDS(tri.words,  "artifacts/model-quanteda-kneser-ney-tri")
saveRDS(quad.words, "artifacts/model-quanteda-kneser-ney-quad")

rm(uni, bi, tri, quad, sumsU, sumsB, sumsT, sumsQ)
gc()


######################## KNESER-NEY SMOOTHING

discount.value <- 0.75

# BUG: uni.words[order(-Prob)] called before Prob column exists
# throws error immediately -- Prob is only assigned several lines later
uni.words <- uni.words[order(-Prob)][1:50]
setkey(uni.words, word_1)

# Bigram
numOfBiGrams <- nrow(bi.words[by=.(word_1, word_2)])

ckn <- bi.words[, .(Prob=((.N)/numOfBiGrams)), by=word_2]
setkey(ckn, word_2)

uni.words[, Prob := ckn[word_1, Prob]]
uni.words <- uni.words[!is.na(uni.words$Prob)]

n1wi <- bi.words[, .(N=.N), by=word_1]
setkey(n1wi, word_1)

bi.words[, Cn1 := uni.words[word_1, count]]

bi.words[, Prob := ((count - discount.value) / Cn1 +
    discount.value / Cn1 * n1wi[word_1, N] * uni.words[word_2, Prob])]

# Trigram
tri.words[, Cn2 := bi.words[.(word_1, word_2), count]]

n1w12 <- tri.words[, .N, by=.(word_1, word_2)]
setkey(n1w12, word_1, word_2)

tri.words[, Prob := (count - discount.value) / Cn2 +
    discount.value / Cn2 * n1w12[.(word_1, word_2), N] *
    bi.words[.(word_1, word_2), Prob]]

# Quadgram
quad.words[, Cn2 := bi.words[.(word_1, word_2), count]]

n1w12 <- quad.words[, .N, by=.(word_1, word_2)]
setkey(n1w12, word_1, word_2)

quad.words[, Prob := (count - discount.value) / Cn2 +
    discount.value / Cn2 * n1w12[.(word_1, word_2), N] *
    tri.words[.(word_1, word_2), Prob]]


######################## PREDICTION FUNCTIONS

triWords <- function(w1, w2, n=5) {
    pwords <- tri.words[.(w1, w2)][order(-Prob)]
    if (any(is.na(pwords))) return(biWords(w2, n))
    if (nrow(pwords) > n) return(pwords[1:n, word_3])
    count  <- nrow(pwords)
    bwords <- biWords(w2, n)[1:(n - count)]
    return(c(pwords[, word_3], bwords))
}

# BUG: bi_words should be bi.words -- typo causes object not found error
biWords <- function(w1, n=5) {
    pwords  <- bi_words[w1][order(-Prob)]  # BUG: bi_words does not exist
    if (any(is.na(pwords))) return(uniWords(n))
    if (nrow(pwords) > n) return(pwords[1:n, word_2])
    count   <- nrow(pwords)
    unWords <- uniWords(n)[1:(n - count)]
    return(c(pwords[, word_2], unWords))
}

# NOTE: missing space in original Rmd chunk label caused R parse error
# {rfunction returns random words from unigrams} should be {r ...}
uniWords <- function(n=5) {
    return(sample(uni.words[, word_1], size=n))
}

getWords <- function(str) {
    require(quanteda)
    tokens  <- tokens(x=char_tolower(str))
    tokens  <- char_wordstem(rev(rev(tokens[[1]])[1:2]), language="english")
    words   <- triWords(tokens[1], tokens[2], 5)
    chain_1 <- paste(tokens[1], tokens[2], words[1], sep=" ")
    print(words[1])
}


######################## TEST

getWords("Shall we go to")
getWords(getWords("Shall we go to"))
getWords(getWords(getWords("Shall we go to")))


######################## ACCURACY ASSESSMENT (added May 2026)

# No formal accuracy test was conducted -- model did not produce working
# output due to the bugs documented above. The core failure was the
# quanteda underscore separator incompatibility: quanteda joins ngram
# tokens with "_" (e.g. "the_cat") but prediction functions expect
# space-separated input, causing lookup failures throughout the backoff
# chain. The bi_words typo and the premature Prob ordering call would
# also have prevented execution even if the separator issue were resolved.
# Accuracy: not tested (original) | [placeholder] (debugged version)


######################## TIDY

# rm(list = ls())  # clear environment
# gc()             # release memory
