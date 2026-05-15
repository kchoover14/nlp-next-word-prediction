# DISPLAY ARTIFACT -- NOT TESTED OR VERIFIED TO RUN END-TO-END
# Round 2 version of the Good-Turing + Katz Backoff model -- stop words retained.
# Round 2 finding: predictions dominated by stop words, low utility in practice.
# Annotations use three categories:
# BUG -- code that was broken and never worked
# REVISED -- code that worked at the time but has been updated
# NOTE -- explanatory context or design decisions

library(tidyverse); library(tidytext)       # REVISED: load individually per convention
library(stringr); library(stringi); library(data.table)
library(ggplot2); library(ggraph)
library(quanteda)


######################## DATA PREPARATION
# NOTE: 26June and 7July used 40% sample; 4a and 4b used 60% sample
# 40% used here as the more conservative and consistent choice with other models

blogs   <- readLines("data/SwiftKey/en_US/en_US.blogs.txt", skipNul=TRUE, encoding="UTF-8")
news    <- readLines("data/SwiftKey/en_US/en_US.news.txt", skipNul=TRUE, encoding="UTF-8")
twitter <- readLines("data/SwiftKey/en_US/en_US.twitter.txt", skipNul=TRUE, encoding="UTF-8")

blogs   <- data_frame(text = blogs)      # REVISED: use tibble()
news    <- data_frame(text = news)
twitter <- data_frame(text = twitter)

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


######################## REMOVE BAD WORDS

badwords <- read.csv(
    "data/base-list-of-bad-words_csv-file_2021_01_18.csv",
    skip=12, skipNul=TRUE, stringsAsFactors=FALSE, header=FALSE,
    col.names=c("word", "v1", "v2", "v3"),
    na.strings="-------------------------------------------------------")
badwords <- data.frame(badwords)
badwords <- select(badwords, "word")

# NOTE: 4a and 4b removed bad words at corpus level rather than text level
# -- functionally equivalent but done here at text level for consistency

# NOTE: 26June version also removed stop words at this stage
# 7July, 4a, 4b removed stop words at token level after corpus creation
# stop word removal at token level retained here as it is more precise


######################## BUILD CORPUS AND TOKENS
# BUG: 26June version referenced text.corpus without creating it first
# causing an immediate error on tokens() -- fixed in 7July version

# NOTE: 26June included stemming (tokens_wordstem) which was deliberately
# dropped in 7July -- stemming broke prediction output since stemmed words
# are not readable English. Not included here.

text.corpus <- corpus(text.sample)

text.tokens <- tokens(text.corpus)
text.tokens <- tokens_tolower(text.tokens)
text.tokens <- tokens(text.tokens,
    remove_numbers = TRUE,
    remove_url     = TRUE,
    remove_symbols = TRUE,
    remove_punct   = TRUE)
text.tokens <- tokens_remove(text.tokens, badwords$word)

rm(badwords, text.sample)


######################## CREATE NGRAMS AND DFM
# NOTE: 4a and 4b only used trigrams, bigrams, unigrams (no quadgram)
# quadgram added here for consistency with other models

ngram1 <- tokens_ngrams(text.tokens, n=1)
ngram2 <- tokens_ngrams(text.tokens, n=2)
ngram3 <- tokens_ngrams(text.tokens, n=3)
ngram4 <- tokens_ngrams(text.tokens, n=4)

ngram1.dfm <- dfm(ngram1)
ngram2.dfm <- dfm(ngram2)
ngram3.dfm <- dfm(ngram3)
ngram4.dfm <- dfm(ngram4)

UniG <- dfm_trim(ngram1.dfm, min_docfreq=.3)
BiG  <- dfm_trim(ngram2.dfm, min_docfreq=.3)
TriG <- dfm_trim(ngram3.dfm, min_docfreq=.3)
QuadG <- dfm_trim(ngram4.dfm, min_docfreq=.3)

rm(text.corpus, text.tokens,
   ngram1, ngram2, ngram3, ngram4,
   ngram1.dfm, ngram2.dfm, ngram3.dfm, ngram4.dfm)
gc()


######################## COUNT NGRAM FREQUENCIES

CountNGramFreq <- function(NGrDfm) {
    FreqV <- colSums(NGrDfm)
    return(data.table(term=names(FreqV), c=FreqV))
}

UniFreq  <- CountNGramFreq(UniG)
BiFreq   <- CountNGramFreq(BiG)
TriFreq  <- CountNGramFreq(TriG)
QuadFreq <- CountNGramFreq(QuadG)

min_count = 4
UniFreq  <- UniFreq[c > min_count,]
BiFreq   <- BiFreq[c > min_count,]
TriFreq  <- TriFreq[c > min_count,]
QuadFreq <- QuadFreq[c > min_count,]

rm(UniG, BiG, TriG, QuadG)
gc()


######################## GOOD-TURING SMOOTHING

# Calculate frequency of frequencies (N_r)
CountNC <- function(FreqVec) {
    CountTbl <- table(FreqVec[,.(c)])
    return(data.table(cbind(c=as.integer(names(CountTbl)), Nr=as.integer(CountTbl))))
}

UniBins  <- CountNC(UniFreq)
BiBins   <- CountNC(BiFreq)
TriBins  <- CountNC(TriFreq)
QuadBins <- CountNC(QuadFreq)

# Average non-zero count -- replace N_r with Z_r
avg.zr <- function(Bins) {
    max <- dim(Bins)[1]
    r   <- 2:(max-1)
    Bins[1,   Zr := 2*Nr/Bins[2,c]]
    Bins[r,   Zr := 2*Nr/(Bins[r+1,c]-Bins[r-1,c])]
    Bins[max, Zr := Nr/(c-Bins[(max-1),c])]
}
avg.zr(UniBins)
avg.zr(BiBins)
avg.zr(TriBins)
avg.zr(QuadBins)

# Fit log-linear regression: log(Z_r) = a + b*log(c)
FitLM <- function(CountTbl) {
    return(lm(log(Zr) ~ log(c), data=CountTbl))
}
UniLM  <- FitLM(UniBins)
BiLM   <- FitLM(BiBins)
TriLM  <- FitLM(TriBins)
QuadLM <- FitLM(QuadBins)

# Apply Katz discounting for low-count ngrams (c <= k)
k = 5
Cal_GTDiscount <- function(cnt, N) {
    model <- switch(N, UniLM, BiLM, TriLM, QuadLM)
    Z1    <- exp(predict(model, newdata=data.frame(c=1)))
    Zr    <- exp(predict(model, newdata=data.frame(c=cnt)))
    Zrp1  <- exp(predict(model, newdata=data.frame(c=(cnt+1))))
    Zkp1  <- exp(predict(model, newdata=data.frame(c=(k+1))))
    sub   <- ((k+1)*Zkp1)/(Z1)
    return(((cnt+1)*(Zrp1)/(Zr) - cnt*sub) / (1-sub))
}

UpdateCount <- function(FreqTbl, N) {
    FreqTbl[c > k,  cDis := as.numeric(c)]
    FreqTbl[c <= k, cDis := Cal_GTDiscount(c, N)]
}
UpdateCount(UniFreq,  1)
UpdateCount(BiFreq,   2)
UpdateCount(TriFreq,  3)
UpdateCount(QuadFreq, 4)

setkey(UniFreq,  term)
setkey(BiFreq,   term)
setkey(TriFreq,  term)
setkey(QuadFreq, term)


######################## PREDICTION FUNCTIONS

get.obs.NGrams.by.pre <- function(wordseq, NgramFreq) {
    PreTxt <- sprintf("%s%s%s", "^", wordseq, "_")
    NgramFreq[grep(PreTxt, NgramFreq[,term], perl=TRUE, useBytes=TRUE),]
}

get.unobs.Ngram.tails <- function(ObsNgrams, N) {
    ObsTails <- str_split_fixed(ObsNgrams[,term], "_", N)[,N]
    return(data.table(term=UniFreq[!ObsTails, term, on="term"]))
}

cal.obs.prob <- function(ObsNgrams, Nm1Grams, wordseq) {
    PreCount <- Nm1Grams[wordseq, c, on=.(term)]
    ObsNgrams[, Prob := ObsNgrams[,cDis] / PreCount]
}

cal.alpha <- function(ObsNGrams, Nm1Grams, wordseq) {
    if (dim(ObsNGrams)[1] != 0) {
        return(sum(ObsNGrams[, c-cDis] / Nm1Grams[wordseq, c, on=.(term)]))
    } else {
        return(1)
    }
}

# BUG: quanteda uses "_" as ngram separator internally but prediction
# functions use "_" to split ngrams -- this causes correct splitting but
# the resulting words still have "_" in them when passed back to the
# user, producing unreadable output. This is the root cause of the
# "same five words for any input" failure -- the alpha normalization
# collapses when underscore-separated terms don't match space-separated
# user input in the backoff chain.
Find_Next_word <- function(xy, words_num) {
    xy <- gsub(" ", "_", xy)
    if (length(which(BiFreq$term == xy)) > 0) {
        ObsTriG       <- get.obs.NGrams.by.pre(xy, TriFreq)
        y             <- str_split_fixed(xy, "_", 2)[,2]
        ObsBiG        <- get.obs.NGrams.by.pre(y, BiFreq)
        UnObsBiTails  <- get.unobs.Ngram.tails(ObsBiG, 2)
        ObsBiG        <- ObsBiG[!str_split_fixed(ObsTriG[,term], "_", 2)[,2], on="term"]
        ObsTriG       <- cal.obs.prob(ObsTriG, BiFreq, xy)
        Alpha_xy      <- cal.alpha(ObsTriG, BiFreq, xy)
        ObsBiG        <- cal.obs.prob(ObsBiG, UniFreq, y)
        Alpha_y       <- cal.alpha(ObsBiG, UniFreq, y)
        UnObsBiTails[, Prob := UniFreq[UnObsBiTails, c, on=.(term)] /
                               UniFreq[UnObsBiTails, sum(c), on=.(term)]]
        UnObsBiTails[, Prob := Alpha_xy * Alpha_y * Prob]
        ObsTriG[, c("c", "cDis") := NULL]
        ObsTriG[, term := str_remove(ObsTriG[,term], "([^_]+_)+")]
        ObsBiG[, c("c", "cDis") := NULL]
        ObsBiG[, term := str_remove(ObsBiG[,term], "([^_]+_)+")]
        ObsBiG[, Prob := Alpha_xy * Prob]
        AllTriG <- setorder(rbind(ObsTriG, ObsBiG, UnObsBiTails), -Prob)
        return(AllTriG[Prob!=0][1:min(dim(AllTriG[Prob!=0])[1], words_num)])
    } else {
        y <- str_split_fixed(xy, "_", 2)[,2]
        if (length(which(UniFreq$term == y)) > 0) {
            ObsBiG       <- get.obs.NGrams.by.pre(y, BiFreq)
            ObsBiG       <- cal.obs.prob(ObsBiG, UniFreq, y)
            Alpha_y      <- cal.alpha(ObsBiG, UniFreq, y)
            UnObsBiTails <- get.unobs.Ngram.tails(ObsBiG, 2)
            UnObsBiTails[, Prob := UniFreq[UnObsBiTails, c, on=.(term)] /
                                   UniFreq[UnObsBiTails, sum(c), on=.(term)]]
            UnObsBiTails[, Prob := Alpha_y * Prob]
            ObsBiG[, c("c", "cDis") := NULL]
            ObsBiG[, term := str_remove(ObsBiG[,term], "([^_]+_)+")]
            AllBiG <- setorder(rbind(ObsBiG, UnObsBiTails), -Prob)
            return(AllBiG[Prob!=0][1:words_num])
        } else {
            return(setorder(UniFreq, -cDis)[1:words_num,
                .(term, Prob=cDis/UniFreq[,sum(c)])])
        }
    }
}

Preprocess <- function(wordseq) {
    names(wordseq) <- NULL
    quest <- wordseq %>%
        tokens(remove_numbers=TRUE, remove_punct=TRUE,
               remove_symbols=TRUE, remove_url=TRUE) %>%
        tokens_tolower()
    return(paste(tail(quest$text1, 2), collapse=" "))
}

Next_word <- function(prephrase, words_num=5) {
    bigr   <- Preprocess(prephrase)
    result <- Find_Next_word(bigr, words_num)
    if (dim(result)[1] == 0) rbind(result, list("<Please input more text>", 1))
    return(result)
}


######################## TEST

Next_word("He likes to eat ice")
Next_word("the prime minister")
Next_word("a nuclear power")


######################## ACCURACY ASSESSMENT (added May 2026)

# No formal accuracy test was conducted -- model produced output but
# returned only the same five words for any input. Root cause was the
# quanteda underscore separator incompatibility causing the alpha
# normalization factor to collapse to a constant, making all predictions
# default to the same top unigrams regardless of input.
# Accuracy: not tested (original) | [placeholder] (debugged version)


######################## TIDY

# rm(list = ls())  # clear environment
# gc()             # release memory
