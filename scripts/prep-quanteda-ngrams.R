######################## LIBRARIES

library(quanteda)
library(data.table)



######################## LOAD AND PREP DATA
# load data
train = readRDS("data/corpus-train.rds")
test = readRDS("data/corpus-test.rds")



######################## BUILD QUANTEDA NGRAMS FUNCTION

#set out directory for function
out_dir   <- "data"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

build_quanteda_ngrams <- function(data, label) {
    cat("Building quanteda ngrams for", label, "...\n")

    text_corpus <- corpus(data, text_field = "word")
    text_tokens <- tokens(text_corpus)

    ngram1 <- tokens_ngrams(text_tokens, n = 1)
    ngram2 <- tokens_ngrams(text_tokens, n = 2)
    ngram3 <- tokens_ngrams(text_tokens, n = 3)
    ngram4 <- tokens_ngrams(text_tokens, n = 4)

    ngram1_dfm <- dfm(ngram1)
    ngram2_dfm <- dfm(ngram2)
    ngram3_dfm <- dfm(ngram3)
    ngram4_dfm <- dfm(ngram4)

    uni  <- dfm_trim(ngram1_dfm, min_docfreq = .3)
    bi   <- dfm_trim(ngram2_dfm, min_docfreq = .3)
    tri  <- dfm_trim(ngram3_dfm, min_docfreq = .3)
    quad <- dfm_trim(ngram4_dfm, min_docfreq = .3)

    rm(text_corpus, text_tokens,
       ngram1, ngram2, ngram3, ngram4,
       ngram1_dfm, ngram2_dfm, ngram3_dfm, ngram4_dfm)
    gc()

    sumsU <- colSums(uni)
    sumsB <- colSums(bi)
    sumsT <- colSums(tri)
    sumsQ <- colSums(quad)

    uni_words <- data.table(word1 = names(sumsU), count = sumsU)

    bi_words <- data.table(
        word1 = sapply(strsplit(names(sumsB), "_", fixed = TRUE), '[[', 1),
        word2 = sapply(strsplit(names(sumsB), "_", fixed = TRUE), '[[', 2),
        count  = sumsB)

    tri_words <- data.table(
        word1 = sapply(strsplit(names(sumsT), "_", fixed = TRUE), '[[', 1),
        word2 = sapply(strsplit(names(sumsT), "_", fixed = TRUE), '[[', 2),
        word3 = sapply(strsplit(names(sumsT), "_", fixed = TRUE), '[[', 3),
        count  = sumsT)

    quad_words <- data.table(
        word1 = sapply(strsplit(names(sumsQ), "_", fixed = TRUE), '[[', 1),
        word2 = sapply(strsplit(names(sumsQ), "_", fixed = TRUE), '[[', 2),
        word3 = sapply(strsplit(names(sumsQ), "_", fixed = TRUE), '[[', 3),
        word4 = sapply(strsplit(names(sumsQ), "_", fixed = TRUE), '[[', 4),
        count  = sumsQ)

    setkey(uni_words,  word1)
    setkey(bi_words,   word1, word2)
    setkey(tri_words,  word1, word2, word3)
    setkey(quad_words, word1, word2, word3, word4)

    rm(uni, bi, tri, quad, sumsU, sumsB, sumsT, sumsQ)
    gc()

    cat("Saving", label, "quanteda ngrams...\n")
    saveRDS(uni_words,  file.path(out_dir, paste0("quanteda-unigrams-",  label, ".rds")))
    saveRDS(bi_words,   file.path(out_dir, paste0("quanteda-bigrams-",   label, ".rds")))
    saveRDS(tri_words,  file.path(out_dir, paste0("quanteda-trigrams-",  label, ".rds")))
    saveRDS(quad_words, file.path(out_dir, paste0("quanteda-quadgrams-", label, ".rds")))}

######################## RUN FOR TRAIN AND TEST

build_quanteda_ngrams(train, "train")
build_quanteda_ngrams(test,  "test")


######################## TIDY
rm(list=ls())
gc()
