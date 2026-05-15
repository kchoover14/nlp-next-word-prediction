# model-r1-quanteda-good-turing.R
# Good-Turing + Katz Backoff on quanteda r1 data (stop words retained, 40/10 train/test split).
# Trains on data/quanteda-*-train.rds
# Accuracy written to outputs/accuracy-r1-quanteda-good-turing.txt

library(dplyr)
library(stringr)
library(data.table)
library(quanteda)
library(udpipe)

######################## LOAD TRAINING DATA

cat("Loading r1 quanteda training data...\n")
uni_words  <- readRDS("data/quanteda-unigrams-train.rds")
bi_words   <- readRDS("data/quanteda-bigrams-train.rds")
tri_words  <- readRDS("data/quanteda-trigrams-train.rds")
quad_words <- readRDS("data/quanteda-quadgrams-train.rds")

setkey(uni_words,  word1)
setkey(bi_words,   word1, word2)
setkey(tri_words,  word1, word2, word3)
setkey(quad_words, word1, word2, word3, word4)

######################## UDPIPE MODEL

cat("Loading udpipe model...\n")
if (!file.exists("data/english-ewt-ud-2.5-191206.udpipe")) {
    dl       <- udpipe_download_model(language = "english-ewt", model_dir = "data")
    ud_model <- udpipe_load_model(dl$file_model)
} else {
    ud_model <- udpipe_load_model("data/english-ewt-ud-2.5-191206.udpipe")
}

get_pos <- function(word) {
    if (is.na(word) || word == "?" || nchar(word) == 0) return(NA_character_)
    result <- as.data.frame(udpipe_annotate(ud_model, x = word))
    if (nrow(result) == 0) return(NA_character_)
    result$upos[1]
}

broad_pos <- function(upos) {
    if (is.na(upos)) return(NA_character_)
    if (upos %in% c("NOUN", "PROPN")) return("noun")
    if (upos == "VERB") return("verb")
    if (upos == "ADJ")  return("adj")
    if (upos == "ADV")  return("adv")
    return("other")
}

######################## COUNT NGRAM FREQUENCIES

uni_freq  <- uni_words[,  .(term = word1,                               c = count)]
bi_freq   <- bi_words[,   .(term = paste(word1, word2, sep="_"),        c = count)]
tri_freq  <- tri_words[,  .(term = paste(word1, word2, word3, sep="_"), c = count)]
quad_freq <- quad_words[, .(term = paste(word1, word2, word3, word4, sep="_"), c = count)]

min_count <- 4
uni_freq  <- uni_freq[c  > min_count]
bi_freq   <- bi_freq[c   > min_count]
tri_freq  <- tri_freq[c  > min_count]
quad_freq <- quad_freq[c > min_count]

setkey(uni_freq,  term)
setkey(bi_freq,   term)
setkey(tri_freq,  term)
setkey(quad_freq, term)

######################## GOOD-TURING SMOOTHING

CountNC <- function(FreqVec) {
    CountTbl <- table(FreqVec[, .(c)])
    data.table(cbind(c = as.integer(names(CountTbl)), Nr = as.integer(CountTbl)))
}

UniBins  <- CountNC(uni_freq)
BiBins   <- CountNC(bi_freq)
TriBins  <- CountNC(tri_freq)
QuadBins <- CountNC(quad_freq)

avg_zr <- function(Bins) {
    max <- dim(Bins)[1]
    r   <- 2:(max-1)
    Bins[1,   Zr := 2*Nr/Bins[2, c]]
    Bins[r,   Zr := 2*Nr/(Bins[r+1, c]-Bins[r-1, c])]
    Bins[max, Zr := Nr/(c-Bins[(max-1), c])]
}
avg_zr(UniBins); avg_zr(BiBins); avg_zr(TriBins); avg_zr(QuadBins)

FitLM <- function(CountTbl) lm(log(Zr) ~ log(c), data = CountTbl)
UniLM  <- FitLM(UniBins)
BiLM   <- FitLM(BiBins)
TriLM  <- FitLM(TriBins)
QuadLM <- FitLM(QuadBins)

k <- 5
Cal_GTDiscount <- function(cnt, N) {
    model <- switch(N, UniLM, BiLM, TriLM, QuadLM)
    Z1    <- exp(predict(model, newdata = data.frame(c = 1)))
    Zr    <- exp(predict(model, newdata = data.frame(c = cnt)))
    Zrp1  <- exp(predict(model, newdata = data.frame(c = (cnt+1))))
    Zkp1  <- exp(predict(model, newdata = data.frame(c = (k+1))))
    sub   <- ((k+1)*Zkp1)/(Z1)
    ((cnt+1)*(Zrp1)/(Zr) - cnt*sub) / (1-sub)
}

UpdateCount <- function(FreqTbl, N) {
    FreqTbl[c > k,  cDis := as.numeric(c)]
    FreqTbl[c <= k, cDis := Cal_GTDiscount(c, N)]
}
UpdateCount(uni_freq, 1); UpdateCount(bi_freq, 2)
UpdateCount(tri_freq, 3); UpdateCount(quad_freq, 4)

setkey(uni_freq, term); setkey(bi_freq, term)
setkey(tri_freq, term); setkey(quad_freq, term)

######################## PREDICTION FUNCTIONS

get_obs_ngrams <- function(wordseq, NgramFreq) {
    PreTxt <- sprintf("%s%s%s", "^", wordseq, "_")
    NgramFreq[grep(PreTxt, NgramFreq[, term], perl = TRUE, useBytes = TRUE), ]
}

get_unobs_tails <- function(ObsNgrams, N) {
    ObsTails <- str_split_fixed(ObsNgrams[, term], "_", N)[, N]
    data.table(term = uni_freq[!ObsTails, term, on = "term"])
}

cal_obs_prob <- function(ObsNgrams, Nm1Grams, wordseq) {
    PreCount <- Nm1Grams[wordseq, c, on = .(term)]
    ObsNgrams[, Prob := ObsNgrams[, cDis] / PreCount]
}

cal_alpha <- function(ObsNGrams, Nm1Grams, wordseq) {
    if (dim(ObsNGrams)[1] != 0) {
        sum(ObsNGrams[, c-cDis] / Nm1Grams[wordseq, c, on = .(term)])
    } else { 1 }
}

Find_Next_word <- function(xy, words_num) {
    xy <- gsub(" ", "_", xy)
    if (length(which(bi_freq$term == xy)) > 0) {
        ObsTriG      <- get_obs_ngrams(xy, tri_freq)
        y            <- str_split_fixed(xy, "_", 2)[, 2]
        ObsBiG       <- get_obs_ngrams(y, bi_freq)
        UnObsBiTails <- get_unobs_tails(ObsBiG, 2)
        ObsBiG       <- ObsBiG[!str_split_fixed(ObsTriG[, term], "_", 2)[, 2], on = "term"]
        ObsTriG      <- cal_obs_prob(ObsTriG, bi_freq, xy)
        Alpha_xy     <- cal_alpha(ObsTriG, bi_freq, xy)
        ObsBiG       <- cal_obs_prob(ObsBiG, uni_freq, y)
        Alpha_y      <- cal_alpha(ObsBiG, uni_freq, y)
        UnObsBiTails[, Prob := uni_freq[UnObsBiTails, c, on = .(term)] /
                               uni_freq[UnObsBiTails, sum(c), on = .(term)]]
        UnObsBiTails[, Prob := Alpha_xy * Alpha_y * Prob]
        ObsTriG[, c("c","cDis") := NULL]
        ObsTriG[, term := gsub("_"," ", str_remove(ObsTriG[, term], "([^_]+_)+"))]
        ObsBiG[,  c("c","cDis") := NULL]
        ObsBiG[,  term := gsub("_"," ", str_remove(ObsBiG[, term], "([^_]+_)+"))]
        ObsBiG[,  Prob := Alpha_xy * Prob]
        AllTriG <- setorder(rbind(ObsTriG, ObsBiG, UnObsBiTails), -Prob)
        return(AllTriG[Prob != 0][1:min(dim(AllTriG[Prob != 0])[1], words_num)])
    } else {
        y <- str_split_fixed(xy, "_", 2)[, 2]
        if (length(which(uni_freq$term == y)) > 0) {
            ObsBiG       <- get_obs_ngrams(y, bi_freq)
            ObsBiG       <- cal_obs_prob(ObsBiG, uni_freq, y)
            Alpha_y      <- cal_alpha(ObsBiG, uni_freq, y)
            UnObsBiTails <- get_unobs_tails(ObsBiG, 2)
            UnObsBiTails[, Prob := uni_freq[UnObsBiTails, c, on = .(term)] /
                                   uni_freq[UnObsBiTails, sum(c), on = .(term)]]
            UnObsBiTails[, Prob := Alpha_y * Prob]
            ObsBiG[, c("c","cDis") := NULL]
            ObsBiG[, term := gsub("_"," ", str_remove(ObsBiG[, term], "([^_]+_)+"))]
            AllBiG <- setorder(rbind(ObsBiG, UnObsBiTails), -Prob)
            return(AllBiG[Prob != 0][1:words_num])
        } else {
            return(setorder(uni_freq, -cDis)[1:words_num, .(term, Prob = cDis/uni_freq[, sum(c)])])
        }
    }
}

Next_word <- function(prephrase, words_num = 10) {
    toks   <- tokens(x = tolower(prephrase),
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_url = TRUE)
    w      <- tail(as.character(toks[[1]]), 2)
    w      <- gsub("_", " ", w)
    bigr   <- paste(w, collapse = " ")
    result <- Find_Next_word(bigr, words_num)
    if (dim(result)[1] == 0) return(data.table(term = "<no match>", Prob = 1))
    return(result)
}

predict_top10 <- function(input) {
    result <- tryCatch(Next_word(input, words_num = 10), error = function(e) NULL)
    if (is.null(result) || nrow(result) == 0) return(character(0))
    result$term
}

######################## MANUAL TESTS

cat("\nManual tests:\n")
cat("i went to the:", paste(predict_top10("i went to the"), collapse=", "), "\n")
cat("happy birthday to:", paste(predict_top10("happy birthday to"), collapse=", "), "\n")
cat("i want to go:", paste(predict_top10("i want to go"), collapse=", "), "\n")

######################## ACCURACY ASSESSMENT

cat("\nRunning accuracy assessment on test data...\n")
bi_test   <- readRDS("data/quanteda-bigrams-test.rds")
tri_test  <- readRDS("data/quanteda-trigrams-test.rds")
quad_test <- readRDS("data/quanteda-quadgrams-test.rds")

set.seed(2021)
bi_sample   <- bi_test[sample(.N,   min(200, .N))]
tri_sample  <- tri_test[sample(.N,  min(300, .N))]
quad_sample <- quad_test[sample(.N, min(500, .N))]

run_accuracy <- function(sample, context, target_col) {
    sample[, top10_preds := lapply(context, predict_top10)]
    sample[, top10_hit   := mapply(function(preds, target) target %in% preds,
                                   top10_preds, sample[[target_col]])]
    sample[, pred_top1   := sapply(top10_preds, function(x) if (length(x) > 0) x[1] else NA_character_)]
    sample[, pos_pred    := sapply(pred_top1,            function(w) broad_pos(get_pos(w)))]
    sample[, pos_target  := sapply(sample[[target_col]], function(w) broad_pos(get_pos(w)))]
    sample[, pos_hit     := pos_pred == pos_target & !is.na(pos_pred) & !is.na(pos_target)]
    sample
}

cat("Running POS tagging...\n")
bi_sample   <- run_accuracy(bi_sample,   paste(bi_sample$word1),                                        "word2")
tri_sample  <- run_accuracy(tri_sample,  paste(tri_sample$word1, tri_sample$word2),                     "word3")
quad_sample <- run_accuracy(quad_sample, paste(quad_sample$word1, quad_sample$word2, quad_sample$word3), "word4")

all_samples <- rbindlist(list(bi_sample, tri_sample, quad_sample), fill = TRUE)

top10_acc <- round(100 * mean(all_samples$top10_hit, na.rm = TRUE), 2)
pos_acc   <- round(100 * mean(all_samples$pos_hit,   na.rm = TRUE), 2)

result_top10 <- paste0("r1-quanteda-good-turing | top-10 accuracy: ", top10_acc, "%")
result_pos   <- paste0("r1-quanteda-good-turing | pos accuracy: ",    pos_acc,   "%")
cat(result_top10, "\n")
cat(result_pos,   "\n")

writeLines(c(result_top10, result_pos), "outputs/accuracy-r1-quanteda-good-turing.txt")
