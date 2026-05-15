# model-r1-quanteda-kneser-ney.R
# Kneser-Ney smoothing on quanteda r1 data (stop words retained, 90/10 split).
# Trains on data/r1-quanteda-train-*.csv
# Tests on  data/quanteda-test-quadgrams.csv
# Accuracy written to outputs/accuracy-r1-quanteda-kneser-ney.txt

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

bi_words   <- bi_words[,   .(count = sum(count)), by = .(word1, word2)]
tri_words  <- tri_words[,  .(count = sum(count)), by = .(word1, word2, word3)]
quad_words <- quad_words[, .(count = sum(count)), by = .(word1, word2, word3, word4)]
setkey(bi_words,   word1, word2)
setkey(tri_words,  word1, word2, word3)
setkey(quad_words, word1, word2, word3, word4)

######################## KNESER-NEY SMOOTHING

discount_value  <- 0.75
numOfBiGrams    <- nrow(bi_words[, by = .(word1, word2)])
ckn             <- bi_words[, .(Prob = ((.N)/numOfBiGrams)), by = word2]
setkey(ckn, word2)

uni_words[, Prob := ckn[word1, Prob]]
uni_words  <- uni_words[!is.na(uni_words$Prob)]
uni_words  <- uni_words[order(-Prob)][1:min(50, .N)]
setkey(uni_words, word1)

n1wi <- bi_words[, .(N = .N), by = word1]
setkey(n1wi, word1)
bi_words[, Cn1 := uni_words[word1, count]]
bi_words[, Prob := ((count - discount_value) / Cn1 +
    discount_value / Cn1 * n1wi[word1, N] * uni_words[word2, Prob])]

tri_words[, Cn2 := bi_words[tri_words[, .(word1, word2)], count, on = .(word1, word2)]]
n1w12 <- tri_words[, .N, by = .(word1, word2)]
setkey(n1w12, word1, word2)
tri_words[, Prob := (count - discount_value) / Cn2 +
    discount_value / Cn2 * n1w12[tri_words[, .(word1, word2)], N, on = .(word1, word2)] *
    bi_words[tri_words[, .(word1, word2)], Prob, on = .(word1, word2)]]

tri_summary <- tri_words[, .(Prob = max(Prob, na.rm = TRUE)), by = .(word1, word2)]
setkey(tri_summary, word1, word2)

quad_words[, Cn2 := bi_words[quad_words[, .(word1, word2)], count, on = .(word1, word2)]]
n1w12 <- quad_words[, .N, by = .(word1, word2)]
setkey(n1w12, word1, word2)
quad_words[, Prob := (count - discount_value) / Cn2 +
    discount_value / Cn2 * n1w12[quad_words[, .(word1, word2)], N, on = .(word1, word2)] *
    tri_summary[quad_words[, .(word1, word2)], Prob, on = .(word1, word2)]]

######################## PREDICTION FUNCTIONS -- TOP 10

uniWords <- function(n = 10) sample(uni_words[, word1], size = n)

biWords <- function(w1, n = 10) {
    pwords <- bi_words[w1][order(-Prob)]
    if (any(is.na(pwords))) return(uniWords(n))
    if (nrow(pwords) >= n) return(pwords[1:n, word2])
    count  <- nrow(pwords)
    return(c(pwords[, word2], uniWords(n)[1:(n - count)]))
}

triWords <- function(w1, w2, n = 10) {
    pwords <- tri_words[.(w1, w2)][order(-Prob)]
    if (any(is.na(pwords))) return(biWords(w2, n))
    if (nrow(pwords) >= n) return(pwords[1:n, word3])
    count  <- nrow(pwords)
    return(c(pwords[, word3], biWords(w2, n)[1:(n - count)]))
}

predict_top10 <- function(input) {
    toks  <- tokens(x = char_tolower(input),
                    remove_punct = TRUE, remove_symbols = TRUE)
    w     <- tail(as.character(toks[[1]]), 2)
    w     <- gsub("_", " ", w)
    words <- triWords(w[1], w[2], 10)
    gsub("_", " ", words)
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

result_top10 <- paste0("r1-quanteda-kneser-ney | top-10 accuracy: ", top10_acc, "%")
result_pos   <- paste0("r1-quanteda-kneser-ney | pos accuracy: ",    pos_acc,   "%")
cat(result_top10, "\n")
cat(result_pos,   "\n")

writeLines(c(result_top10, result_pos), "outputs/accuracy-r1-quanteda-kneser-ney.txt")
