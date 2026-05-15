# model-r1-quanteda-mle.R
# MLE backoff on quanteda r1 data (stop words retained, 40/10 train/test split).
# Trains on data/quanteda-*-train.rds
# Accuracy written to outputs/accuracy-r1-quanteda-mle.txt

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

######################## MLE PREDICTION -- TOP 10

predict_top10 <- function(input, n = 10) {
    words <- tolower(unlist(strsplit(input, " ")))
    len   <- length(words)

    if (len >= 3) {
        w1 <- words[len-2]; w2 <- words[len-1]; w3 <- words[len]
        matches <- quad_words[.(w1, w2, w3)][order(-count)]
        if (nrow(matches) > 0) return(head(matches$word4, n))
    }
    if (len >= 2) {
        w1 <- words[len-1]; w2 <- words[len]
        matches <- tri_words[.(w1, w2)][order(-count)]
        if (nrow(matches) > 0) return(head(matches$word3, n))
    }
    w1 <- words[len]
    matches <- bi_words[.(w1)][order(-count)]
    if (nrow(matches) > 0) return(head(matches$word2, n))

    return(head(uni_words$word1, n))
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
    sample[, pred_top1   := sapply(top10_preds, `[[`, 1)]
    sample[, pos_pred    := sapply(pred_top1,            function(w) broad_pos(get_pos(w)))]
    sample[, pos_target  := sapply(sample[[target_col]], function(w) broad_pos(get_pos(w)))]
    sample[, pos_hit     := pos_pred == pos_target & !is.na(pos_pred) & !is.na(pos_target)]
    sample
}

cat("Running POS tagging...\n")
bi_sample   <- run_accuracy(bi_sample,   paste(bi_sample$word1),                                    "word2")
tri_sample  <- run_accuracy(tri_sample,  paste(tri_sample$word1, tri_sample$word2),                 "word3")
quad_sample <- run_accuracy(quad_sample, paste(quad_sample$word1, quad_sample$word2, quad_sample$word3), "word4")

all_samples <- rbindlist(list(bi_sample, tri_sample, quad_sample), fill = TRUE)

top10_acc <- round(100 * mean(all_samples$top10_hit, na.rm = TRUE), 2)
pos_acc   <- round(100 * mean(all_samples$pos_hit,   na.rm = TRUE), 2)

result_top10 <- paste0("r1-quanteda-mle | top-10 accuracy: ", top10_acc, "%")
result_pos   <- paste0("r1-quanteda-mle | pos accuracy: ",    pos_acc,   "%")
cat(result_top10, "\n")
cat(result_pos,   "\n")

writeLines(c(result_top10, result_pos), "outputs/accuracy-r1-quanteda-mle.txt")
