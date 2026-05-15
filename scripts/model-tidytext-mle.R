######################## LIBRARIES

library(dplyr)
library(stringr)
library(data.table)
library(udpipe)

######################## LOAD TRAINING DATA

uni_words  <- readRDS("data/tidytext-unigrams-train.rds")
bi_words   <- readRDS("data/tidytext-bigrams-train.rds")
tri_words  <- readRDS("data/tidytext-trigrams-train.rds")
quad_words <- readRDS("data/tidytext-quadgrams-train.rds")

bi_test   <- setDT(readRDS("data/tidytext-bigrams-test.rds"))
tri_test  <- setDT(readRDS("data/tidytext-trigrams-test.rds"))
quad_test <- setDT(readRDS("data/tidytext-quadgrams-test.rds"))

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

######################## NGRAM MATCHING FUNCTIONS -- TOP 10

get_top10 <- function(input_words) {
    num <- length(input_words)

    if (num >= 3) {
        matches <- dplyr::filter(quad_words,
                          word1 == input_words[num-2],
                          word2 == input_words[num-1],
                          word3 == input_words[num]) |>
            arrange(desc(n)) |> slice(1:10) |> pull(word4)
        if (length(matches) > 0) return(matches)
    }
    if (num >= 2) {
        matches <- dplyr::filter(tri_words,
                          word1 == input_words[num-1],
                          word2 == input_words[num]) |>
            arrange(desc(n)) |> slice(1:10) |> pull(word3)
        if (length(matches) > 0) return(matches)
    }
    matches <- dplyr::filter(bi_words, word1 == input_words[num]) |>
        arrange(desc(n)) |> slice(1:10) |> pull(word2)
    if (length(matches) > 0) return(matches)

    return(head(uni_words$unigram, 10))
}

predict_top10 <- function(input) {
    input <- tolower(str_replace_all(input, "[^[:alpha:][:space:]]*", ""))
    words <- unlist(str_split(str_trim(input), boundary("word")))
    get_top10(words)
}

######################## ACCURACY ASSESSMENT

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

cat("Running accuracy assessment...\n")
bi_sample   <- run_accuracy(bi_sample,   paste(bi_sample$word1),                          "word2")
tri_sample  <- run_accuracy(tri_sample,  paste(tri_sample$word1,  tri_sample$word2),      "word3")
quad_sample <- run_accuracy(quad_sample, paste(quad_sample$word1, quad_sample$word2, quad_sample$word3), "word4")

all_samples <- rbindlist(list(bi_sample, tri_sample, quad_sample), fill = TRUE)

top10_acc <- round(100 * mean(all_samples$top10_hit, na.rm = TRUE), 2)
pos_acc   <- round(100 * mean(all_samples$pos_hit,   na.rm = TRUE), 2)

result_top10 <- paste0("r1-tidytext-mle | top-10 accuracy: ", top10_acc, "%")
result_pos   <- paste0("r1-tidytext-mle | pos accuracy: ",    pos_acc,   "%")
cat(result_top10, "\n")
cat(result_pos,   "\n")

writeLines(c(result_top10, result_pos), "outputs/accuracy-r1-tidytext-mle.txt")
