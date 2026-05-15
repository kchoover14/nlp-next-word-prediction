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

######################## INTERPOLATION WEIGHTS

lambda4 <- 0.40   # quadgram
lambda3 <- 0.30   # trigram
lambda2 <- 0.20   # bigram
lambda1 <- 0.10   # unigram

######################## INTERPOLATED TOP 10

get_top10_interpolated <- function(input_words) {
    num <- length(input_words)

    uni_probs <- uni_words |>
        mutate(prob = n / sum(n)) |>
        select(word = unigram, prob)

    bi_probs <- if (num >= 1) {
        filter(bi_words, word1 == input_words[num]) |>
            mutate(prob = n / sum(n)) |>
            select(word = word2, prob)
    } else tibble(word = character(), prob = numeric())

    tri_probs <- if (num >= 2) {
        filter(tri_words, word1 == input_words[num-1], word2 == input_words[num]) |>
            mutate(prob = n / sum(n)) |>
            select(word = word3, prob)
    } else tibble(word = character(), prob = numeric())

    quad_probs <- if (num >= 3) {
        filter(quad_words,
               word1 == input_words[num-2],
               word2 == input_words[num-1],
               word3 == input_words[num]) |>
            mutate(prob = n / sum(n)) |>
            select(word = word4, prob)
    } else tibble(word = character(), prob = numeric())

    all_words <- unique(c(uni_probs$word, bi_probs$word, tri_probs$word, quad_probs$word))
    if (length(all_words) == 0) return(head(uni_words$unigram, 10))

    get_p <- function(df, w) { p <- df$prob[df$word == w]; if (length(p) == 0) 0 else p[1] }

    scores <- sapply(all_words, function(w) {
        lambda1 * get_p(uni_probs,  w) +
        lambda2 * get_p(bi_probs,   w) +
        lambda3 * get_p(tri_probs,  w) +
        lambda4 * get_p(quad_probs, w)
    })

    all_words[order(scores, decreasing = TRUE)][1:min(10, length(all_words))]
}

predict_top10 <- function(input) {
    input <- tolower(str_replace_all(input, "[^[:alpha:][:space:]]*", ""))
    words <- unlist(str_split(str_trim(input), boundary("word")))
    get_top10_interpolated(words)
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

result_top10 <- paste0("r1-tidytext-interpolated | top-10 accuracy: ", top10_acc, "%")
result_pos   <- paste0("r1-tidytext-interpolated | pos accuracy: ",    pos_acc,   "%")
cat(result_top10, "\n")
cat(result_pos,   "\n")

writeLines(c(result_top10, result_pos), "outputs/accuracy-r1-tidytext-interpolated.txt")
