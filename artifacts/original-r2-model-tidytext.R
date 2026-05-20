# DISPLAY ARTIFACT -- NOT TESTED OR VERIFIED TO RUN END-TO-END
# This script documents the original Simple MLE Backoff model from June 2021.
# Annotations use three categories:
# BUG -- code that was broken and never worked
# REVISED -- code that worked at the time but has been updated
# NOTE -- explanatory context or design decisions
# See model-tidytext-debugged.R for a corrected version.

library(dplyr)    # data wrangling
library(stringr)  # string manipulation
library(tidytext) # text mining and tokenization


######################## LOAD TRAINING DATA

# REVISED: readRDS updated to fread() -- data now saved as CSV
# REVISED: folder prefix and file extension added to paths
uni_words  = readRDS("data/unigrams90")
bi_words   = readRDS("data/bigrams90")
tri_words  = readRDS("data/trigrams90")
quad_words = readRDS("data/quadgrams90")


######################## NGRAM MATCHING FUNCTIONS

bigram = function(input_words) {
    num = length(input_words)
    out = filter(bi_words, word1 == input_words[num]) |>
        top_n(1, n) |>
        filter(row_number() == 1L) |>
        # REVISED: select(num_range("word", 2)) is fragile positional selection
        # works but breaks if column order changes -- use pull(word2) instead
        select(num_range("word", 2)) |>
        as.character()
    ifelse(out == "character(0)", "?", return(out))
}

trigram = function(input_words) {
    num = length(input_words)
    out = filter(tri_words,
                 word1 == input_words[num - 1],
                 word2 == input_words[num]) |>
        top_n(1, n) |>
        filter(row_number() == 1L) |>
        # REVISED: same fragile positional selection
        select(num_range("word", 3)) |>
        as.character()
    ifelse(out == "character(0)", bigram(input_words), return(out))
}

# BUG: quadgram function was missing entirely in the 25June version
# referenced in predict_next_word but never defined -- caused immediate error
# present in revised version only
quadgram = function(input_words) {
    num = length(input_words)
    out = filter(quad_words,
                 word1 == input_words[num - 2],
                 word2 == input_words[num - 1],
                 word3 == input_words[num]) |>
        top_n(1, n) |>
        filter(row_number() == 1L) |>
        # REVISED: same fragile positional selection
        select(num_range("word", 4)) |>
        as.character()
    ifelse(out == "character(0)", trigram(input_words), return(out))
}


######################## PREDICTION FUNCTION

# Cleans user input, determines word count, calls appropriate matching function.

predict_next_word = function(input) {
    input_df    = tibble(text = input)
    replace_reg = "[^[:alpha:][:space:]]*"
    input_df    = input_df |>
        mutate(text = str_replace_all(text, replace_reg, ""))
    input_count = str_count(input_df$text, boundary("word"))
    input_words = unlist(str_split(input_df$text, boundary("word")))
    input_words = tolower(input_words)
    out = ifelse(input_count == 1, bigram(input_words),
          ifelse(input_count == 2, trigram(input_words),
                                   quadgram(input_words)))
    return(out)
}


######################## MANUAL TESTS

predict_next_word("and bought a case of")
predict_next_word("In case of a")


######################## ACCURACY ASSESSMENT (added May 2026)

# No formal accuracy test was conducted for this model iteration.
# Model 1 uses simple maximum likelihood n-gram matching with backoff
# and no smoothing. Predictions were evaluated manually against test
# inputs only. A formal accuracy test was introduced in the final
# Stupid Backoff model (see app.R and app-original.R),
# which achieved 14.21% accuracy on a random sample of 10,000
# quadgram test cases.


######################## TIDY

# rm(list = ls())  # clear environment
# gc()             # release memory
