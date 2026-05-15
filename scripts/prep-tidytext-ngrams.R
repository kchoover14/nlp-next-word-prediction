######################## Libraries
library(tidytext)
library(tidyr)
library(dplyr)
library(data.table)


######################## LOAD DATA
train = readRDS("data/corpus-train.rds")
test = readRDS("data/corpus-test.rds")



######################## PREP TRAIN NGRAMS
#### create ngrams
unigrams = train |>
  unnest_tokens(unigram, text, token = "ngrams", n = 1) |>
  count(unigram, sort = TRUE)
bigrams = train |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  count(bigram, sort = TRUE)
trigrams = train |>
  unnest_tokens(trigram, text, token = "ngrams", n = 3) |>
  count(trigram, sort = TRUE)
quadgrams = train |>
  unnest_tokens(quadgram, text, token = "ngrams", n = 4) |>
  count(quadgram, sort = TRUE)

#### trim ngrams
unigrams_trim = unigrams |>
  mutate(proportion=n / sum(n)) |>
  arrange(desc(proportion)) |>
  mutate(coverage = cumsum(proportion))|>
  filter(coverage <= 0.9)
bigrams_trim = bigrams |>
  mutate(proportion=n / sum(n)) |>
  arrange(desc(proportion)) |>
  mutate(coverage = cumsum(proportion))|>
  filter(coverage <= 0.9)
trigrams_trim = trigrams |>
  mutate(proportion=n / sum(n)) |>
  arrange(desc(proportion)) |>
  mutate(coverage = cumsum(proportion))|>
  filter(coverage <= 0.9)
quadgrams_trim = quadgrams |>
  mutate(proportion=n / sum(n)) |>
  arrange(desc(proportion)) |>
  mutate(coverage = cumsum(proportion))|>
  filter(coverage <= 0.9)

#### split ngrams
bigrams_split = bigrams_trim |>
  separate(bigram, c("word1", "word2"), sep = " ")
trigrams_split = trigrams_trim |>
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
quadgrams_split = quadgrams_trim |>
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")

#### index
setkey(setDT(bigrams_split), word1, word2)
setkey(setDT(trigrams_split), word1, word2, word3)
setkey(setDT(quadgrams_split), word1, word2, word3, word4)

#### unite ngrams, OPTIONAL IF FILE BLOAT?
#bigrams_united = unite(bigrams_split, word1, word2, sep = " ")
#trigrams_united = unite(trigrams_split, word1, word2, word3, sep = " ")
#quadgrams_united = unite(quadgrams_split, word1, word2, word3, word4, sep = " ")


######################## PREP TEST NGRAMS
#### create ngrams
bigrams_test = test |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  count(bigram, sort = TRUE)
trigrams_test = test |>
  unnest_tokens(trigram, text, token = "ngrams", n = 3) |>
  count(trigram, sort = TRUE)
quadgrams_test = test |>
  unnest_tokens(quadgram, text, token = "ngrams", n = 4) |>
  count(quadgram, sort = TRUE)

#### trim ngrams
bigrams_test_trim = bigrams_test |>
  mutate(proportion=n / sum(n)) |>
  arrange(desc(proportion)) |>
  mutate(coverage = cumsum(proportion))|>
  filter(coverage <= 0.9)
trigrams_test_trim = trigrams_test |>
  mutate(proportion=n / sum(n)) |>
  arrange(desc(proportion)) |>
  mutate(coverage = cumsum(proportion))|>
  filter(coverage <= 0.9)
quadgrams_test_trim = quadgrams_test |>
  mutate(proportion=n / sum(n)) |>
  arrange(desc(proportion)) |>
  mutate(coverage = cumsum(proportion))|>
  filter(coverage <= 0.9)

#### split ngrams
bigrams_test_split = bigrams_test_trim |>
  separate(bigram, c("word1", "word2"), sep = " ")
trigrams_test_split = trigrams_test_trim |>
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
quadgrams_test_split = quadgrams_test_trim |>
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")

#### index
setkey(setDT(bigrams_test_split), word1, word2)
setkey(setDT(trigrams_test_split), word1, word2, word3)
setkey(setDT(quadgrams_test_split), word1, word2, word3, word4)

# unite ngrams if file bloat, as above



######################## SAVE DATA
saveRDS(unigrams_trim, "data/tidytext-unigrams-train.rds", compress = "xz")
saveRDS(bigrams_split, "data/tidytext-bigrams-train.rds", compress = "xz")
saveRDS(trigrams_split, "data/tidytext-trigrams-train.rds", compress = "xz")
saveRDS(quadgrams_split, "data/tidytext-quadgrams-train.rds", compress = "xz")

saveRDS(bigrams_test_split, "data/tidytext-bigrams-test.rds", compress = "xz")
saveRDS(trigrams_test_split, "data/tidytext-trigrams-test.rds", compress = "xz")
saveRDS(quadgrams_test_split, "data/tidytext-quadgrams-test.rds", compress = "xz")



######################## TIDY
gc()
rm(list = ls())
