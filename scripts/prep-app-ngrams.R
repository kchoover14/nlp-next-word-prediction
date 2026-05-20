######################## Libraries

library(tidytext)
library(tidyr)
library(dplyr)
library(data.table)
library(hunspell)


######################## LOAD DATA
# load sampled data
# corpus = read.csv("data/corpus-sample30.csv")
# corpus = read.csv("data/corpus-sample40.csv")
# corpus = read.csv("data/corpus-sample50.csv")
# corpus = read.csv("data/corpus-sample60.csv")
# corpus = read.csv("data/corpus-sample70.csv")
corpus = read.csv("data/corpus-cleaned.csv")

######################## CREATE PENTAGRAMs

pentagrams = corpus |>
  unnest_tokens(pentagrams, word, token = "ngrams", n = 5) |>
  count(pentagrams, sort = TRUE)

######################## PREP NGRAMS

#trim
pentagrams_trim = pentagrams |>
  filter(n >= 5) |>
  mutate(proportion = n / sum(n)) |>
  arrange(desc(proportion)) |>
  mutate(coverage = cumsum(proportion))|>
  filter(coverage <= 0.8)|>
  mutate(coverage = 1-coverage) |>
  select(!c(n, proportion))

# split ngrams
pentagrams_split = pentagrams_trim |>
  separate(pentagrams, c("word1", "word2", "word3", "word4", "word5"), sep = " ")

# remove any ngrams that do not contain english
pentagrams_filtered <- pentagrams_split |>
  filter(word1 %in% c("a", "i") | hunspell_check(word1)) |>
  filter(word2 %in% c("a", "i") | hunspell_check(word2)) |>
  filter(word3 %in% c("a", "i") | hunspell_check(word3)) |>
  filter(word4 %in% c("a", "i") | hunspell_check(word4)) |>
  filter(word5 %in% c("a", "i") | hunspell_check(word5))

# index
setkey(setDT(pentagrams_filtered), word1, word2, word3, word4, word5)



######################## SAVE DATA

write.csv(pentagrams_filtered, "app/pentagrams100.csv")



######################## TIDY
gc()
rm(list = ls())
