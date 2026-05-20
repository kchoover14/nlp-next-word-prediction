######################### LIBRARIES
library(data.table)
library(dplyr)
library(stringr)
library(lexicon)



######################## IF RESAMPLING, SKIP TO CHECKPOINT
# load cleaned data and resample from There



######################## DATA PREPARATION
# read data
blogs = readLines("data/SwiftKey/en_US/en_US.blogs.txt", skipNul=TRUE,
                   encoding = "UTF-8")
news = readLines("data/SwiftKey/en_US/en_US.news.txt", skipNul=TRUE,
                  encoding = "UTF-8")
twitter=readLines("data/SwiftKey/en_US/en_US.twitter.txt", skipNul=TRUE,
                   encoding = "UTF-8")

# convert to data.table
blogs   = data.table(text = blogs) |>  mutate(source = "blogs")
news    = data.table(text = news) |>  mutate(source = "news")
twitter = data.table(text = twitter) |>  mutate(source = "twitter")

# combine samples
corpus = bind_rows(blogs, news, twitter) |>
  relocate(source, .before = text)
rm(blogs, news, twitter)



######################## CLEAN DATA
#### remove punct except apost, symbols, http, www

corpus = corpus |>
    mutate(text = tolower(word)) |>
    mutate(text = str_replace_all(text, "[^[:alnum:][:space:]']", "")) |>
    mutate(text = str_replace_all(text, "[[:digit:]]", "")) |>
    mutate(text = str_replace_all(text, " ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")) |>
    mutate(text = str_replace_all(text, " www\\.[^ ]*", ""))

#### Remove profanity

profanity  <- lexicon::profanity_alvarez
all_lines <- corpus |>
  filter(!apply(sapply(profanity, function(bw) grepl(bw, text, fixed = TRUE)), 1, any))



######################## SAVE CLEANED DATA
# change col name
corpus = corpus |> rename(word = text)

# save cleaned data
write.csv(corpus, "data/corpus-cleaned.csv", row.names = FALSE)



######################## SUBSET AND SAVE DATA
#### CHECKPOINT: if resampling, load cleaned data and resample from here
# corpus=read.csv("data/corpus_cleaned.csv")

#### sample 30% of cleaned corpus
set.seed(202630) # for reproducibility

# sample 30%
sample30 = .30
corpus_sample30 <- corpus |>
  group_by(source) |>
  slice_sample(prop = 0.3) |>
  ungroup()

# save
write.csv(corpus_sample30, "data/corpus-sample30.csv", row.names = FALSE)

#### sample 40% of cleaned corpus
set.seed(202640)# for reproducibility

# sample 40%
sample40 = .40
corpus_sample40 <- corpus |>
  group_by(source) |>
  slice_sample(prop = 0.4) |>
  ungroup()

# save
write.csv(corpus_sample40, "data/corpus-sample40.csv", row.names = FALSE)

#### sample 50% of cleaned corpus
set.seed(202650)# for reproducibility

# sample 50%
sample50 = .50
corpus_sample50 <- corpus |>
  group_by(source) |>
  slice_sample(prop = 0.5) |>
  ungroup()

# save
write.csv(corpus_sample50, "data/corpus-sample50.csv", row.names = FALSE)


#### sample 60% of cleaned corpus
set.seed(202660)# for reproducibility

# sample 60%
sample60 = .60
corpus_sample70 <- corpus |>
  group_by(source) |>
  slice_sample(prop = 0.6) |>
  ungroup()

# save
write.csv(corpus_sample70, "data/corpus-sample60.csv", row.names = FALSE)

#### sample 70% of cleaned corpus
set.seed(202670)# for reproducibility

# sample 70%
sample70 = .70
corpus_sample70 <- corpus |>
  group_by(source) |>
  slice_sample(prop = 0.7) |>
  ungroup()

# save
write.csv(corpus_sample70, "data/corpus-sample70.csv", row.names = FALSE)


######################## TIDY

gc()
rm(list = ls())
