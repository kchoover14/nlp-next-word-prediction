######################### LIBRARIES
library(data.table)
library(dplyr)
library(stringr)
library(lexicon)

######################## DATA PREPARATION
# read data
blogs = readLines("data/SwiftKey/en_US/en_US.blogs.txt", skipNul=TRUE,
                   encoding = "UTF-8")
news = readLines("data/SwiftKey/en_US/en_US.news.txt", skipNul=TRUE,
                  encoding = "UTF-8")
twitter=readLines("data/SwiftKey/en_US/en_US.twitter.txt", skipNul=TRUE,
                   encoding = "UTF-8")

# convert to data.table
blogs   = data.table(text = blogs)
news    = data.table(text = news)
twitter = data.table(text = twitter)

# create subsets for train
set.seed(2021)
sample_train = .50
blogs_train = blogs |> sample_n(nrow(blogs)*sample_train)
news_train = news |> sample_n(nrow(news)*sample_train)
twitter_train = twitter |> sample_n(nrow(twitter)*sample_train)

# create subsets for test
set.seed(2021)
sample_test = .10
blogs_test = blogs |> sample_n(nrow(blogs)*sample_test)
news_test = news |> sample_n(nrow(news)*sample_test)
twitter_test = twitter |> sample_n(nrow(twitter)*sample_test)

# combine samples
train = bind_rows(blogs_train, news_train, twitter_train)
test = bind_rows(blogs_test, news_test, twitter_test)

# change
train = train |> rename(word = text)
test = test |> rename(word = text)

# clean env
rm(blogs, blogs_test, blogs_train, news, news_test, news_train, twitter, twitter_test, twitter_train, sample_test, sample_train)




######################## CLEAN DATA
#### remove punct except apost, symbols, http, www
#train
train = train |>
    mutate(text = tolower(word)) |>
    mutate(text = str_replace_all(text, "[^[:alnum:][:space:]']", "")) |>
    mutate(text = str_replace_all(text, "[[:digit:]]", "")) |>
    mutate(text = str_replace_all(text, " ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")) |>
    mutate(text = str_replace_all(text, " www\\.[^ ]*", ""))
#test
test = test |>
    mutate(text = tolower(word)) |>
    mutate(text = str_replace_all(text, "[^[:alnum:][:space:]']", "")) |>
    mutate(text = str_replace_all(text, "[[:digit:]]", "")) |>
    mutate(text = str_replace_all(text, " ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")) |>
    mutate(text = str_replace_all(text, " www\\.[^ ]*", ""))


#### Remove profanity
# train
profanity  <- lexicon::profanity_alvarez
all_lines <- train |>
  filter(!apply(sapply(profanity, function(bw) grepl(bw, text, fixed = TRUE)), 1, any))

# test
all_lines <- test |>
  filter(!apply(sapply(profanity, function(bw) grepl(bw, text, fixed = TRUE)), 1, any))



######################## SAVE DATA
saveRDS(train, "data/corpus-train.rds", compress = "xz")
saveRDS(test, "data/corpus-test.rds", compress = "xz")


######################## TIDY
gc()
rm(list = ls())





