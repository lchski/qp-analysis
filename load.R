library(tidyverse)
library(textclean)
library(tidytext)
library(SnowballC)
library(exploratory)

data("stop_words")

stop_words <- stop_words %>% add_row(word = "speaker", lexicon = "custom")

library(RPostgreSQL)
library(DBI)
library(dbplyr)

con <- DBI::dbConnect(
    drv = dbDriver("PostgreSQL"),
    host = "localhost",
    dbname = "openparliament"
  )

## Pull all QP statements (those labelled by a string containing "Oral", to capture variations)
qp_statements <- tbl(con, "hansards_statement") %>%
  filter(str_detect(h1_en, "Oral")) %>%
  collect() %>%
  mutate(
    content_en_plaintext = replace_html(content_en)
  ) %>%
  arrange(id)

## Pull all MPs, and information about their parties
mps <- tbl(con, "core_electedmember") %>%
  right_join(tbl(con, "core_party"), by = c("party_id" = "id")) %>%
  right_join(tbl(con, "core_politician"), by = c("politician_id" = "id")) %>%
  collect()

sampleData <- qp_statements %>% top_n(1000, id)

tinyData <- qp_statements %>%
  filter(! procedural) %>%
  filter(! is.na(politician_id)) %>%
  filter(time > "2018-01-01") %>%
  select(id, time, h2_en, who_en, content_en_plaintext) %>%
  unnest_tokens(word, content_en_plaintext) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word))
## TODO: Compare with word stemming vs not

statements_by_word_frequency <- tinyData %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

statement_similarity <- statements_by_word_frequency %>%
  arrange(tf_idf) %>%
  left_join(qp_statements) %>%
  select(id:tf_idf, time, h2_en, who_en) %>%
  do_cosine_sim.kv(subject = id, key = word, value = tf_idf, distinct = TRUE)

top_similar_statements <- statement_similarity %>%
  filter(value > 0.5) %>%
  arrange(desc(value)) %>%
  top_n(100) %>%
  select(id.x, id.y)

qp_statements %>%
  select(id, time, h2_en, who_en, content_en_plaintext) %>%
  filter(id %in% top_similar_statements$id.x | id %in% top_similar_statements$id.y) %>%
  View()
