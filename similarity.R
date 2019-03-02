# SETUP
source("load.R")

library(tidytext)
library(SnowballC)
library(exploratory)

data("stop_words")
stop_words <- stop_words %>% add_row(word = "speaker", lexicon = "custom")


# ANALYSIS

## Load the set of statements we want to work with.
base_statements <- qp_statements %>%
  filter(! procedural) %>%
  filter(! is.na(politician_id))

## NB: Change the filter(s) as you see fit.
## Examples: time, party, parliamentary session.
statements_to_analyse <- base_statements %>%
  inner_join(mps, by = c("member_id" = "id")) %>%
  filter(time > "2018-01-01")

## Reduce the statements to lemmatized tokens. (Thanks Busa!)
statements_by_token <- statements_to_analyse %>%
  select(id, content_en_plaintext) %>%
  unnest_tokens(word, content_en_plaintext) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word))

## Calculate the TF-IDF score for each word.
statements_by_word_frequency <- statements_by_token %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

## Calculate the cosine similarity of each QP statement.
statement_similarity <- statements_by_word_frequency %>%
  do_cosine_sim.kv(subject = id, key = word, value = tf_idf, distinct = TRUE)

## Retrieve the statements with the highest similarity scores.
top_similar_statements <- statement_similarity %>%
  filter(value > 0.5) %>%
  top_n(100, value)

## Just get unique IDs of documents that have high similarity scores.
top_similar_statement_ids <- union(top_similar_statements$id.x, top_similar_statements$id.y)

## View the statements with the highest scores.
statements_to_analyse %>%
  select(id, time, h2_en, who_en, short_name_en, content_en_plaintext) %>%
  filter(id %in% top_similar_statement_ids) %>%
  View()
