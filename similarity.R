# SETUP
source("load.R")

library(lubridate)
library(tidytext)
library(SnowballC)
library(exploratory)

library(cluster)
library(factoextra)
library(NbClust)

data("stop_words")
stop_words <- stop_words %>% add_row(word = "speaker", lexicon = "custom")


# ANALYSIS

## Load the set of statements we want to work with.
base_statements <- qp_statements %>%
  filter(! procedural) %>%
  filter(! is.na(politician_id)) %>%
  inner_join(mps, by = c("member_id" = "id")) %>%
  mutate(year = year(time), week = week(time), year_week = as.integer(paste0(year, week)))

## NB: Change the filter(s) as you see fit.
## Examples: time, party, parliamentary session.
statements_to_analyse <- base_statements %>%
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



## CLUSTERING

### Start with statements that have some similarity.
cluster_statements <- statement_similarity %>%
  filter(value > 0.5)
cluster_statement_ids <- union(cluster_statements$id.x, cluster_statements$id.y)

### Get the statement full text.
cluster_statements_to_analyse <- statements_to_analyse %>%
  filter(id %in% cluster_statement_ids)

### Reduce the statements to lemmatized tokens. (Thanks Busa!)
cluster_statements_by_token <- cluster_statements_to_analyse %>%
  select(id, content_en_plaintext) %>%
  unnest_tokens(word, content_en_plaintext) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word))

### Calculate the TF-IDF score for each word.
cluster_statements_by_word_frequency <- cluster_statements_by_token %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

cluster_statements_with_reduced_dimensions <- cluster_statements_by_word_frequency %>%
  do_svd.kv(id, word, tf_idf, n_component = 3)
  
cluster_statements_with_reduced_dimensions_spread <- cluster_statements_with_reduced_dimensions %>%
  spread(new.dimension, value)

optimal_cluster_count <- NbClust(cluster_statements_with_reduced_dimensions_spread, method = "complete")

clusters2 <- kmeans(cluster_statements_with_reduced_dimensions_spread, centers = 2, nstart = 25)
clusters3 <- kmeans(cluster_statements_with_reduced_dimensions_spread, centers = 3, nstart = 25)
clusters7 <- kmeans(cluster_statements_with_reduced_dimensions_spread, centers = 7, nstart = 25)
clusters12 <- kmeans(cluster_statements_with_reduced_dimensions_spread, centers = 12, nstart = 25)

fviz_nbclust(cluster_statements_with_reduced_dimensions_spread, kmeans, method = "wss")

p1 <- fviz_cluster(clusters2, geom = "point", data = cluster_statements_with_reduced_dimensions_spread) + ggtitle("k = 2")
p2 <- fviz_cluster(clusters3, geom = "point",  data = cluster_statements_with_reduced_dimensions_spread) + ggtitle("k = 3")
p3 <- fviz_cluster(clusters7, geom = "point",  data = cluster_statements_with_reduced_dimensions_spread) + ggtitle("k = 7")
p4 <- fviz_cluster(clusters12, geom = "point",  data = cluster_statements_with_reduced_dimensions_spread) + ggtitle("k = 12")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


## FUNCTIONIZING

analyse_statement_similarity <- function(statements, similarity_threshold = 0.9) {
  statements_by_tokenized_lemmas <- tokenize_by_lemma(statements)
  statements_by_token_frequency <- calculate_word_frequencies(statements_by_tokenized_lemmas)
  statements_scored_for_similarity <- score_document_similarities(statements_by_token_frequency)

  statements_above_similarity_threshold <- statements_scored_for_similarity %>%
    filter(value > similarity_threshold)

  statements_above_similarity_threshold
}

tokenize_by_lemma <- function(statements) {
  statements %>%
    select(id, content_en_plaintext) %>%
    unnest_tokens(word, content_en_plaintext) %>%
    anti_join(stop_words) %>%
    mutate(word = wordStem(word))
}

calculate_word_frequencies <- function(statements_by_tokenized_lemmas) {
  statements_by_tokenized_lemmas %>%
    count(id, word, sort = TRUE) %>%
    ungroup() %>%
    bind_tf_idf(word, id, n)
}

score_document_similarities <- function(statements_by_token_frequency) {
  statements_by_token_frequency %>%
    do_cosine_sim.kv(subject = id, key = word, value = tf_idf, distinct = TRUE)
}

get_details_about_statement_pairs <- function(statement_pairs, all_statements = statements_to_analyse) {
  statement_pairs %>%
    mutate(pair_number = row_number()) %>%
    rowwise() %>%
    mutate(statements = list(view_specific_statements(all_statements, c(id.x, id.y)))) %>%
    select(pair_number, value, statements)
}

view_specific_statements <- function(all_statements, statement_ids) {
  all_statements %>%
    select(id, time, year_week, h2_en, who_en, short_name_en, content_en_plaintext) %>%
    filter(id %in% statement_ids)
}

find_pairs_with_different_dates <- function(pairs) {
  pair_numbers <- pairs %>%
    mutate(date = paste(year(time), yday(time), sep="-")) %>%
    group_by(pair_number) %>%
    summarize(dates = n_distinct(date)) %>%
    filter(dates > 1) %>%
    pull(pair_number)

  pairs %>% filter(pair_number %in% pair_numbers)
}

find_pairs_with_different_values <- function(pairs, column_to_compare) {
  col_to_compare <- enquo(column_to_compare)
  print(col_to_compare)
  
  pair_numbers <- pairs %>%
    group_by(pair_number) %>%
    summarize(comparisons = n_distinct(!! col_to_compare)) %>%
    filter(comparisons > 1) %>%
    pull(pair_number)

  pairs %>% filter(pair_number %in% pair_numbers)
}

view_specific_statements(
  statements_to_analyse,
  analyse_statement_similarity(
    base_statements %>%
      filter(time > "2018-12-01"),
    0.75
  )
) %>% View()

govt <- analyse_statement_similarity(
  base_statements %>%
    filter(year_week > 201825)
) %>%
  get_details_about_statement_pairs()

opp <- analyse_statement_similarity(
  base_statements %>%
    filter(slug.x != "liberal") %>%
    filter(year_week > 201839),
  similarity_threshold = 0.6
) %>%
  get_details_about_statement_pairs()

govt %>%
  unnest() %>%
  find_pairs_with_different_dates() %>%
  select(pair_number, time, value, content_en_plaintext, who_en, id:short_name_en) %>%
  View()

govt %>%
  unnest() %>%
  mutate(date = paste(year(time), yday(time), sep="-")) %>%
  find_pairs_with_different_values(column_to_compare = date) %>%
  select(pair_number, time, value, content_en_plaintext, who_en, id:short_name_en) %>%
  View()

govt %>%
  unnest() %>%
  find_pairs_with_different_values(column_to_compare = who_en) %>%
  select(pair_number, time, value, content_en_plaintext, who_en, id:short_name_en) %>%
  View()

govt %>%
  unnest() %>%
  select(pair_number, value, content_en_plaintext, who_en, id:short_name_en) %>%
  View()

  #group_by(pair_number, value) %>%
  #nest() %>%
  View()

