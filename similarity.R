# SETUP
source("load.R")
source("lib/similarity.R")

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






view_specific_statements(
  base_statements,
  analyse_statement_similarity(
    base_statements %>%
      filter(time > "2018-12-01"),
    0.75
  )
) %>% view_useful_fields() %>% View()

govt <- analyse_statement_similarity(
  base_statements %>%
    filter(slug.x == "liberal") %>%
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

opp %>%
  unnest() %>%
  find_pairs_with_different_values(column_to_compare = who_en) %>%
  select(pair_number, time, value, content_en_plaintext, who_en, id:short_name_en) %>%
  View()

govt %>%
  unnest() %>%
  select(pair_number, value, content_en_plaintext, who_en, id:short_name_en) %>%
  View()
