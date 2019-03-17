# SETUP
source("load.R")
source("lib/similarity.R")

library(magrittr)
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
target_ids <- analyse_statement_similarity(
  statements = base_statements %>%
    filter(slug.x == "liberal") %>%
    filter(year_week > 201848),
  similarity_threshold = 0.5
) %>% extract2("above_threshold_ids")

target_statements <- base_statements %>%
  filter(id %in% target_ids)

test <- cluster_statements_kmeans(target_statements)

fviz_cluster(test$clusters, geom = "point", data = test$scored_statements_by_id) +
  ggtitle(paste0("k = ", test$cluster_count))






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
