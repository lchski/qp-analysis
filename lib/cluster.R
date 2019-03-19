cluster_statements_kmeans <- function(statements_to_cluster) {
  statements_with_reduced_dimensions <- analyse_statement_similarity(
    statements = statements_to_cluster,
    similarity_threshold = 0
  ) %>%
    extract2("token_frequency") %>%
    do_svd.kv(id, word, tf_idf, n_component = 3)

  statements_with_reduced_dimensions_spread <- statements_with_reduced_dimensions %>%
    spread(new.dimension, value)

  optimal_cluster_count_summary <- NbClust(statements_with_reduced_dimensions_spread, method = "complete")

  optimal_cluster_count <- length(unique(optimal_cluster_count_summary$Best.partition))

  clusterer <- function(cluster_count) {
    kmeans(
      statements_with_reduced_dimensions_spread,
      centers = cluster_count,
      nstart = 25
    )
  }
  
  optimal_clusters <- clusterer(optimal_cluster_count)

  clustered_statements <- statements_to_cluster %>%
    mutate(cluster = optimal_clusters$cluster)

  list(
    "scored_statements_by_id" = statements_with_reduced_dimensions_spread,
    "clusterer" = clusterer,
    "clusters" = clusters,
    "cluster_count" = optimal_cluster_count,
    "clustered_statements" = clustered_statements
  )
}
