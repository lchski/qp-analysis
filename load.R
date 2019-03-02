library(tidyverse)
library(textclean)

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
