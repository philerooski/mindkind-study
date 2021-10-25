library(synapser)
library(tidyverse)
library(glue)

DEFAULT_HEALTH_DATA_RECORD_TABLE = "syn25178957"
OUTPUT_PARENT <- "syn24611562"

get_default_health_data_records <- function(synapse_id) {
  query <- synTableQuery(glue(
      "SELECT * FROM {synapse_id} ",
      "WHERE dataGroups NOT LIKE '%test_user%'"))
  health_data_records <- synDownloadTableColumns(query, "rawData")
  health_data_records_df <- tibble(
      rawData = names(health_data_records),
      path = health_data_records)
  health_data <- query$asDataFrame() %>%
    as_tibble() %>%
    inner_join(health_data_records_df)
  return(health_data)
}

count_how_did_you_hear_by_site <- function(health_data) {
  site_info <- purrr::pmap_dfr(health_data, function(...) {
    record <- list(...)
    health_data_record <- jsonlite::read_json(record$path)
    result <- tibble(
        whereDoYouLive = health_data_record$whereDoYouLive,
        howDidYouHear = health_data_record$howDidYouHear)
    return(result)
  })
  summarized_site_info <- site_info %>%
    count(whereDoYouLive, howDidYouHear) %>%
    arrange(whereDoYouLive, desc(n))
  return(summarized_site_info)
}

store_to_synapse <- function(site_counts, synapse_parent) {
  fname <- "how_did_you_hear_by_site.csv"
  write_csv(site_counts, fname)
  f <- synapser::File(fname,
                      parent = synapse_parent)
  synStore(f)
  unlink(fname)
}

main <- function() {
  synLogin()
  health_data <- get_default_health_data_records(
      synapse_id = DEFAULT_HEALTH_DATA_RECORD_TABLE)
  site_counts <- count_how_did_you_hear_by_site(
      health_data = health_data)
  store_to_synapse(
      site_counts = site_counts,
      synapse_parent = OUTPUT_PARENT)
}

main()

