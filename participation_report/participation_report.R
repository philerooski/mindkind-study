library(synapser)
library(bridgeclient)
library(tidyverse)

BRIDGE_APP <- "wellcome"
BRIDGE_STUDY <- "wellcome-study"
OUTPUT_PARENT <- "syn24611562"
HEALTH_DATA_SUMMARY_TABLE <- "syn25144942"

read_args <- function() {
  option_list <- list(
      make_option("--bridgeEmail",
          help="Required. The email address associated with your Bridge account."),
      make_option("--bridgePassword", help="Required. Your Bridge password."),
      make_option("--synapseEmail",
          help="Required. The email address associated with your Synapse account."),
      make_option("--synapsePassword", help="Required. Your Synapse password."))
  parser <- OptionParser(
      option_list=option_list,
      prog="Rscript",
      description=paste0(
         "This script produces a tabular file, indexed by phone number ",
         "and month, of how many non-background recorder activities each ",
         "participant completed."))
  args <- parse_args(parser)
  return(args)
}

create_table_index <- function(all_za_users) {
  distinct_healthcodes <- all_za_users %>%
    distinct(healthCode)
  phone_numbers <- purrr::map_dfr(distinct_healthcodes$healthCode, function(hc) {
    this_participant <- bridgeclient::get_participant(health_code = hc)
    if (!is.list(this_participant$phone)) {
      index_row <- tibble(healthCode = hc,
                          phone_number = NA_character_)
    } else {
      index_row <- tibble(healthCode = hc,
                          phone_number = this_participant$phone$number)
    }
    return(index_row)
  })
  distinct_months <- all_za_users %>%
    mutate(month = lubridate::month(createdOn)) %>%
    distinct(month)
  distinct_healthcode_months <- purrr::cross_df(
    list(healthCode = pull(distinct_healthcodes, healthCode),
         month = pull(distinct_months, month)))
  table_index <- inner_join(phone_numbers,
                            distinct_healthcode_months)
  return(table_index)
}

# TODO: possibly select just the active tasks rather than weeding out
# background tasks? Don't forget to remove withdrawl survey from Surveys
# table (check dataType field)
get_non_background_activity_count <- function(all_za_users) {
  activity_counts <- all_za_users %>%
    filter(str_detect(originalTable, "Surveys")) %>%
    mutate(month = lubridate::month(createdOn)) %>%
    count(healthCode, month) %>%
    rename(activity_count = n) %>%
    as_tibble() %>%
    arrange(healthCode, month)
  return(activity_counts)
}

store_to_synapse <- function(participant_report) {
  fname <- "participant_activity_report.csv"
  write_csv(participant_report, fname)
  f <- synapser::File(fname,
                      parent=OUTPUT_PARENT)
  synStore(f)
  unlink(fname)
}

main <- function() {
  args <- read_args()
  synLogin(email = args$synapseEmail, password = args$synapsePassword)
  bridge_login(BRIDGE_APP, args$bridgeEmail, args$bridgePassword)
  all_za_users <- as_tibble(synTableQuery(glue::glue(
      "select * from {HEALTH_DATA_SUMMARY_TABLE} ",
      "where dataGroups like '%ZA%' ",
      "and dataGroups not like '%test%'"
      ))$asDataFrame())
  table_index <- create_table_index(all_za_users)
  activity_counts <- get_non_background_activity_count(all_za_users)
  participant_report <- inner_join(
      table_index,
      activity_counts,
      by = c("healthCode", "month"))
  store_to_synapse(participant_report)
}

main()
