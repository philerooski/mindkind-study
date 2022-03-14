library(bridgeclient)
library(synapser)
library(optparse)
library(glue)

BRIDGE_APP <- "wellcome"
BRIDGE_STUDY <- "wellcome-study"
OUTPUT_PARENT <- "syn27630674"
OUTPUT_ENTITY_VIEW <- "syn25832312"

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
         "This script exports account data associated with each account in ",
         "the wellcome study to Synapse. Each account has a single file ",
         "exported as a File entity and that File entity is annotated ",
         "with the top-level, atomic properties of the participant account. ",
         "The `clientData` properties are also included as annotations ",
         "on the File."))
  args <- parse_args(parser)
  return(args)
}

get_previously_exported_accounts <- function(synapse_id) {
  q <- synTableQuery(glue("SELECT bridge_id FROM {synapse_id}"))
  df <- q$asDataFrame()
  list_of_bridge_ids <- df[["bridge_id"]]
  return(list_of_bridge_ids)
}

extract_annotations <- function(account_data) {
  # If the property is atomic than we include it as an annotation,
  annotations <- purrr::map(account_data, function(value) {
    if (typeof(value) %in% c("logical", "character", "numeric")) {
      return(value)
    } else {
      return(NULL)
    }
  })
  names(annotations) <- names(account_data)
  annotations <- purrr::compact(annotations)
  #  `createdOn` and `id` are both reserved annotation keys.
  annotations[["id"]] <- NULL
  annotations[["createdOn"]] <- NULL
  annotations[["bridge_id"]] <- account_data[["id"]]
  annotations[["createdOnBridge"]] <- account_data[["createdOn"]]
  # Some non-atomic properties are also useful annotations
  data_groups <- stringr::str_c(account_data[["dataGroups"]], collapse=",")
  annotations[["dataGroups"]] <- data_groups
  if (hasName(account_data, "clientData")) {
    # client data is a flat list (except for gender?)
    client_data_annotations <- purrr::map(account_data[["clientData"]], function(cd) {
      if (typeof(cd) == "list") {
        value <- stringr::str_c(cd, collapse=",")
        return(value)
      } else {
        return(cd)
      }
    })
    names(client_data_annotations) <- names(account_data[["clientData"]])
    annotations <- c(annotations, client_data_annotations)
  }
  clean_names <- purrr::map(names(annotations),
                            ~ stringr::str_replace_all(., "-", "_"))
  names(annotations) <- unlist(clean_names)
  return(annotations)
}

store_to_synapse <- function(annotations) {
  fname <- tempfile(fileext=".json")
  jsonlite::write_json(annotations, fname)
  f <- synapser::File(fname,
                      parent=OUTPUT_PARENT,
                      annotations=annotations)
  synStore(f)
  unlink(fname)
}

main <- function() {
  args <- read_args()
  synLogin(email = args$synapseEmail, password = args$synapsePassword)
  bridge_login(BRIDGE_APP, args$bridgeEmail, args$bridgePassword)
  all_accounts <- get_all_participants()
  previously_exported_accounts <- get_previously_exported_accounts(OUTPUT_ENTITY_VIEW)
  accounts_to_export <- purrr::discard(
      all_accounts, ~ .[["id"]] %in% previously_exported_accounts)
  if (length(accounts_to_export) > 0) {
    account_data <- purrr::map(accounts_to_export, function(account) {
        account_data <- get_participant(user_id = account$id)
        return(account_data)
    })
    annotations <- purrr::map(account_data, extract_annotations)
    purrr::map(annotations, store_to_synapse)
  }
}

main()

