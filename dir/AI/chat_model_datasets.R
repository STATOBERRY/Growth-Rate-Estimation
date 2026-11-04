# =============================================================================
# chat_model_datasets.R  ·  RAISINS Repeated Measures One-way ANOVA
# =============================================================================
# Model / example dataset serving for the RA-One chat assistant.
#
# PUBLIC FUNCTIONS (called by chat_server.R):
#   detect_model_dataset_request(msg)      - TRUE if user wants example data
#   get_rcbd_model_datasets(data_dir)       - discover CSV files + metadata
#   build_model_dataset_reply(datasets)    - markdown listing
# =============================================================================


#' Detect if user wants model / example / sample datasets.
detect_model_dataset_request <- function(msg) {
  m <- tolower(msg)
  grepl(paste0(
    "model.*data|example.*data|sample.*data|test.*data|",
    "demo.*data|dataset|data.*file|download.*data|",
    "try.*data|see.*data|show.*data|need.*data|",
    "give.*data|provide.*data|share.*data|",
    "model.*dataset|example.*dataset|sample.*dataset|",
    "dataset.*1|dataset.*2|dataset.*3|",
    "existing.*data|built.?in.*data|default.*data|",
    "pre.?loaded.*data|available.*data"
  ), m, perl = TRUE)
}


#' List bundled Repeated Measures One-way model CSV files.
#' Searches several possible directories. For each CSV, looks for a companion
#' .json (same stem) for label/desc; auto-generates a description otherwise.
#'
#' @param data_dir  Primary directory to search (can be NULL).
#' @return Named list of list(path, label, desc)
get_rcbd_model_datasets <- function(data_dir = NULL) {
  search_dirs <- c(
    data_dir,
    "modules/rm_twoway/data",
    "data",
    "www/data"
  )
  search_dirs <- search_dirs[!is.null(search_dirs) & nzchar(search_dirs)]

  for (dir in search_dirs) {
    if (dir.exists(dir)) {
      csv_files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
      if (length(csv_files) > 0) {
        datasets <- lapply(csv_files, function(csv_path) {
          stem      <- tools::file_path_sans_ext(basename(csv_path))
          json_path <- file.path(dir, paste0(stem, ".json"))

          meta <- if (file.exists(json_path)) {
            tryCatch(jsonlite::fromJSON(json_path),
                     error = function(e) list(label = stem, desc = ""))
          } else {
            df_peek <- tryCatch(read.csv(csv_path, nrows = 5),
                                error = function(e) NULL)
            desc <- if (!is.null(df_peek))
              paste0(ncol(df_peek), " columns: ",
                     paste(names(df_peek), collapse = ", "))
            else ""
            list(label = gsub("_", " ", stem), desc = desc)
          }

          list(
            path  = normalizePath(csv_path),
            label = meta$label %||% gsub("_", " ", stem),
            desc  = meta$desc  %||% ""
          )
        })
        names(datasets) <- tools::file_path_sans_ext(basename(csv_files))
        return(datasets)
      }
    }
  }
  list()
}


#' Markdown reply describing available model datasets.
build_model_dataset_reply <- function(datasets) {
  if (length(datasets) == 0) {
    return(paste0(
      "No model datasets are currently available. ",
      "You can ask me to **create a data template** instead \u2014 ",
      "just tell me treatments, replications, and variables."))
  }

  lines <- sapply(names(datasets), function(nm) {
    ds <- datasets[[nm]]
    desc_part <- if (nzchar(ds$desc)) paste0(" \u2014 ", ds$desc) else ""
    paste0("- **", ds$label, "**", desc_part)
  })

  paste0(
    "Here are the **Repeated Measures One-way ANOVA model datasets** available for download:\n\n",
    paste(lines, collapse = "\n"), "\n\n",
    "Download any of these to study the data arrangement ",
    "or to test the analysis.")
}


#' Local fallback
if (!exists("%||%")) `%||%` <- function(a, b) if (!is.null(a)) a else b
