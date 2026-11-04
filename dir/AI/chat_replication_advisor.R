# =============================================================================
# chat_replication_advisor.R  ·  RAISINS CRD
# =============================================================================

#' Detect if user asks about replication requirements.
detect_replication_question <- function(msg) {
  m <- tolower(msg)
  grepl(paste0(
    "how many.*rep|how many.*replicat|minimum.*rep|min.*rep|",
    "enough.*rep|sufficient.*rep|rep.*needed|rep.*required|",
    "number of rep|no\\.? of rep|replications.*for.*treat|",
    "rep.*for.*\\d+.*treat|\\d+.*treat.*how many.*rep|",
    "error.*df|error.*degree|degree.*freedom.*error|",
    "how many.*rep.*\\d+.*treat|what.*replication|",
    "replication.*enough|replication.*sufficient|",
    "how.*rep.*should|should.*replicate|should.*replicat|adequate.*replicat|",
    "what if.*\\d+.*trt|what if.*\\d+.*treat|",
    "i have \\d+.*trt|i have \\d+.*treat|",
    "\\d+.*trt.*how|\\d+.*treat.*how|",
    "for \\d+.*trt|for \\d+.*treat|",
    "with \\d+.*trt|with \\d+.*treat|",
    "\\d+ trt|\\d+ treatment"
  ), m, perl = TRUE)
}


#' Build a markdown reply with replication guidance.
#' @param n_trt  Number of treatments (NULL \u2192 show a general table)
build_replication_reply <- function(n_trt = NULL) {

  # Uses compute_min_replication() from chat_csv_generator.R
  if (!is.null(n_trt) && !is.na(n_trt) && n_trt >= 2) {
    n_trt  <- as.integer(n_trt)
    min_r  <- compute_min_replication(n_trt)
    err_df <- n_trt * (min_r - 1L)

    return(paste0(
      "**Replication guidance for ", n_trt, " treatments (CRD)**\n\n",
      "In a CRD, **error df = t \u00d7 (r \u2212 1)**.\n",
      "Error df must be **\u2265 12** for reliable F-tests.\n\n",
      "| Item | Value |\n| --- | --- |\n",
      "| Treatments (t) | ", n_trt, " |\n",
      "| Minimum replications (r) | **", min_r, "** |\n",
      "| Error df = t \u00d7 (r \u2212 1) | ", err_df, " |\n",
      "| Total observations (N = t \u00d7 r) | ", n_trt * min_r, " |\n\n",
      if (min_r <= 3) {
        paste0("Even **", min_r, " reps** meets the threshold, but **4\u20135 reps** are recommended for better precision.")
      } else if (min_r <= 5) {
        paste0("You need at least **", min_r, " reps** per treatment \u2014 a practical number for most experiments.")
      } else {
        paste0("You need at least **", min_r, " reps**. With many treatments consider an **RBD** for efficiency.")
      }
    ))
  }

  # General table
  trt_vals <- c(2, 3, 4, 5, 6, 7, 8, 10, 12, 15, 20)
  rows <- sapply(trt_vals, function(t) {
    r  <- compute_min_replication(t)
    paste0("| ", t, " | ", r, " | ", t * (r - 1L), " | ", t * r, " |")
  })

  paste0(
    "**Minimum replications for CRD (error df \u2265 12)**\n\n",
    "| Treatments (t) | Min. Reps (r) | Error df | Total N |\n",
    "| --- | --- | --- | --- |\n",
    paste(rows, collapse = "\n"), "\n\n",
    "**Rule of thumb:** The table shows the minimum \u2014 aim for 4\u20135 reps when practical.\n\n",
    "Tell me your number of treatments and I\u2019ll calculate the exact requirement.")
}
