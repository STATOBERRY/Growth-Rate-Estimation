
classify_chat_intent <- function(msg) {

  # Priority order matters:
  # 1. Replication question  (most specific - "how many reps for 5 treatments")
  # 2. Datagen request       (user specifies t/r/resp, or asks for CSV)
  # 3. Model dataset request (wants built-in example data)
  # 4. Format info           (general layout question, no numbers given)
  # 5. Fall through to API

  if (detect_replication_question(msg))   return("replication")
  if (detect_datagen_request(msg))        return("datagen")
  if (detect_model_dataset_request(msg))  return("model_data")

  # Catch broad "how should I format my data" without specific numbers
  if (grepl(paste0(
    "data.*format|format.*data|how.*upload|upload.*format|",
    "column.*name|header.*name|what.*column"
  ), tolower(msg), perl = TRUE)) return("format_info")

  "api"
}
