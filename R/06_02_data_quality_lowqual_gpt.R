#' **Identify low-quality open-ended responses using the OpenAI API**
#'
#' @description
#' This function flags low-quality open-ended survey responses using the OpenAI API.
#' Responses are evaluated for signs of gibberish, nonsense, random text, irrelevant words, or other indicators of poor data quality.
#'
#' @param data A data frame containing the survey data.
#' @param x The open-ended variable to evaluate for response quality.
#' @param batch_size Integer; number of responses per API call. Defaults to `100`.
#' @param model Character string; the OpenAI model to use. Defaults to `gpt-4o`.
#'
#' @details
#' Requires an OpenAI API key, which can be generated at `https://platform.openai.com/`, to be set in your R session using `Sys.setenv(OPENAI_API_KEY="...")`.
#'
#' The model classifies each response as low-quality (`1`) or valid (`0`).
#'
#' A response is flagged as low-quality if it is:
#'  - Gibberish or random characters
#'  - Off-topic or meaningless
#'  - Contains only emojis or irrelevant text
#'
#' A response is considered valid if it is interpretable, relevant, and meaningful.
#'
#' The function appends a new column to the dataset with the results.
#'
#' @return
#' A data frame with an additional variable named `x_lowqual` containing the classification results.
#'
#' @examples
#' # Identify low-quality respondents in question Q5
#' data <- lowqual_gpt(data = survey_data, x = Q5)
#'
#' @export

lowqual_gpt <- function(data, x, batch_size = 100, model = "gpt-4o") {
  # Check required packages ----

  required_pkgs <- c("rlang", "dplyr", "purrr", "stringr", "httr2", "jsonlite")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  `%>%` <- dplyr::`%>%`

  # Check API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (is.null(api_key) || api_key == "") {
    stop(
      "OpenAI API key is not set.\n\n",
      "Please generate one at https://platform.openai.com/ and set it using either of the following methods:\n\n",
      "1. Temporarily (for this session):\n\n",
      "   Sys.setenv(OPENAI_API_KEY = 'your_api_key')\n\n",
      "2. Permanently:\n\n",
      "   Add the following line to your .Renviron file:\n",
      "   OPENAI_API_KEY='your_api_key'\n\n",
      "   You can locate your .Renviron file with:\n",
      "   file.path(Sys.getenv('HOME'), '.Renviron')\n",
      call. = FALSE
    )
  }

  # Capture variable expressions and validate
  var_expr <- rlang::enexpr(x)
  var_name <- rlang::as_string(var_expr)
  if (!var_name %in% names(data)) {stop("Variable ", var_name, " was not found in the dataset.", call. = FALSE)}

  # Identify question label
  q_label <- attr(data[[var_name]], "label", exact = TRUE)
  if (is.null(q_label)) q_label <- var_name

  # Prepare responses
  responses <- data[[var_name]]
  n_total <- length(responses)
  results <- rep(NA_real_, n_total)

  # Identify valid responses
  valid_idx <- which(!is.na(responses) & stringr::str_trim(responses) != "")
  valid_responses <- responses[valid_idx]

  # OpenAI API request ----

  batches <- split(seq_along(valid_responses), ceiling(seq_along(valid_responses) / batch_size))

  batch_results <- purrr::map(batches, function(idx) {
    these_resps <- valid_responses[idx]
    resp_text <- paste0(seq_along(these_resps), ". ", these_resps, collapse = "\n")

    req <- httr2::request("https://api.openai.com/v1/chat/completions") %>%
      httr2::req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ) %>%
      httr2::req_body_json(
        list(
          model = model,
          temperature = 0,
          seed = 123,
          messages = list(
            list(
              role = "system",
              content = paste(
                "You are a survey data quality analyst. Your task is to identify low-quality open-ended responses.",
                "Low-quality means nonsense, gibberish, random text, irrelevant words, or emojis.",
                "High-quality means a meaningful, interpretable response that fits a survey context.",
                "Responses may appear in any language — translate them into English only if necessary, but do not penalize them for being non-English or short if they make sense.",
                "Return only numeric flags in strict JSON array format, with no commentary or explanation.",
                "Follow these rules strictly:",
                "1. Output must be a JSON array of 0s and 1s, same length as the list of responses.",
                "2. 1 = low-quality (nonsense, gibberish, meaningless, random, irrelevant).",
                "3. 0 = valid, meaningful response.",
                "4. Do not include any text or markdown outside the JSON array."
              )
            ),
            list(
              role = "user",
              content = paste(
                "Question label:", q_label, "\n",
                "Responses to check:\n",
                resp_text,
                "\n\nReturn only the JSON array of 0s and 1s, e.g., [0, 1, 0, 0, 1]."
              )
            )
          )
        )
      ) %>%
      httr2::req_perform()

    result <- httr2::resp_body_json(req)
    raw_result <- result$choices[[1]]$message$content

    # Extract JSON substring
    json_text <- stringr::str_extract(raw_result, "\\[\\s*[0-1,\\s]+\\]")

    parsed <- tryCatch({
      jsonlite::fromJSON(json_text)
    }, error = function(e) rep(NA_real_, length(these_resps)))

    if (length(parsed) != length(these_resps)) parsed <- rep(NA_real_, length(these_resps))
    parsed
  })

  # Combine results
  all_valid <- unlist(batch_results)
  results[valid_idx] <- all_valid

  # Output ----

  # Add output variable
  new_var <- paste0(var_name, "_lowqual")
  data[[new_var]] <- results
  attr(data[[new_var]], "label") <- paste(var_name, "low-quality response")

  return(data)
}
