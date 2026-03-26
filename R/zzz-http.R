#' Internal helper to retrieve CEPALSTAT resources
#' @keywords internal
cepal_get <- function(url, format = c("json", "csv"), timeout_sec = 60) {
  format <- match.arg(format)

  req <- httr2::request(url) |>
    httr2::req_user_agent(
      "CepalStatR (https://github.com/Henry-Osorto/CepalStatR)"
    ) |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_retry(
      max_tries = 5,
      retry_on_failure = TRUE,
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
      },
      backoff = function(tries) min(60, 2^tries)
    )

  resp <- try(httr2::req_perform(req), silent = TRUE)

  if (inherits(resp, "try-error")) {
    rlang::abort(
      "No se pudo conectar con la API de CEPALSTAT.",
      class = "cepal_error"
    )
  }

  status <- httr2::resp_status(resp)

  if (status >= 400) {
    rlang::abort(
      paste0("La API de CEPALSTAT devolvió un error HTTP ", status, "."),
      class = "cepal_error"
    )
  }

  if (format == "json") {
    return(httr2::resp_body_json(resp, simplifyVector = TRUE))
  }

  utils::read.csv(
    text = httr2::resp_body_string(resp),
    stringsAsFactors = FALSE
  )
}



# Helper name indicator
#' Get indicator name from indicator ID
#' @keywords internal
get_indicator_name <- function(id.indicator, language.en = TRUE) {
  ind_tbl <- call.indicators(language.en = language.en)

  if (isTRUE(language.en)) {
    id_col <- "Indicator ID"
    name_cols <- c("Indicator.3", "Indicator.2", "Indicator.1")
  } else {
    id_col <- "Id del Indicador"
    name_cols <- c("Indicador.3", "Indicador.2", "Indicador.1")
  }

  row_ind <- ind_tbl[ind_tbl[[id_col]] == as.numeric(id.indicator), , drop = FALSE]

  if (nrow(row_ind) == 0) {
    return(NA_character_)
  }

  vals <- unlist(row_ind[1, name_cols], use.names = FALSE)
  vals <- vals[!is.na(vals) & vals != ""]

  if (length(vals) == 0) {
    return(NA_character_)
  }

  vals[length(vals)]
}
