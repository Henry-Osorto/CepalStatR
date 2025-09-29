#' Internal: Perform a GET to CEPALSTAT with retries
#' @keywords internal
cepal_get <- function(base, path = NULL, query = list(), timeout_sec = 60) {
  req <- httr2::request(base) |>
    httr2::req_url_path_append(path %||% "") |>
    httr2::req_user_agent(sprintf("CepalStatR/%s (+https://github.com/Henry-Osorto/CepalStatR)", utils::packageVersion("CepalStatR"))) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_retry(max_tries = 5, backoff = ~min(60, 2^.x), is_retryable = function(resp, err) {
      if (!is.null(err)) return(TRUE)
      status <- httr2::resp_status(resp)
      status >= 500 || status == 429
    })

  resp <- try(httr2::req_perform(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::abort("No se pudo contactar la API de CEPALSTAT. Verifique conexión e intente nuevamente.", class = "cepal_error")
  }
  if (httr2::resp_status(resp) >= 400) {
    msg <- try(httr2::resp_body_string(resp), silent = TRUE)
    rlang::abort(paste0("Error HTTP ", httr2::resp_status(resp), " desde CEPALSTAT: ", msg %||% ""), class = "cepal_error")
  }
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}
