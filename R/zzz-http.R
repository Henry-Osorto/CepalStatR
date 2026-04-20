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
      paste0("La API de CEPALSTAT devolvi<c3><b3> un error HTTP ", status, "."),
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


img_to_data_uri <- function(path) {
  if (!nzchar(path) || !file.exists(path)) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(path))
  mime <- switch(
    ext,
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    svg = "image/svg+xml",
    "application/octet-stream"
  )

  base64enc::dataURI(file = path, mime = mime)
}
