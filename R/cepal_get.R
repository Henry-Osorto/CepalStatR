#' Internal helper to retrieve CEPALSTAT resources
#'
#' Performs an HTTP GET request to a CEPALSTAT endpoint and parses the
#' response according to the requested format.
#'
#' @param url A non-empty character string containing the complete request URL.
#' @param format Response format. One of `"json"` or `"csv"`.
#' @param timeout_sec Positive numeric value indicating the request timeout
#'   in seconds.
#' @param simplify_vector Logical. If `TRUE`, JSON arrays are simplified when
#'   possible. If `FALSE`, the original nested list structure is preserved.
#'
#' @return The parsed response returned by CEPALSTAT.
#'
#' @keywords internal
cepal_get <- function(
    url,
    format = c("json", "csv"),
    timeout_sec = 60,
    simplify_vector = FALSE
) {
  format <- match.arg(format)

  if (
    !is.character(url) ||
    length(url) != 1L ||
    is.na(url) ||
    !nzchar(url)
  ) {
    stop(
      "url must be a non-empty character string.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(timeout_sec) ||
    length(timeout_sec) != 1L ||
    is.na(timeout_sec) ||
    timeout_sec <= 0
  ) {
    stop(
      "timeout_sec must be a positive numeric value.",
      call. = FALSE
    )
  }

  if (
    !is.logical(simplify_vector) ||
    length(simplify_vector) != 1L ||
    is.na(simplify_vector)
  ) {
    stop(
      "simplify_vector must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  request <- httr2::request(url) |>
    httr2::req_user_agent(
      "CepalStatR (https://github.com/Henry-Osorto/CepalStatR)"
    ) |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_retry(
      max_tries = 5,
      retry_on_failure = TRUE,
      is_transient = function(response) {
        httr2::resp_status(response) %in%
          c(429L, 500L, 502L, 503L, 504L)
      },
      backoff = function(tries) {
        min(60, 2^tries)
      }
    )

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(error) {
      stop(
        paste0(
          "Could not retrieve data from CEPALSTAT: ",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )

  if (identical(format, "json")) {
    return(
      httr2::resp_body_json(
        response,
        simplifyVector = simplify_vector
      )
    )
  }

  utils::read.csv(
    text = httr2::resp_body_string(
      response,
      encoding = "UTF-8"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
