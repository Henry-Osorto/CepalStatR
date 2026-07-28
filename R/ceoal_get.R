#' Internal CEPALSTAT GET request helper
#'
#' Performs HTTP GET requests to CEPALSTAT endpoints and parses responses
#' according to the requested format.
#'
#' @param url Character string with the request URL.
#' @param format Character string indicating the response format. One of
#'   `"json"` or `"csv"`.
#' @param timeout_sec Numeric value indicating the request timeout in seconds.
#'   Default is `60`.
#' @param simplify_vector Logical. If `TRUE`, JSON responses are simplified
#'   into vectors, matrices, or data frames when possible. If `FALSE`, the
#'   parsed JSON structure is returned without vector simplification. Default
#'   is `FALSE`.
#'
#' @return Parsed CEPALSTAT response.
#' @keywords internal
cepal_get <- function(url,
                      format = c("json", "csv"),
                      timeout_sec = 60,
                      simplify_vector = FALSE) {
  format <- match.arg(format)

  if (!is.character(url) || length(url) != 1 || is.na(url) || !nzchar(url)) {
    stop("url must be a non-empty character string.", call. = FALSE)
  }

  if (!is.numeric(timeout_sec) || length(timeout_sec) != 1 ||
      is.na(timeout_sec) || timeout_sec <= 0) {
    stop("timeout_sec must be a positive numeric value.", call. = FALSE)
  }

  if (!is.logical(simplify_vector) || length(simplify_vector) != 1 ||
      is.na(simplify_vector)) {
    stop("simplify_vector must be TRUE or FALSE.", call. = FALSE)
  }

  request <- httr2::request(url)
  request <- httr2::req_timeout(request, timeout_sec)
  request <- httr2::req_user_agent(
    request,
    paste0("CepalStatR/", utils::packageVersion("CepalStatR"))
  )

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(e) {
      stop(
        "Could not retrieve data from CEPALSTAT: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  status <- httr2::resp_status(response)

  if (status >= 400) {
    stop(
      "CEPALSTAT API request failed with HTTP status ",
      status,
      ".",
      call. = FALSE
    )
  }

  body <- httr2::resp_body_string(response, encoding = "UTF-8")

  if (format == "json") {
    return(jsonlite::fromJSON(body, simplifyVector = simplify_vector))
  }

  utils::read.csv(
    text = body,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
