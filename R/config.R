# Internal CEPALSTAT configuration --------------------------------------------

cepalstat_api_base_url <- function() {
  "https://api-cepalstat.cepal.org/cepalstat/api/v1"
}

cepalstat_build_url <- function(path, query = list()) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("path must be a non-empty character string.", call. = FALSE)
  }

  base_url <- sub("/+$", "", cepalstat_api_base_url())
  path <- sub("^/+", "", path)

  url <- paste0(base_url, "/", path)

  if (length(query) > 0) {
    keep <- !vapply(query, is.null, logical(1))
    query <- query[keep]

    if (length(query) > 0) {
      query_string <- paste(
        vapply(names(query), function(nm) {
          paste0(
            utils::URLencode(nm, reserved = TRUE),
            "=",
            utils::URLencode(as.character(query[[nm]]), reserved = TRUE)
          )
        }, character(1)),
        collapse = "&"
      )

      url <- paste0(url, "?", query_string)
    }
  }

  url
}
