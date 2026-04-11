#' @keywords internal
get_cepal_dimensions <- function(id.indicator, lang = "en") {
  url <- paste0(
    "https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/",
    id.indicator,
    "/dimensions?lang=", lang, "&format=json&in=1&path=0"
  )

  jsonlite::fromJSON(url)
}
