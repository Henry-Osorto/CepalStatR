#' @keywords internal
get_cepal_dimensions <- function(
    id.indicator,
    lang = "en"
) {

  url <- cepalstat_build_url(
    path = paste0(
      "indicator/",
      id.indicator,
      "/dimensions"
    ),
    query = list(
      lang = lang,
      format = "json",
      `in` = 1,
      path = 0
    )
  )

  cepal_get(
    url = url,
    format = "json",
    timeout_sec = 60,
    simplify_vector = TRUE
  )
}
