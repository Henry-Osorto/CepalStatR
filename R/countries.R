#' List of countries available in CEPALSTAT
#'
#' @description
#' Retrieves the list of countries available in CEPALSTAT using the
#' dimensions endpoint of a reference indicator. This approach avoids
#' downloading full datasets and ensures consistency with the API structure.
#'
#' @param language.en Logical. If TRUE (default), returns country names in English.
#' If FALSE, returns names in Spanish.
#'
#' @return A data frame containing the list of countries.
#'
#' @details
#' The function extracts the "Country" (or "Pa<c3><ad>s") dimension from the
#' CEPALSTAT API using a JSON-based request. This method is more efficient
#' than retrieving full indicator datasets.
#'
#' @export
#'
#' @examples
#' countries()
#' countries(language.en = FALSE)
countries <- function(language.en = TRUE) {

  # ---- Validaci\u00f3n ----
  if (!is.logical(language.en) || length(language.en) != 1 || is.na(language.en)) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  lang <- if (isTRUE(language.en)) "en" else "es"

  # ---- Llamado a dimensiones ----
  json <- get_cepal_dimensions(id.indicator = 1, lang = lang)

  dims <- json$body$dimensions %||% list()

  if (is.null(dims) || length(dims) == 0) {
    stop("No dimensions found in CEPALSTAT response.", call. = FALSE)
  }

  # ---- Identificar dimensi<c3><b3>n de pa<c3><ad>ses ----

  country_dim <- dims[grepl("Pa\u00eds__ESTANDAR|Country__ESTANDAR", dims$name),]


  if (length(country_dim) == 0) {
    stop("Country dimension not found.", call. = FALSE)
  }

  members <- do.call(rbind, country_dim$members)

  if (is.null(members) || nrow(members) == 0) {
    stop("No country members found.", call. = FALSE)
  }

  countries_vec <- sort(unique(members$name))

  # ---- Salida ----
  if (isTRUE(language.en)) {
    return(data.frame(Countries = countries_vec, stringsAsFactors = FALSE))
  } else {
    return(data.frame(Paises = countries_vec, stringsAsFactors = FALSE))
  }
}
