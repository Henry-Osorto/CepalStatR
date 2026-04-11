#' Retrieve CEPALSTAT dimensions for an indicator
#'
#' @description
#' Retrieves all dimensions associated with a CEPALSTAT indicator,
#' including their members and metadata.
#'
#' @param id.indicator Numeric or character. Indicator ID.
#' @param language.en Logical. If TRUE (default), English labels are used.
#' If FALSE, Spanish labels are returned.
#'
#' @return A list of dimensions. Each element contains:
#' \itemize{
#'   \item name: Dimension name
#'   \item id: Dimension ID
#'   \item members: Data frame of dimension members
#' }
#'
#' @export
#'
#' @examples
#' dims <- cepal_dimensions(1)
#' names(dims)
cepal_dimensions <- function(id.indicator,
                             language.en = TRUE) {

  # ---- Validación ----
  if (!is.numeric(id.indicator) && !is.character(id.indicator)) {
    stop("id.indicator must be numeric or character.", call. = FALSE)
  }

  if (!is.logical(language.en) || length(language.en) != 1) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  lang <- if (isTRUE(language.en)) "en" else "es"

  json <- get_cepal_dimensions(id.indicator = id.indicator, lang = lang)

  dims <- json$body$dimensions

  if (is.null(dims) || length(dims) == 0) {
    stop("No dimensions found for this indicator.", call. = FALSE)
  }

  # ---- Transformación ----
  out <- lapply(dims, function(d) {

    members <- d$members

    if (!is.null(members) && nrow(members) > 0) {
      members_df <- data.frame(
        id = members$id,
        name = members$name,
        order = members$order,
        stringsAsFactors = FALSE
      )
    } else {
      members_df <- NULL
    }

    list(
      name = gsub("__ESTANDAR", "", d$name),
      id = d$id,
      members = members_df
    )
  })

  names(out) <- sapply(out, function(x) x$name)

  return(out)
}
