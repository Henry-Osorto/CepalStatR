#' Retrieve CEPALSTAT dimensions for an indicator
#'
#' @description
#' Retrieves all dimensions associated with a CEPALSTAT indicator,
#' including their members and metadata.
#'
#' @param id.indicator Numeric or character. Indicator ID.
#' @param language.en Logical. If `TRUE` (default), English labels are used.
#' If `FALSE`, Spanish labels are used.
#'
#' @return A list of dimensions. Each element contains:
#' \itemize{
#'   \item name: Dimension name
#'   \item id: Dimension ID
#'   \item members: Data frame of dimension members
#' }
#' @export
#'
#' @examples
#' dims <- cepal_dimensions(1)
#' names(dims)
cepal_dimensions <- function(id.indicator, language.en = TRUE) {

  if ((!is.numeric(id.indicator) && !is.character(id.indicator)) ||
      length(id.indicator) != 1 || is.na(id.indicator)) {
    stop("id.indicator must be a single numeric or character value.", call. = FALSE)
  }

  if (!is.logical(language.en) || length(language.en) != 1 || is.na(language.en)) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  lang <- if (isTRUE(language.en)) "en" else "es"

  json <- get_cepal_dimensions(id.indicator = id.indicator, lang = lang)

  dims <- json$body$dimensions

  if (is.null(dims) || length(dims) == 0) {
    stop("No dimensions found for this indicator.", call. = FALSE)
  }

  out <- lapply(dims, function(d) {

    if (!is.list(d)) {
      return(NULL)
    }

    d_name <- d[["name"]]
    d_id <- d[["id"]]
    members <- d[["members"]]

    members_df <- NULL

    if (is.data.frame(members) && nrow(members) > 0) {
      keep <- intersect(c("id", "name", "order"), names(members))
      members_df <- members[, keep, drop = FALSE]
      rownames(members_df) <- NULL
    } else if (is.list(members) && length(members) > 0) {
      members_df <- tryCatch(
        {
          tmp <- dplyr::bind_rows(members)
          keep <- intersect(c("id", "name", "order"), names(tmp))
          tmp <- tmp[, keep, drop = FALSE]
          rownames(tmp) <- NULL
          tmp
        },
        error = function(e) NULL
      )
    }

    list(
      name = gsub("__ESTANDAR", "", as.character(d_name %||% "")),
      id = d_id,
      members = members_df
    )
  })

  out <- Filter(Negate(is.null), out)
  names(out) <- vapply(out, function(x) x$name, character(1))

  out
}
