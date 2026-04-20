#' Call indicators from CEPALSTAT
#'
#' @description
#' Retrieves the thematic tree from the CEPALSTAT API in JSON format and
#' returns a data frame with the hierarchical structure of indicators.
#'
#' @param language.en Logical. If `TRUE` (default), labels are returned in English.
#' If `FALSE`, labels are returned in Spanish.
#' @param progress Logical. If `TRUE` (default), progress messages are displayed.
#'
#' @return A data frame with the hierarchical thematic structure of CEPALSTAT
#' indicators and the corresponding indicator ID.
#'
#' @export
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' data.indicators <- call.indicators()
#' data.indicators <- call.indicators(language.en = FALSE)
#' }
call.indicators <- function(language.en = TRUE, progress = TRUE) {

  # Validate arguments ----
  if (!is.logical(language.en) || length(language.en) != 1 || is.na(language.en)) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("progress must be TRUE or FALSE.", call. = FALSE)
  }

  # Internal helper ----
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  lang <- if (isTRUE(language.en)) "en" else "es"

  names.en <- c(
    "Area", "Dimension", "Subdimension", "Group",
    "Sub Group Level 1", "Sub Group Level 2",
    "Indicator Name", "Indicator ID"
  )

  names.es <- c(
    "\u00c1rea", "Dimensi\u00f3n", "Subdimensi\u00f3n", "Grupo",
    "Sub Grupo Nivel 1", "Sub Grupo Nivel 2",
    "Nombre Indicador", "ID Indicador"
  )

  url.indicators <- paste0(
    "https://api-cepalstat.cepal.org/cepalstat/api/v1/thematic-tree?lang=",
    lang,
    "&format=json"
  )

  if (progress) {
    if (isTRUE(language.en)) {
      message("Downloading thematic tree from CEPALSTAT...")
    } else {
      message("Descargando \u00e1rbol tem\u00e1tico desde CEPALSTAT...")
    }
  }

  # Download JSON safely ----
  indicadores <- tryCatch(
    jsonlite::fromJSON(url.indicators, simplifyVector = FALSE),
    error = function(e) {
      stop(
        paste0("Could not retrieve the thematic tree from CEPALSTAT: ", e$message),
        call. = FALSE
      )
    }
  )

  # Validate JSON structure ----
  if (!is.list(indicadores) || is.null(indicadores$body)) {
    stop("The CEPALSTAT response does not contain a valid 'body' node.", call. = FALSE)
  }

  if (is.null(indicadores$header$success) || !isTRUE(indicadores$header$success)) {
    stop("The CEPALSTAT API returned an unsuccessful response.", call. = FALSE)
  }

  if (progress) {
    if (isTRUE(language.en)) {
      message("Parsing indicator hierarchy...")
    } else {
      message("Procesando jerarqu\u00eda de indicadores...")
    }
  }

  # Recursive flattening ----
  flatten_cepal_tree <- function(node, path = character(), results = list()) {

    current_name <- node$name %||% NA_character_
    current_path <- c(path, current_name)

    has_children <- !is.null(node$children) && length(node$children) > 0
    has_indicator <- !is.null(node$indicator_id)

    if (has_indicator) {
      results[[length(results) + 1]] <- data.frame(
        level_1 = if (length(current_path) >= 1) current_path[1] else NA_character_,
        level_2 = if (length(current_path) >= 2) current_path[2] else NA_character_,
        level_3 = if (length(current_path) >= 3) current_path[3] else NA_character_,
        level_4 = if (length(current_path) >= 4) current_path[4] else NA_character_,
        level_5 = if (length(current_path) >= 5) current_path[5] else NA_character_,
        level_6 = if (length(current_path) >= 6) current_path[6] else NA_character_,
        level_7 = if (length(current_path) >= 7) current_path[7] else NA_character_,
        node_name = current_name,
        indicator_id = suppressWarnings(as.numeric(node$indicator_id)),
        area_id = if (!is.null(node$area_id)) suppressWarnings(as.numeric(node$area_id)) else NA_real_,
        order = if (!is.null(node$order)) suppressWarnings(as.numeric(node$order)) else NA_real_,
        node_type = "indicator",
        stringsAsFactors = FALSE
      )
    }

    if (has_children) {
      for (child in node$children) {
        results <- flatten_cepal_tree(child, path = current_path, results = results)
      }
    }

    results
  }

  cepal_tree_to_df <- function(json_obj) {
    if (is.null(json_obj$body)) {
      stop("The JSON object does not contain 'body'.", call. = FALSE)
    }

    res <- flatten_cepal_tree(json_obj$body, path = character(), results = list())

    if (length(res) == 0) {
      return(data.frame())
    }

    dplyr::bind_rows(res)
  }

  df <- cepal_tree_to_df(indicadores)

  # Empty result protection ----
  if (!is.data.frame(df) || nrow(df) == 0) {
    stop("No indicators were returned by the CEPALSTAT API.", call. = FALSE)
  }

  # Keep relevant columns and clean values ----
  df <- df |>
    dplyr::select(level_2:node_name, indicator_id) |>
    dplyr::mutate(
      indicator_id = suppressWarnings(as.numeric(indicator_id)),
      dplyr::across(
        .cols = where(is.character),
        .fns = ~ trimws(.x)
      )
    )

  # Rename output columns ----
  if (isTRUE(language.en)) {
    names(df) <- names.en
  } else {
    names(df) <- names.es
  }

  if (progress) {
    if (isTRUE(language.en)) {
      message("Finished downloading indicators.")
    } else {
      message("Descarga de indicadores finalizada.")
    }
  }

  return(df)
}
