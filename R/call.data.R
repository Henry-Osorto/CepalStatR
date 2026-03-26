#' Call Data Indicators CEPALSTAT
#'
#' @description
#' call.data is a function that allows you to consult the CEPALSTAT API to obtain
#' a data frame with the values of the desired indicator, disaggregations, by year,
#' countries and other dimensions available for the selected indicator.
#'
#' @usage call.data(id.indicator, language.en = TRUE, notes = FALSE,
#'                  progress = TRUE, add.indicator.name = TRUE)
#'
#' @param id.indicator You must determine the ID of the indicator or variable that you want to obtain.
#' @param language.en If TRUE or omitted, the default language will be English.
#' Select FALSE to choose the Spanish language.
#' @param notes Logical. If TRUE, methodological notes will be added when available.
#' @param progress Logical. If TRUE, status messages are shown during download.
#' @param add.indicator.name Logical. If TRUE, adds indicator_id and indicator_name columns.
#'
#' @return A data frame with values, labels, and optionally indicator metadata.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_at
#' @importFrom dplyr filter
#' @import tidyr separate
#' @import jsonlite fromJSON
#' @import stats setNames
#' @import stringr str_count
#'
#' @examples
#' data <- call.data(id.indicator = 1)
#' data <- call.data(id.indicator = 1, language.en = FALSE)
#' data <- call.data(id.indicator = 1, notes = TRUE)
#'

utils::globalVariables(c("Years", "Country", "value", "id", "name"))

call.data <- function(id.indicator,
                      language.en = TRUE,
                      notes = FALSE,
                      progress = TRUE,
                      add.indicator.name = TRUE) {

  # Validaciones básicas ----
  if (missing(id.indicator) || length(id.indicator) != 1) {
    stop("id.indicator must be a single indicator ID.", call. = FALSE)
  }

  if (!is.logical(language.en) || length(language.en) != 1 || is.na(language.en)) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(notes) || length(notes) != 1 || is.na(notes)) {
    stop("notes must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("progress must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(add.indicator.name) || length(add.indicator.name) != 1 || is.na(add.indicator.name)) {
    stop("add.indicator.name must be TRUE or FALSE.", call. = FALSE)
  }

  # Helper interno para recuperar el nombre del indicador ----
  get_indicator_name <- function(id.indicator, language.en = TRUE) {
    ind_tbl <- try(call.indicators(language.en = language.en), silent = TRUE)

    if (inherits(ind_tbl, "try-error") || !is.data.frame(ind_tbl)) {
      return(NA_character_)
    }

    # detectar columna del ID de forma robusta
    id_candidates <- c("Indicator ID", "Id del Indicador")
    id_col <- id_candidates[id_candidates %in% names(ind_tbl)][1]

    if (is.na(id_col) || length(id_col) == 0) {
      return(NA_character_)
    }

    idx <- which(trimws(as.character(ind_tbl[[id_col]])) == trimws(as.character(id.indicator)))

    if (length(idx) == 0) {
      return(NA_character_)
    }

    row_ind <- ind_tbl[idx[1], , drop = FALSE]

    # buscar columnas posibles del nombre, en inglés y español
    name_candidates <- c("Indicator.3", "Indicator.2", "Indicator.1",
                         "Indicador.3", "Indicador.2", "Indicador.1")

    name_cols <- name_candidates[name_candidates %in% names(row_ind)]

    if (length(name_cols) == 0) {
      return(NA_character_)
    }

    vals <- unlist(row_ind[1, name_cols], use.names = FALSE)
    vals <- trimws(as.character(vals))
    vals <- vals[!is.na(vals) & vals != "" & vals != "NA"]

    if (length(vals) == 0) {
      return(NA_character_)
    }

    # tomar la última columna no vacía
    vals[length(vals)]
  }

  # Lenguaje ----
  lang <- if (isTRUE(language.en)) "en" else "es"

  # Nombre del indicador ----
  indicator_name <- get_indicator_name(id.indicator = id.indicator,
                                       language.en = language.en)

  if (progress) {
    if (!is.na(indicator_name)) {
      message("Downloading indicator: ", indicator_name, " (ID: ", id.indicator, ")")
    } else {
      message("Downloading indicator ID: ", id.indicator)
    }
  }

  # 1. Definir URL de datos ----
  url.data <- paste0(
    "https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/",
    id.indicator,
    "/data?lang=", lang, "&format=json&in=1"
  )

  # 2. Importar datos en formato JSON ----
  data.lista <- jsonlite::fromJSON(url.data)

  if (is.null(data.lista$body$data) || nrow(data.lista$body$data) == 0) {
    stop("No data were returned for this indicator.", call. = FALSE)
  }

  # 3. Extraer datos ----
  data <- data.lista$body$data

  if (progress) {
    message("Downloading dimensions...")
  }

  # 4. Llamar dimensiones ----
  url.dimensiones <- paste0(
    "https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/",
    id.indicator,
    "/dimensions?lang=", lang, "&format=csv&in=1"
  )

  dimension <- utils::read.csv(url.dimensiones)

  nombre <- names(data)
  nombre <- nombre[grepl("dim_", nombre)]

  if (length(nombre) > 0) {
    for (i in seq_along(nombre)) {
      n <- c(nombre[i], paste0("Labels_", nombre[i]))

      d <- dimension %>%
        dplyr::select(member_id, name) %>%
        stats::setNames(n)

      data <- suppressMessages(dplyr::left_join(data, d))
    }

    data <- data[, !names(data) %in% nombre, drop = FALSE]
  }

  # 5. Cambiar variables a numérico donde corresponde ----
  if ("value" %in% names(data)) {
    data$value <- as.numeric(data$value)
  }

  if ("Labels_dim_29117" %in% names(data)) {
    data$Labels_dim_29117 <- as.numeric(data$Labels_dim_29117)
  }

  # 6. Llamar notas si son solicitadas ----
  if (isTRUE(notes) && "notes_ids" %in% names(data) && any(!is.na(data$notes_ids) & data$notes_ids != "")) {

    if (progress) {
      message("Downloading footnotes...")
    }

    url.notes <- paste0(
      "https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/",
      id.indicator,
      "/footnotes?lang=", lang, "&format=csv"
    )

    notes_df <- utils::read.csv(url.notes)

    notes_non_missing <- data$notes_ids[!is.na(data$notes_ids) & data$notes_ids != ""]
    n_notes <- max(stringr::str_count(notes_non_missing, pattern = ",")) + 1

    if (lang == "en") {
      notas.n <- paste0("id_Notes_", seq_len(n_notes))
      notas.labels <- paste0("Notes_", seq_len(n_notes))
    } else {
      notas.n <- paste0("id_Nota_", seq_len(n_notes))
      notas.labels <- paste0("Nota_", seq_len(n_notes))
    }

    data <- data %>%
      tidyr::separate(notes_ids, into = notas.n, sep = ",", fill = "right", remove = TRUE)

    for (i in seq_along(notas.n)) {
      n <- c(notas.n[i], notas.labels[i])

      d <- notes_df %>%
        stats::setNames(n)

      data[[notas.n[i]]] <- suppressWarnings(as.numeric(data[[notas.n[i]]]))
      data <- suppressMessages(dplyr::left_join(data, d))
    }

    data <- data[, !names(data) %in% notas.n, drop = FALSE]
  }

  if (progress) {
    message("Assigning labels to dimensions...")
  }

  # 7. Asignar nombres a las variables ----
  url.label <- paste0(
    "https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/",
    id.indicator,
    "/dimensions?lang=", lang, "&format=json&in=1"
  )

  labels.var.lista <- jsonlite::fromJSON(url.label)

  if (!is.null(labels.var.lista$body$dimensions) && nrow(labels.var.lista$body$dimensions) > 0) {
    labels.var <- labels.var.lista$body$dimensions[, 1:2, drop = FALSE]
    labels.var$label <- paste0("Labels_dim_", labels.var$id)
    labels.var$name <- gsub("__ESTANDAR", "", labels.var$name)

    nombre_df <- data.frame(label = names(data), stringsAsFactors = FALSE)
    nombre_df <- suppressMessages(dplyr::left_join(nombre_df, labels.var))
    nombre_df <- nombre_df %>%
      dplyr::mutate(nombre = ifelse(is.na(name), label, name))

    names(data) <- nombre_df$nombre
  }

  # 8. Agregar ID y nombre del indicador ----
  if (isTRUE(add.indicator.name)) {
    data$indicator_id <- as.numeric(id.indicator)
    data$indicator_name <- indicator_name
  }

  if (progress) {
    if (!is.na(indicator_name)) {
      message("Finished download: ", indicator_name)
    } else {
      message("Finished download for indicator ID: ", id.indicator)
    }
  }

  return(data)
}
