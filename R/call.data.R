#' Call Data Indicators CEPALSTAT
#'
#' @description
#' call.data allows querying the CEPALSTAT API to obtain a data frame
#' with indicator values, disaggregations, years, countries, and other
#' dimensions available for the selected indicator.
#'
#' @usage call.data(id.indicator, language.en = TRUE, notes = FALSE)
#'
#' @param id.indicator Indicator ID to retrieve.
#' @param language.en Logical. TRUE for English, FALSE for Spanish.
#' @param notes Logical. If TRUE, methodological notes are added when available.
#'
#' @return A data frame with indicator values and labels.
#' @export
call.data <- function(id.indicator,
                      language.en = TRUE,
                      notes = FALSE) {

  if (missing(id.indicator) || length(id.indicator) != 1) {
    stop("id.indicator must be a single indicator ID.", call. = FALSE)
  }

  if (!is.logical(language.en) || length(language.en) != 1) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(notes) || length(notes) != 1) {
    stop("notes must be TRUE or FALSE.", call. = FALSE)
  }

  lang <- if (isTRUE(language.en)) "en" else "es"

  url.data <- paste0(
    "https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/",
    id.indicator,
    "/data?lang=", lang, "&format=json&in=1"
  )

  data.lista <- cepal_get(url.data, format = "json")

  if (is.null(data.lista$body$data) || nrow(data.lista$body$data) == 0) {
    stop("No data were returned for this indicator.", call. = FALSE)
  }

  data <- data.lista$body$data

  url.dimensiones <- paste0(
    "https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/",
    id.indicator,
    "/dimensions?lang=", lang, "&format=csv&in=1"
  )

  dimension <- cepal_get(url.dimensiones, format = "csv")

  nombre <- names(data)
  nombre <- nombre[grepl("dim_", nombre)]

  if (length(nombre) > 0) {
    for (i in seq_along(nombre)) {
      n <- c(nombre[i], paste0("Labels_", nombre[i]))

      d <- dimension |>
        dplyr::select(member_id, name) |>
        stats::setNames(n)

      data <- suppressMessages(dplyr::left_join(data, d))
    }

    data <- data[, !names(data) %in% nombre, drop = FALSE]
  }

  if ("value" %in% names(data)) {
    data$value <- as.numeric(data$value)
  }

  if ("Labels_dim_29117" %in% names(data)) {
    data$Labels_dim_29117 <- as.numeric(data$Labels_dim_29117)
  }

  if (isTRUE(notes) && "notes_ids" %in% names(data) && any(!is.na(data$notes_ids))) {
    url.notes <- paste0(
      "https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/",
      id.indicator,
      "/footnotes?lang=", lang, "&format=csv"
    )

    notes_df <- cepal_get(url.notes, format = "csv")

    n_notes <- max(stringr::str_count(data$notes_ids, pattern = ","), na.rm = TRUE) + 1

    notas.n <- if (lang == "en") {
      paste0("id_Notes_", seq_len(n_notes))
    } else {
      paste0("id_Nota_", seq_len(n_notes))
    }

    data <- tidyr::separate(data, notes_ids, into = notas.n, fill = "right")

    for (i in seq_along(notas.n)) {
      n <- c(
        notas.n[i],
        if (lang == "en") paste0("Notes_", i) else paste0("Nota_", i)
      )

      d <- stats::setNames(notes_df, n)

      data[[notas.n[i]]] <- as.numeric(data[[notas.n[i]]])

      data <- suppressMessages(dplyr::left_join(data, d))
    }

    data <- data[, !names(data) %in% notas.n, drop = FALSE]
  }

  url.label <- paste0(
    "https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/",
    id.indicator,
    "/dimensions?lang=", lang, "&format=json&in=1"
  )

  labels.var.lista <- cepal_get(url.label, format = "json")

  if (!is.null(labels.var.lista$body$dimensions)) {
    labels.var <- labels.var.lista$body$dimensions[, 1:2, drop = FALSE]
    labels.var$label <- paste0("Labels_dim_", labels.var$id)
    labels.var$name <- gsub("__ESTANDAR", "", labels.var$name)

    nombre_df <- data.frame(label = names(data), stringsAsFactors = FALSE)
    nombre_df <- suppressMessages(dplyr::left_join(nombre_df, labels.var))
    nombre_df <- dplyr::mutate(nombre_df, nombre = ifelse(is.na(name), label, name))

    names(data) <- nombre_df$nombre
  }

  data
}
