#' Call Data Indicators CEPALSTAT
#'
#' @description
#' Retrieves indicator data from the CEPALSTAT API in JSON format and returns
#' a data frame with values, dimensions, metadata, and optionally footnotes.
#'
#' @usage call.data(id.indicator, language.en = TRUE, notes = FALSE,
#'                  progress = TRUE, add.indicator.name = TRUE)
#'
#' @param id.indicator A single CEPALSTAT indicator ID.
#' @param language.en Logical. If `TRUE` (default), the output uses English labels.
#' If `FALSE`, Spanish labels are used when applicable.
#' @param notes Logical. If `TRUE`, footnotes are joined when available.
#' @param progress Logical. If `TRUE`, progress messages are shown.
#' @param add.indicator.name Logical. If `TRUE`, adds indicator ID and indicator name.
#'
#' @return A data frame with indicator values, dimension labels, and metadata.
#' @export
#'
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_remove
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' data <- call.data(id.indicator = 4788)
#' data <- call.data(id.indicator = 4788, language.en = FALSE)
#' data <- call.data(id.indicator = 4788, notes = TRUE)
#' }
call.data <- function(id.indicator,
                      language.en = TRUE,
                      notes = FALSE,
                      progress = TRUE,
                      add.indicator.name = TRUE) {

  # 1. Validate arguments ----
  if (missing(id.indicator) || length(id.indicator) != 1 || is.na(id.indicator)) {
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

  # Internal helper ----
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  lang <- if (isTRUE(language.en)) "en" else "es"

  # 2. Build URL ----
  url.data <- paste0(
    "https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/",
    id.indicator,
    "/data?lang=", lang, "&format=json&in=1"
  )

  if (progress) {
    if (isTRUE(language.en)) {
      message("Downloading indicator data from CEPALSTAT...")
    } else {
      message("Descargando datos del indicador desde CEPALSTAT...")
    }
  }

  # 3. Download JSON safely ----
  data.lista <- tryCatch(
    jsonlite::fromJSON(url.data, simplifyVector = FALSE),
    error = function(e) {
      stop(
        paste0("Could not retrieve indicator data from CEPALSTAT: ", e$message),
        call. = FALSE
      )
    }
  )

  # 4. Validate JSON structure ----
  if (!is.list(data.lista) || is.null(data.lista$body)) {
    stop("The CEPALSTAT response does not contain a valid 'body' node.", call. = FALSE)
  }

  if (is.null(data.lista$header$success) || !isTRUE(data.lista$header$success)) {
    stop("The CEPALSTAT API returned an unsuccessful response.", call. = FALSE)
  }

  body <- data.lista$body

  metadata <- body$metadata %||% list()
  data_raw <- body$data %||% list()
  dimensions <- body$dimensions %||% list()
  footnotes <- body$footnotes %||% list()

  # 5. Recover indicator name from metadata or fallback ----
  indicator_name <- metadata$indicator_name %||% NA_character_

  if (progress) {
    if (!is.na(indicator_name) && nzchar(indicator_name)) {
      if (isTRUE(language.en)) {
        message("Indicator: ", indicator_name, " (ID: ", id.indicator, ")")
      } else {
        message("Indicador: ", indicator_name, " (ID: ", id.indicator, ")")
      }
    } else {
      if (isTRUE(language.en)) {
        message("Indicator ID: ", id.indicator)
      } else {
        message("ID del indicador: ", id.indicator)
      }
    }
  }

  # 6. Convert data to data.frame safely ----
  if (length(data_raw) == 0) {
    data <- data.frame(stringsAsFactors = FALSE)
  } else {
    data <- tryCatch(
      dplyr::bind_rows(data_raw),
      error = function(e) {
        stop(
          paste0("The 'data' node could not be converted into a data frame: ", e$message),
          call. = FALSE
        )
      }
    )
  }

  # 7. Stop if no records ----
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("No data were returned for this indicator.", call. = FALSE)
  }

  # 8. Convert metadata to one-row data.frame safely ----
  metadata_df <- if (length(metadata) > 0) {
    as.data.frame(metadata, stringsAsFactors = FALSE)
  } else {
    data.frame(stringsAsFactors = FALSE)
  }

  # 9. Assign value labels from dimensions ----
  dim_cols <- names(data)
  dim_cols <- dim_cols[grepl("^dim_", dim_cols)]

  if (progress) {
    if (isTRUE(language.en)) {
      message("Assigning labels to dimension values...")
    } else {
      message("Asignando etiquetas a los valores de las dimensiones...")
    }
  }

  if (length(dim_cols) > 0 && length(dimensions) > 0) {

    # convertir dimensions (lista JSON) a estructura simple
    dimension <- data.frame(
      id = suppressWarnings(as.numeric(sapply(dimensions, function(x) x$id %||% NA))),
      stringsAsFactors = FALSE
    )
    dimension$members <- I(lapply(dimensions, function(x) x$members %||% list()))

    id <- names(data)
    id <- id[grepl("^dim_", id)]
    id <- suppressWarnings(as.numeric(gsub("^dim_", "", id)))

    if (length(id) > 0) {
      for (i in id) {

        names_dim <- c(paste0("dim_", i), paste0("Labels_dim_", i))

        d <- dimension[dimension$id == i, , drop = FALSE]
        if (nrow(d) == 0) next

        v <- tryCatch(
          dplyr::bind_rows(d$members[[1]]),
          error = function(e) NULL
        )

        if (is.null(v) || nrow(v) == 0) next
        if (!all(c("id", "name") %in% names(v))) next

        v <- v |>
          dplyr::select(id, name) |>
          dplyr::mutate(
            id = suppressWarnings(as.numeric(id)),
            name = trimws(as.character(name))
          ) |>
          stats::setNames(names_dim)

        # asegurar compatibilidad de tipos para el join
        data[[names_dim[1]]] <- suppressWarnings(as.numeric(data[[names_dim[1]]]))

        data <- suppressMessages(
          dplyr::left_join(data, v, by = setNames(names_dim[1], names_dim[1]))
        )
      }
    }
  }

  # 10. Assign variable labels from dimensions ----
  if (progress) {
    if (isTRUE(language.en)) {
      message("Assigning variable names...")
    } else {
      message("Asignando nombres de variables...")
    }
  }

  if (length(dimensions) > 0) {
    dims_df <- tryCatch(
      do.call(rbind, lapply(dimensions, function(x) {
        data.frame(
          id = as.character(x$id %||% NA),
          name = as.character(x$name %||% NA_character_),
          stringsAsFactors = FALSE
        )
      })),
      error = function(e) NULL
    )

    if (!is.null(dims_df) && nrow(dims_df) > 0) {
      dims_df$name <- stringr::str_remove(dims_df$name, "__ESTANDAR")

      data_names <- data.frame(id = names(data), stringsAsFactors = FALSE)

      suppressMessages(data_names <- data_names |>
                         dplyr::mutate(id_clean = stringr::str_remove(id, "^Labels_dim_")) |>
                         dplyr::left_join(dims_df, by = c("id_clean" = "id")) |>
                         dplyr::mutate(name = ifelse(is.na(name), id, name)))

      names(data) <- data_names$name
    }
  }

  # 11. Add metadata columns ----
  if (nrow(metadata_df) > 0) {
    if (isTRUE(language.en)) {
      meta_cols <- c(
        indicator_meta_name = metadata_df$indicator_name %||% NA_character_,
        unit = metadata_df$unit %||% NA_character_,
        definition = metadata_df$definition %||% NA_character_,
        data_features = metadata_df$data_features %||% NA_character_,
        calculation_methodology = metadata_df$calculation_methodology %||% NA_character_,
        comments = metadata_df$comments %||% NA_character_,
        theme = metadata_df$theme %||% NA_character_,
        area_meta = metadata_df$area %||% NA_character_,
        last_update = metadata_df$last_update %||% NA_character_
      )
    } else {
      meta_cols <- c(
        nombre_indicador_meta = metadata_df$indicator_name %||% NA_character_,
        unidad = metadata_df$unit %||% NA_character_,
        definicion = metadata_df$definition %||% NA_character_,
        caracteristicas_datos = metadata_df$data_features %||% NA_character_,
        metodologia_calculo = metadata_df$calculation_methodology %||% NA_character_,
        comentarios = metadata_df$comments %||% NA_character_,
        tema = metadata_df$theme %||% NA_character_,
        area_meta = metadata_df$area %||% NA_character_,
        ultima_actualizacion = metadata_df$last_update %||% NA_character_
      )
    }

    for (nm in names(meta_cols)) {
      data[[nm]] <- as.character(meta_cols[[nm]])
    }
  }

  # 12. Add footnotes optionally ----
  if (isTRUE(notes) && length(footnotes) > 0) {
    if (progress) {
      if (isTRUE(language.en)) {
        message("Adding footnotes...")
      } else {
        message("Agregando notas...")
      }
    }

    footnotes_df <- tryCatch(
      dplyr::bind_rows(
        lapply(footnotes, function(x) {
          list(
            notes_ids = as.character(x$id %||% NA),
            note_text = as.character(x$description %||% NA_character_)
          )
        })
      ),
      error = function(e) NULL
    )

    if (!is.null(footnotes_df) &&
        nrow(footnotes_df) > 0 &&
        "notes_ids" %in% names(data)) {

      data$notes_ids <- as.character(data$notes_ids)
      suppressMessages(data <- dplyr::left_join(data, footnotes_df, by = "notes_ids"))

      if (isTRUE(language.en)) {
        names(data)[names(data) == "note_text"] <- "Notes"
      } else {
        names(data)[names(data) == "note_text"] <- "Notas"
      }
    }
  }

  # 13. Remove raw dim_* columns ----
  raw_dim_cols <- names(data)
  raw_dim_cols <- raw_dim_cols[grepl("^dim_", raw_dim_cols)]
  if (length(raw_dim_cols) > 0) {
    data <- data[, !names(data) %in% raw_dim_cols, drop = FALSE]
  }

  # 14. Reorder and standardize value column ----
  if ("value" %in% names(data)) {
    data$value <- suppressWarnings(as.numeric(data$value))

    if (isTRUE(language.en)) {
      data <- data |>
        dplyr::mutate(Value = value) |>
        dplyr::select(Value, dplyr::everything(), -value)
    } else {
      data <- data |>
        dplyr::mutate(Valor = value) |>
        dplyr::select(Valor, dplyr::everything(), -value)
    }
  }

  # 15. Drop columns that are usually technical/raw if present ----
  drop_candidates <- c("source_id","notes_ids", "iso3")
  drop_candidates <- drop_candidates[drop_candidates %in% names(data)]
  if (length(drop_candidates) > 0) {
    data <- data[, !names(data) %in% drop_candidates, drop = FALSE]
  }

  # 16. Add indicator name and ID optionally ----
  if (isTRUE(add.indicator.name)) {
    if (isTRUE(language.en)) {
      data$indicator_id <- suppressWarnings(as.numeric(id.indicator))
      data$indicator_name <- indicator_name
    } else {
      data$id_indicador <- suppressWarnings(as.numeric(id.indicator))
      data$nombre_indicador <- indicator_name
    }
  }

  # 17. Trim character columns ----
  data <- data |>
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ trimws(.x)
      )
    )

  # 18. Final message ----
  if (progress) {
    if (isTRUE(language.en)) {
      if (!is.na(indicator_name) && nzchar(indicator_name)) {
        message("Finished download: ", indicator_name)
      } else {
        message("Finished download for indicator ID: ", id.indicator)
      }
    } else {
      if (!is.na(indicator_name) && nzchar(indicator_name)) {
        message("Descarga finalizada: ", indicator_name)
      } else {
        message("Descarga finalizada para el indicador ID: ", id.indicator)
      }
    }
  }

  return(data)
}
