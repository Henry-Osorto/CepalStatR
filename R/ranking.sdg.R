#' Sustainable Development Goals ranking
#'
#' @description
#' Creates a ranking chart for a CEPALSTAT indicator belonging to the
#' Sustainable Development Goals (SDG/ODS) dimension, using the latest
#' available data by country.
#'
#' @param id.indicator Numeric or character. Indicator ID.
#' @param language.en Logical. If `TRUE` (default), English labels are used.
#' If `FALSE`, Spanish labels are used.
#' @param save Logical. If `TRUE`, saves the resulting figure.
#' @param file Character. Output filename when `save = TRUE`. If `NULL`, a default
#' filename is used.
#' @param format Character. Output format when `save = TRUE`. One of `"png"`
#' (default), `"pdf"` or `"svg"`.
#' @param height Numeric. Height of saved figure.
#' @param width Numeric. Width of saved figure.
#' @param size.title Numeric. Title size.
#' @param title Logical. If `TRUE`, displays the chart title.
#' @param caption Logical. If `TRUE`, displays a note indicating the latest
#' available year by country.
#' @param color Character. Color for countries in the ranking.
#' @param color.gc Character. Color for regional aggregates if present.
#'
#' @return Invisibly returns a `ggplot` object.
#' @export
#'
#' @examples
#' \dontrun{
#' ranking.sdg(id.indicator = 3682)
#' ranking.sdg(id.indicator = 3682, language.en = FALSE)
#'
#' ranking.sdg(
#'   id.indicator = 3682,
#'   save = TRUE
#' )
#'
#' ranking.sdg(
#'   id.indicator = 3682,
#'   save = TRUE,
#'   format = "pdf",
#'   file = "ranking_sdg.pdf"
#' )
#'
#' ranking.sdg(
#'   id.indicator = 3682,
#'   save = TRUE,
#'   format = "svg",
#'   file = "ranking_sdg.svg"
#' )
#' }
ranking.sdg <- function(id.indicator,
                        language.en = TRUE,
                        save = FALSE,
                        file = NULL,
                        format = c("png", "pdf", "svg"),
                        height = 5,
                        width = 9,
                        size.title = 10,
                        title = TRUE,
                        caption = TRUE,
                        color = "#0C4A61",
                        color.gc = "#34B0AA") {

  format <- match.arg(format)

  # ---- Validation ----
  if ((!is.numeric(id.indicator) && !is.character(id.indicator)) ||
      length(id.indicator) != 1 || is.na(id.indicator)) {
    stop("id.indicator must be a single numeric or character value.", call. = FALSE)
  }

  if (!is.logical(language.en) || length(language.en) != 1 || is.na(language.en)) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(save) || length(save) != 1 || is.na(save)) {
    stop("save must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(file) && (!is.character(file) || length(file) != 1 || is.na(file))) {
    stop("file must be NULL or a single character string.", call. = FALSE)
  }

  if (!is.numeric(height) || length(height) != 1 || is.na(height) || height <= 0) {
    stop("height must be a positive number.", call. = FALSE)
  }

  if (!is.numeric(width) || length(width) != 1 || is.na(width) || width <= 0) {
    stop("width must be a positive number.", call. = FALSE)
  }

  if (!is.numeric(size.title) || length(size.title) != 1 || is.na(size.title) || size.title <= 0) {
    stop("size.title must be a positive number.", call. = FALSE)
  }

  if (!is.logical(title) || length(title) != 1 || is.na(title)) {
    stop("title must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(caption) || length(caption) != 1 || is.na(caption)) {
    stop("caption must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.character(color) || length(color) != 1 || is.na(color)) {
    stop("color must be a single character string.", call. = FALSE)
  }

  if (!is.character(color.gc) || length(color.gc) != 1 || is.na(color.gc)) {
    stop("color.gc must be a single character string.", call. = FALSE)
  }

  # ---- Language-specific settings ----
  if (isTRUE(language.en)) {
    indicators_df <- call.indicators(language.en = TRUE, progress = FALSE)
    data <- call.data(
      id.indicator = id.indicator,
      language.en = TRUE,
      progress = FALSE,
      add.indicator.name = TRUE
    )

    dim_col <- "Dimension"
    id_col <- "Indicator ID"
    indicator_name_col <- "Indicator Name"

    country_col <- "Country"
    year_col <- "Years"
    value_col <- "Value"

    target_dimension <- "Sustainable Development Goals (SDG)"

    region_values <- c(
      "Latin America and the Caribbean",
      "Latin America",
      "South America",
      "Central America",
      "Caribbean"
    )

    y_lab <- "Indicator achievement"
    x_lab <- "Country"
    note_prefix <- "Note: Latest data available, "
    file_default <- paste0("Indicator_achievement_", id.indicator, ".", format)

    percent_patterns <- c("Proportion", "(%)", "Percentage")
    warning_msg <- "Select the indicator ID from the Sustainable Development Goals (SDG) dimension."
  } else {
    indicators_df <- call.indicators(language.en = FALSE, progress = FALSE)
    data <- call.data(
      id.indicator = id.indicator,
      language.en = FALSE,
      progress = FALSE,
      add.indicator.name = TRUE
    )

    dim_col <- "Dimensión"
    id_col <- "ID Indicador"
    if (!id_col %in% names(indicators_df)) {
      id_col <- "Id del Indicador"
    }
    indicator_name_col <- "Nombre Indicador"

    country_col <- "País"
    year_col <- "Años"
    value_col <- "Valor"

    target_dimension <- "Objetivos de Desarrollo Sostenible (ODS)"

    region_values <- c(
      "América Latina y el Caribe",
      "América Latina",
      "América del Sur",
      "América Central",
      "El Caribe"
    )

    y_lab <- "Logro del indicador"
    x_lab <- "País"
    note_prefix <- "Nota: Último dato disponible, "
    file_default <- paste0("Logro_del_indicador_", id.indicator, ".", format)

    percent_patterns <- c("Proporción", "(en porcentajes)", "Porcentaje")
    warning_msg <- "Seleccione el ID de indicadores de la dimensión de Objetivos de Desarrollo Sostenible (ODS)."
  }

  # ---- Validate required columns ----
  required_indicator_cols <- c(dim_col, id_col, indicator_name_col)
  missing_indicator_cols <- setdiff(required_indicator_cols, names(indicators_df))
  if (length(missing_indicator_cols) > 0) {
    stop(
      paste0(
        "Missing required columns in call.indicators(): ",
        paste(missing_indicator_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  required_data_cols <- c(country_col, year_col, value_col)
  missing_data_cols <- setdiff(required_data_cols, names(data))
  if (length(missing_data_cols) > 0) {
    stop(
      paste0(
        "Missing required columns in call.data(): ",
        paste(missing_data_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # ---- Find indicator in SDG/ODS dimension ----
  indicators_df <- indicators_df |>
    dplyr::filter(.data[[dim_col]] == target_dimension) |>
    dplyr::filter(as.character(.data[[id_col]]) == as.character(id.indicator))

  if (nrow(indicators_df) == 0) {
    warning(warning_msg, call. = FALSE)
    return(invisible(NULL))
  }

  indicator_row <- indicators_df[1, , drop = FALSE]
  name.indicator <- as.character(indicator_row[[indicator_name_col]][1])

  # ---- Helper to wrap long text ----
  wrap_text <- function(x, width_words = 10) {
    words <- stringr::str_split(x, pattern = "\\s+")[[1]]
    n <- length(words)

    if (n <= width_words) {
      return(paste(words, collapse = " "))
    }

    groups <- split(words, ceiling(seq_along(words) / width_words))
    paste(vapply(groups, paste, collapse = " ", FUN.VALUE = character(1)), collapse = "\n")
  }

  name.indicator.o <- wrap_text(name.indicator, width_words = 10)

  # ---- Detect percentage/proportion-like indicator ----
  percent <- any(vapply(
    percent_patterns,
    function(p) grepl(p, name.indicator, fixed = TRUE),
    logical(1)
  ))

  # ---- Process data with new call.data structure ----
  data <- data |>
    dplyr::group_by(.data[[country_col]]) |>
    dplyr::mutate(
      Maximo = max(.data[[year_col]], na.rm = TRUE),
      filtro = ifelse(.data[[year_col]] == Maximo, 1, 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(filtro == 1) |>
    dplyr::group_by(.data[[year_col]], .data[[country_col]]) |>
    dplyr::summarise(
      valor = mean(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data[[year_col]])

  if (isTRUE(percent)) {
    data <- data |>
      dplyr::mutate(
        filtro = ifelse(.data[[country_col]] %in% region_values, 1, 0)
      )
  } else {
    data <- data |>
      dplyr::filter(!(.data[[country_col]] %in% region_values)) |>
      dplyr::mutate(
        filtro = ifelse(.data[[country_col]] %in% region_values, 1, 0)
      )
  }

  if (nrow(data) == 0) {
    stop("No data available after processing the selected indicator.", call. = FALSE)
  }

  # ---- Build caption ----
  cap_years <- unique(data[[year_col]])
  cap.list <- vector("list", length(cap_years))

  for (i in seq_along(cap_years)) {
    paises <- data[[country_col]][data[[year_col]] == cap_years[i]]
    cap.list[[i]] <- paste0(cap_years[i], ": ", paste(paises, collapse = ", "))
  }

  cap <- paste0(note_prefix, paste(cap.list, collapse = "; "))
  cap <- wrap_text(cap, width_words = 15)

  # ---- Plot ----
  alc <- unique(data$filtro)

  if (length(alc) == 1) {
    g <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = stats::reorder(.data[[country_col]], valor), y = valor)
    ) +
      ggplot2::geom_col(fill = color) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = if (isTRUE(title)) name.indicator.o else NULL,
        y = y_lab,
        x = x_lab,
        caption = if (isTRUE(caption)) cap else NULL
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
        plot.caption = ggplot2::element_text(hjust = 0)
      )
  } else {
    g <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = stats::reorder(.data[[country_col]], valor),
        y = valor,
        fill = as.character(filtro)
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c(color, color.gc)) +
      ggplot2::labs(
        title = if (isTRUE(title)) name.indicator.o else NULL,
        y = y_lab,
        x = x_lab,
        caption = if (isTRUE(caption)) cap else NULL
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
        plot.caption = ggplot2::element_text(hjust = 0),
        legend.position = "none"
      )
  }

  print(g)

  # ---- Save if requested ----
  if (isTRUE(save)) {
    if (is.null(file)) {
      file <- file_default
    }

    if (identical(format, "svg")) {
      if (!requireNamespace("svglite", quietly = TRUE)) {
        stop("Package 'svglite' is required for SVG output.", call. = FALSE)
      }
      svglite::svglite(filename = file, width = width, height = height)
      print(g)
      grDevices::dev.off()
    } else {
      ggplot2::ggsave(
        filename = file,
        plot = g,
        width = width,
        height = height,
        device = format
      )
    }
  }

  return(invisible(g))
}
