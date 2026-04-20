#' Population pyramids
#'
#' @description
#' Creates one or multiple population pyramids for a selected country and years
#' using CEPALSTAT population-by-age-and-sex data.
#'
#' @param country Character. Name of the country or region to plot.
#' @param years Numeric vector of positions between 1 and 31 corresponding to
#' 5-year intervals from 1950 to 2100.
#' @param language.en Logical. If `TRUE` (default), English labels are used.
#' If `FALSE`, Spanish labels are used.
#' @param color Character vector of length 2 with colors for men and women.
#' @param save Logical. If `TRUE`, saves the resulting figure.
#' @param file Character. Output filename when `save = TRUE`. If `NULL`, a default
#' filename is used.
#' @param format Character. Output format when `save = TRUE`. One of `"png"`
#' (default), `"pdf"` or `"svg"`.
#' @param height Numeric. Height of saved figure.
#' @param width Numeric. Width of saved figure.
#' @param caption Logical. If `TRUE`, adds a caption with source information.
#' @param progress Logical. If `TRUE`, progress messages are shown.
#'
#' @return Invisibly returns a grob object created by `gridExtra::arrangeGrob()`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Single year
#' pyramids(country = "Honduras", years = 16)
#'
#' # Multiple years
#' pyramids(country = "Honduras", years = c(1, 5, 10, 15))
#'
#' # Save as PNG
#' pyramids(
#'   country = "Honduras",
#'   years = c(1, 5, 10, 15),
#'   save = TRUE
#' )
#'
#' # Save as PDF
#' pyramids(
#'   country = "Honduras",
#'   years = c(1, 5, 10, 15),
#'   save = TRUE,
#'   format = "pdf",
#'   file = "pyramids.pdf"
#' )
#'
#' # Save as SVG
#' pyramids(
#'   country = "Honduras",
#'   years = c(1, 5, 10, 15),
#'   save = TRUE,
#'   format = "svg",
#'   file = "pyramids.svg"
#' )
#' }
pyramids <- function(country,
                     years = 1:31,
                     language.en = TRUE,
                     color = c("#0C4A61", "#34B0AA"),
                     save = FALSE,
                     file = NULL,
                     format = c("png", "pdf", "svg"),
                     height = 5,
                     width = 7,
                     caption = TRUE,
                     progress = TRUE) {

  format <- match.arg(format)

  if (!is.character(country) || length(country) < 1 || anyNA(country)) {
    stop("country must be a non-missing character vector.", call. = FALSE)
  }

  if (!is.numeric(years) || any(is.na(years)) || any(years < 1) || any(years > 31)) {
    stop("years must be numeric values between 1 and 31.", call. = FALSE)
  }

  if (!is.logical(language.en) || length(language.en) != 1 || is.na(language.en)) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.character(color) || length(color) != 2 || anyNA(color)) {
    stop("color must be a character vector of length 2.", call. = FALSE)
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

  if (!is.logical(caption) || length(caption) != 1 || is.na(caption)) {
    stop("caption must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("progress must be TRUE or FALSE.", call. = FALSE)
  }

  if (progress) {
    if (isTRUE(language.en)) {
      message("Preparing population pyramids...")
    } else {
      message("Preparando pirámides poblacionales...")
    }
  }

  df <- call.data(
    id.indicator = 31,
    language.en = language.en,
    progress = progress
  )

  years_filter <- seq(1950, 2100, 5)[years]

  if (isTRUE(language.en)) {
    country_col <- "Country"
    year_col <- "Years"
    sex_col <- "Sex"
    age_col <- "Age__group (each five_year) (0_100 and over)"
    total_age <- "Total age"
    both_sexes <- "Both sexes"
    male_value <- "Men"
    female_value <- "Women"
    male_label <- "Men"
    female_label <- "Women"
    ages_labels <- c(
      "0 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24",
      "25 - 29", "30 - 34", "35 - 39", "40 - 44", "45 - 49",
      "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74",
      "75 - 79", "80 - 84", "85 - 89", "90 - 94", "95 - 99",
      "100 and over"
    )
    age_map <- c(
      "0_4" = 1, "5_9" = 2, "10_14" = 3, "15_19" = 4, "20_24" = 5,
      "25_29" = 6, "30_34" = 7, "35_39" = 8, "40_44" = 9, "45_49" = 10,
      "50_54" = 11, "55_59" = 12, "60_64" = 13, "65_69" = 14, "70_74" = 15,
      "75_79" = 16, "80_84" = 17, "85_89" = 18, "90_94" = 19, "95_99" = 20,
      "100 and over" = 21
    )
    subtitle_prefix <- "Year: "
    xlab <- "Thousands of people"
    ylab <- "Age"
    caption_text <- paste(
      "Source: CEPALSTAT (ECLAC).",
      "Population estimates and projections by five-year age groups, 1950–2100."
    )
    default_file <- paste0("Population_pyramids.", format)
  } else {
    country_col <- "País"
    year_col <- "Años"
    sex_col <- "Sexo"
    age_col <- "Edad__Grupos de edad (cada 5 años) (0_100 mas)"
    total_age <- "Total edades"
    both_sexes <- "Ambos sexos"
    male_value <- "Hombres"
    female_value <- "Mujeres"
    male_label <- "Hombres"
    female_label <- "Mujeres"
    ages_labels <- c(
      "0 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24",
      "25 - 29", "30 - 34", "35 - 39", "40 - 44", "45 - 49",
      "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74",
      "75 - 79", "80 - 84", "85 - 89", "90 - 94", "95 - 99",
      "100 +"
    )
    age_map <- c(
      "0_4" = 1, "5_9" = 2, "10_14" = 3, "15_19" = 4, "20_24" = 5,
      "25_29" = 6, "30_34" = 7, "35_39" = 8, "40_44" = 9, "45_49" = 10,
      "50_54" = 11, "55_59" = 12, "60_64" = 13, "65_69" = 14, "70_74" = 15,
      "75_79" = 16, "80_84" = 17, "85_89" = 18, "90_94" = 19, "95_99" = 20,
      "100 y más" = 21
    )
    subtitle_prefix <- "Año: "
    xlab <- "Miles de personas"
    ylab <- "Edad"
    caption_text <- paste(
      "Fuente: CEPALSTAT (CEPAL).",
      "Estimaciones y proyecciones de población por grupos quinquenales de edad, 1950–2100."
    )
    default_file <- paste0("Piramides_poblacionales.", format)
  }

  required_cols <- c(country_col, year_col, sex_col, age_col, "value")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  if (progress) {
    if (isTRUE(language.en)) {
      message("Filtering and transforming data...")
    } else {
      message("Filtrando y transformando datos...")
    }
  }

  df <- df |>
    dplyr::filter(
      .data[[country_col]] %in% country,
      .data[[year_col]] %in% years_filter,
      .data[[age_col]] != total_age,
      .data[[sex_col]] != both_sexes
    ) |>
    dplyr::mutate(
      Edad_num = unname(age_map[.data[[age_col]]]),
      Edad = factor(Edad_num, levels = 1:21, labels = ages_labels),
      Sex_plot = ifelse(.data[[sex_col]] == male_value, male_label, female_label),
      value = ifelse(.data[[sex_col]] == male_value, -1 * value, value)
    )

  if (nrow(df) == 0) {
    stop("No data available for the selected country and years.", call. = FALSE)
  }

  if (progress) {
    if (isTRUE(language.en)) {
      message("Creating plots...")
    } else {
      message("Creando gráficos...")
    }
  }

  plots <- vector("list", length(years_filter))

  for (i in seq_along(years_filter)) {
    data_i <- df[df[[year_col]] == years_filter[i], , drop = FALSE]

    if (nrow(data_i) == 0) next

    v <- round(c(
      min(data_i$value, na.rm = TRUE),
      min(data_i$value, na.rm = TRUE) / 2,
      0,
      max(data_i$value, na.rm = TRUE) / 2,
      max(data_i$value, na.rm = TRUE)
    ), 0)

    plots[[i]] <- ggplot2::ggplot(
      data_i,
      ggplot2::aes(x = Edad, y = value, fill = Sex_plot)
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_y_continuous(breaks = v, labels = abs(v)) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = color) +
      ggplot2::labs(
        subtitle = paste0(subtitle_prefix, years_filter[i]),
        y = xlab,
        x = ylab,
        fill = ""
      )
  }

  plots <- Filter(Negate(is.null), plots)

  if (length(plots) == 0) {
    stop("No plots could be created for the selected years.", call. = FALSE)
  }

  bottom_grob <- if (isTRUE(caption)) {
    grid::textGrob(
      caption_text,
      x = 0,
      hjust = 0,
      gp = grid::gpar(fontsize = 9)
    )
  } else {
    NULL
  }

  g <- gridExtra::arrangeGrob(
    grobs = plots,
    bottom = bottom_grob
  )

  grid::grid.newpage()
  grid::grid.draw(g)

  if (isTRUE(save)) {
    if (progress) {
      if (isTRUE(language.en)) {
        message("Saving output...")
      } else {
        message("Guardando salida...")
      }
    }

    if (is.null(file)) {
      file <- default_file
    }

    if (identical(format, "svg")) {
      if (!requireNamespace("svglite", quietly = TRUE)) {
        stop("Package 'svglite' is required for SVG output.", call. = FALSE)
      }
      svglite::svglite(filename = file, width = width, height = height)
      grid::grid.draw(g)
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
