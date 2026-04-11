pyramids <- function(country,
                     years = 1:31,
                     language.en = TRUE,
                     color = c('#B20B27', '#0A1873'),
                     save = FALSE,
                     file = NULL,
                     height = 5,
                     width = 7) {

  if (!is.character(country)) stop("country must be character.", call. = FALSE)
  if (!is.numeric(years)) stop("years must be numeric.", call. = FALSE)
  if (!is.logical(language.en)) stop("language.en must be TRUE/FALSE.", call. = FALSE)
  if (!is.logical(save)) stop("save must be TRUE/FALSE.", call. = FALSE)

  df <- call.data(id.indicator = 31, language.en = language.en)

  years_filter <- seq(1950, 2100, 5)[years]

  if (isTRUE(language.en)) {
    country_col <- "Country"
    year_col <- "Years"
    sex_col <- "Sex"
    age_col <- names(df)[grepl("Age__", names(df))]
    male <- "Men"
    female <- "Women"
    ylab <- "Thousands of people"
    xlab <- "Age"
    subtitle_txt <- "Year: "
  } else {
    country_col <- "País"
    year_col <- "Años"
    sex_col <- "Sexo"
    age_col <- names(df)[grepl("Edad__", names(df))]
    male <- "Hombres"
    female <- "Mujer"
    ylab <- "Miles de Personas"
    xlab <- "Edad"
    subtitle_txt <- "Año: "
  }

  df <- df |>
    dplyr::filter(
      .data[[country_col]] %in% country,
      .data[[year_col]] %in% years_filter,
      .data[[sex_col]] != "Both sexes"
    )

  df$value <- ifelse(df[[sex_col]] == male, -1 * df$value, df$value)

  plots <- lapply(years_filter, function(y) {

    data_y <- df[df[[year_col]] == y, ]

    ggplot2::ggplot(data_y, ggplot2::aes(x = .data[[age_col]], y = value, fill = .data[[sex_col]])) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = color) +
      ggplot2::labs(
        subtitle = paste0(subtitle_txt, y),
        y = ylab,
        x = xlab
      )
  })

  g <- do.call(gridExtra::grid.arrange, plots)

  if (isTRUE(save)) {
    if (is.null(file)) {
      file <- "population_pyramids.png"
    }
    ggplot2::ggsave(file, plot = g, height = height, width = width)
  }

  return(g)
}
