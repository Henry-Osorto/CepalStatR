#' CEPALSTAT Thematic Map (Interactive Tree)
#'
#' @description
#' Creates an interactive thematic tree of CEPALSTAT indicators with
#' the same visual structure as viewer.indicators().
#'
#' @param language.en Logical. TRUE for English, FALSE for Spanish.
#' @param open.browser Logical. Open in browser (default FALSE).
#'
#' @return HTML widget
#' @export
cepal_topic_map <- function(language.en = TRUE,
                            open.browser = FALSE) {

  if (!requireNamespace("collapsibleTree", quietly = TRUE)) {
    stop("Package 'collapsibleTree' is required.", call. = FALSE)
  }

  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required.", call. = FALSE)
  }

  # ===============================
  # 1. DATA
  # ===============================
  df <- call.indicators(language.en = language.en, progress = FALSE)

  # ===============================
  # 2. COLUMN DETECTION
  # ===============================
  if (isTRUE(language.en)) {
    area_col <- "Area"
    dim_col  <- "Dimension"
    sub_col  <- "Subdimension"
    grp_col  <- "Group"
    name_col <- "Indicator Name"
    id_col   <- "Indicator ID"

    subtitle <- "Statistical Data Portal and Publications"
    footer_text <- "Interactive thematic map of indicators"
    url_text <- "For more information visit:"
  } else {
    area_col <- "Área"
    dim_col  <- "Dimensión"
    sub_col  <- "Subdimensión"
    grp_col  <- "Grupo"
    name_col <- "Nombre Indicador"
    id_col   <- "ID Indicador"

    subtitle <- "Portal de Datos y Publicaciones Estadísticas"
    footer_text <- "Mapa temático interactivo de indicadores"
    url_text <- "Para más información visita:"
  }

  # ===============================
  # 3. PREP DATA
  # ===============================
  df <- df |>
    dplyr::mutate(
      Indicator_label = paste0(.data[[name_col]], " (ID: ", .data[[id_col]], ")")
    )

  total_indicators <- length(unique(df[[id_col]]))

  # ===============================
  # 4. LOAD IMAGES (helpers)
  # ===============================
  pkg_path <- system.file("CEPALSTAT_images", package = "CepalStatR")

  logo_pkg <- img_to_data_uri(file.path(pkg_path, "CepalStatR_icon.png"))

  logo_eclac <- if (isTRUE(language.en)) {
    img_to_data_uri(file.path(pkg_path, "eclac_logo_en.png"))
  } else {
    img_to_data_uri(file.path(pkg_path, "eclac_logo_es.png"))
  }

  # ===============================
  # 5. HEADER (igual viewer)
  # ===============================
  header <- htmltools::tags$div(
    style = "
      display:flex;
      justify-content:space-between;
      align-items:center;
      flex-wrap:wrap;
      margin-bottom:15px;
    ",

    # LEFT
    htmltools::tags$div(
      style = "display:flex; flex-direction:column; align-items:flex-start;",

      htmltools::tags$img(
        src = logo_pkg,
        style = "height:45px; margin-bottom:5px;"
      ),

      htmltools::tags$span(
        style = "font-size:12px; color:#374151;",
        paste0(
          if (isTRUE(language.en))
            "Number of indicators: "
          else
            "Cantidad de indicadores: ",
          format(total_indicators, big.mark = ",")
        )
      )
    ),

    # RIGHT
    htmltools::tags$div(
      style = "display:flex; align-items:center; gap:10px;",

      htmltools::tags$img(
        src = logo_eclac,
        style = "height:55px;"
      ),

      htmltools::tags$div(
        style = "text-align:left;",

        htmltools::tags$div(
          style = "font-size:16px; font-weight:bold;",
          "CEPALSTAT"
        ),

        htmltools::tags$div(
          style = "font-size:12px;",
          subtitle
        )
      )
    )
  )

  # ===============================
  # 6. TREE
  # ===============================
  tree <- collapsibleTree::collapsibleTree(
    df,
    hierarchy = c(area_col, dim_col, sub_col, grp_col, "Indicator_label"),
    root = "CEPALSTAT",
    collapsed = TRUE,
    zoomable = TRUE
  )

  # ===============================
  # 7. FOOTER (igual viewer + extra)
  # ===============================
  footer <- htmltools::tags$div(
    style = "
      display:flex;
      justify-content:space-between;
      flex-wrap:wrap;
      margin-top:15px;
      font-size:12px;
      color:#4B5563;
    ",

    # LEFT vacío (puedes usarlo luego)
    htmltools::tags$div(),

    # RIGHT
    htmltools::tags$div(
      style = "text-align:right;",

      htmltools::tags$div(footer_text),

      htmltools::tags$div(
        paste0(
          url_text, " ",
          htmltools::tags$a(
            href = "https://statistics.cepal.org/portal/cepalstat/",
            "https://statistics.cepal.org/portal/cepalstat/",
            target = "_blank"
          )
        )
      )
    )
  )

  # ===============================
  # 8. COMBINE
  # ===============================
  out <- htmltools::browsable(
    htmltools::tagList(
      header,
      tree,
      footer
    )
  )

  # ===============================
  # 9. OPEN BROWSER
  # ===============================
  if (isTRUE(open.browser)) {
    tmp <- tempfile(fileext = ".html")
    htmltools::save_html(out, tmp)
    utils::browseURL(tmp)
  }

  return(invisible(out))
}
