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

  # ===============================
  # 0. VALIDATION
  # ===============================
  if (!requireNamespace("collapsibleTree", quietly = TRUE)) {
    stop("Package 'collapsibleTree' is required.", call. = FALSE)
  }

  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required.", call. = FALSE)
  }

  if (!is.logical(language.en) || length(language.en) != 1 || is.na(language.en)) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(open.browser) || length(open.browser) != 1 || is.na(open.browser)) {
    stop("open.browser must be TRUE or FALSE.", call. = FALSE)
  }

  # ===============================
  # 1. LOAD DATA
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
    available_label <- "Number of indicators"
  } else {
    area_col <- "Área"
    dim_col  <- "Dimensión"
    sub_col  <- "Subdimensión"
    grp_col  <- "Grupo"
    name_col <- "Nombre Indicador"
    id_col   <- "ID Indicador"

    subtitle <- "Portal de Datos y Publicaciones Estadísticas"
    footer_text <- "Mapa temático interactivo de indicadores"
    available_label <- "Cantidad de indicadores"
  }

  required_cols <- c(area_col, dim_col, sub_col, grp_col, name_col, id_col)
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(
      paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
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
  # 4. LOAD IMAGES (CORRECTED)
  # ===============================
  pkg_logo_path <- system.file(
    "CEPALSTAT_images",
    "CepalStatR_icon.png",
    package = "CepalStatR"
  )

  inst_logo_path <- if (isTRUE(language.en)) {
    system.file("CEPALSTAT_images", "eclac_logo_en.png", package = "CepalStatR")
  } else {
    system.file("CEPALSTAT_images", "eclac_logo_es.png", package = "CepalStatR")
  }

  if (!nzchar(pkg_logo_path)) {
    stop("Package logo not found in inst/CEPALSTAT_images.", call. = FALSE)
  }

  if (!nzchar(inst_logo_path)) {
    stop("Institutional logo not found in inst/CEPALSTAT_images.", call. = FALSE)
  }

  pkg_logo <- img_to_data_uri(pkg_logo_path)
  inst_logo <- img_to_data_uri(inst_logo_path)

  # ===============================
  # 5. CSS (REUSED)
  # ===============================
  responsive_style <- viewer_indicators_style()

  # ===============================
  # 6. HEADER (MATCH viewer.indicators)
  # ===============================
  header_block <- htmltools::tags$div(
    class = "cepal-header",

    htmltools::tags$div(
      class = "cepal-left",

      htmltools::tags$img(
        src = pkg_logo,
        class = "cepal-pkg-logo"
      ),

      htmltools::tags$div(
        "CepalStatR",
        class = "cepal-pkg-text"
      ),

      htmltools::tags$div(
        paste0(available_label, ": ", format(total_indicators, big.mark = ",")),
        class = "cepal-pkg-count"
      )
    ),

    htmltools::tags$div(
      class = "cepal-right",

      htmltools::tags$div(
        class = "cepal-right-top",

        htmltools::tags$img(
          src = inst_logo,
          class = "cepal-inst-logo"
        ),

        htmltools::tags$div(
          htmltools::tags$div(
            "CEPALSTAT",
            class = "cepal-main-title"
          ),

          htmltools::tags$div(
            subtitle,
            class = "cepal-subtitle"
          )
        )
      )
    )
  )

  # ===============================
  # 7. TREE
  # ===============================
  tree <- collapsibleTree::collapsibleTree(
    df,
    hierarchy = c(area_col, dim_col, sub_col, grp_col, name_col, id_col),
    root = "CEPALSTAT",
    collapsed = TRUE,
    zoomable = TRUE
  )

  # ===============================
  # 8. FOOTER (MATCH viewer + EXTRA)
  # ===============================
  footer_block <- htmltools::tags$div(
    style = paste0(
      "padding:10px 22px 16px 22px;",
      "font-size:11.5px;",
      "color:#6B7280;",
      "background:#FFFFFF;"
    ),

    htmltools::tags$div(footer_text),

    if (isTRUE(language.en)) {
      htmltools::HTML(
        'For more information about CEPALSTAT, visit <a href="https://statistics.cepal.org/portal/cepalstat/" target="_blank">https://statistics.cepal.org/portal/cepalstat/</a>'
      )
    } else {
      htmltools::HTML(
        'Para más información de CEPALSTAT accede a <a href="https://statistics.cepal.org/portal/cepalstat/" target="_blank">https://statistics.cepal.org/portal/cepalstat/</a>'
      )
    }
  )

  # ===============================
  # 9. COMBINE
  # ===============================
  out <- htmltools::browsable(
    htmltools::tagList(
      responsive_style,
      header_block,
      tree,
      footer_block
    )
  )

  # ===============================
  # 10. OPEN BROWSER
  # ===============================
  if (isTRUE(open.browser)) {
    tmp <- tempfile(fileext = ".html")
    htmltools::save_html(out, tmp)
    utils::browseURL(tmp)
  }

  return(out)
}
