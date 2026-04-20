#' CEPALSTAT thematic map of indicators
#'
#' @description
#' Creates an interactive thematic tree of CEPALSTAT indicators using
#' `collapsibleTree`, with the same visual header and footer style as
#' `viewer.indicators()`.
#'
#' @param language.en Logical. If `TRUE` (default), English labels are used.
#' If `FALSE`, Spanish labels are used.
#' @param progress Logical. If `TRUE`, progress messages are shown.
#' @param open.browser Logical. If `TRUE`, the generated HTML is saved to a
#' temporary file and opened in the default web browser. Defaults to `FALSE`.
#'
#' @return A browsable HTML widget.
#' @export
#'
#' @examples
#' \dontrun{
#' cepal_topic_map()
#' cepal_topic_map(language.en = FALSE)
#' cepal_topic_map(open.browser = TRUE)
#' }
topic_map <- function(language.en = TRUE,
                            progress = TRUE,
                            open.browser = FALSE) {

  # ---- Validation ----
  if (!requireNamespace("collapsibleTree", quietly = TRUE)) {
    stop("Package 'collapsibleTree' is required.", call. = FALSE)
  }

  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required.", call. = FALSE)
  }

  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package 'htmlwidgets' is required.", call. = FALSE)
  }

  if (!is.logical(language.en) || length(language.en) != 1 || is.na(language.en)) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("progress must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(open.browser) || length(open.browser) != 1 || is.na(open.browser)) {
    stop("open.browser must be TRUE or FALSE.", call. = FALSE)
  }

  if (progress) {
    if (isTRUE(language.en)) {
      message("Preparing thematic map...")
    } else {
      message("Preparando mapa tem\u00e1tico...")
    }
  }

  # ---- Load indicators ----
  df <- call.indicators(language.en = language.en, progress = progress)

  if (!is.data.frame(df) || nrow(df) == 0) {
    stop("No indicators were returned by call.indicators().", call. = FALSE)
  }

  # ---- Column detection ----
  if (isTRUE(language.en)) {
    area_col <- "Area"
    dim_col  <- "Dimension"
    sub_col  <- "Subdimension"
    grp_col  <- "Group"
    name_col <- "Indicator Name"
    id_col   <- "Indicator ID"

    subtitle <- "Statistical Data Portal and Publications"
    package_desc <- "R interface to access, explore and visualize CEPALSTAT indicators"
    available_label <- "Available indicators"
    footer_extra <- "Interactive thematic map of indicators"
  } else {
    area_col <- "\u00c1rea"
    dim_col  <- "Dimensi\u00f3n"
    sub_col  <- "Subdimensi\u00f3n"
    grp_col  <- "Grupo"
    name_col <- "Nombre Indicador"
    id_col   <- "ID Indicador"

    subtitle <- "Portal de Datos y Publicaciones Estad\u00edsticas"
    package_desc <- "Interfaz en R para acceder, explorar y visualizar indicadores de CEPALSTAT"
    available_label <- "Indicadores disponibles"
    footer_extra <- "Mapa tem\u00e1tico interactivo de indicadores"
  }

  required_cols <- c(area_col, dim_col, sub_col, grp_col, name_col, id_col)
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(
      paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  # ---- Count indicators ----
  n_indicators <- length(unique(stats::na.omit(df[[id_col]])))

  # ---- Load images exactly as in viewer.indicators ----
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
    stop("Package logo 'CepalStatR_icon.png' was not found in inst/CEPALSTAT_images.", call. = FALSE)
  }

  if (!nzchar(inst_logo_path)) {
    stop("Institutional logo was not found in inst/CEPALSTAT_images.", call. = FALSE)
  }

  pkg_logo <- img_to_data_uri(pkg_logo_path)
  inst_logo <- img_to_data_uri(inst_logo_path)

  if (is.null(pkg_logo)) {
    stop("Package logo could not be encoded as data URI.", call. = FALSE)
  }

  if (is.null(inst_logo)) {
    stop("Institutional logo could not be encoded as data URI.", call. = FALSE)
  }

  # ---- Reuse CSS helper ----
  responsive_style <- viewer_indicators_style()

  # ---- Header copied from viewer.indicators ----
  header_block <- htmltools::tags$div(
    class = "cepal-header",

    htmltools::tags$div(
      class = "cepal-left",
      htmltools::tags$img(
        src = pkg_logo,
        class = "cepal-pkg-logo"
      ),
      htmltools::tags$div(
        package_desc,
        class = "cepal-pkg-text"
      ),
      htmltools::tags$div(
        paste0(available_label, ": ", format(n_indicators, big.mark = ",")),
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
      ),
      htmltools::tags$div(
        if (isTRUE(language.en)) {
          htmltools::HTML(
            'For more information about CEPALSTAT, visit <a href="https://statistics.cepal.org/portal/cepalstat/" target="_blank">https://statistics.cepal.org/portal/cepalstat/</a>'
          )
        } else {
          htmltools::HTML(
            'Para m\u00e1s informaci\u00f3n de CEPALSTAT accede a <a href="https://statistics.cepal.org/portal/cepalstat/" target="_blank">https://statistics.cepal.org/portal/cepalstat/</a>'
          )
        },
        class = "cepal-meta"
      )
    )
  )

  # ---- Keep only requested columns for tree ----
  df_tree <- df[, c(area_col, dim_col, sub_col, grp_col, name_col, id_col), drop = FALSE]

  # ---- Build tooltip text for final node hover ----
  # Uses metadata columns if they exist; otherwise falls back to name + id.
  meta_candidates <- if (isTRUE(language.en)) {
    c("unit", "definition", "comments", "theme", "area_meta", "last_update")
  } else {
    c("unidad", "definicion", "comentarios", "tema", "area_meta", "ultima_actualizacion")
  }

  present_meta <- intersect(meta_candidates, names(df))

  tooltip_df <- data.frame(
    id = as.character(df[[id_col]]),
    tooltip = NA_character_,
    stringsAsFactors = FALSE
  )

  if (length(present_meta) == 0) {
    tooltip_df$tooltip <- paste0(
      df[[name_col]], "\n",
      "ID: ", df[[id_col]]
    )
  } else {
    tooltip_df$tooltip <- apply(
      cbind(
        as.character(df[[name_col]]),
        paste0("ID: ", df[[id_col]]),
        sapply(present_meta, function(col) {
          paste0(col, ": ", df[[col]])
        })
      ),
      1,
      function(x) paste(stats::na.omit(x[x != "" & !is.na(x)]), collapse = "\n")
    )
  }

  # keep one tooltip per indicator id
  tooltip_df <- tooltip_df[!duplicated(tooltip_df$id), , drop = FALSE]

  # ---- Tree ----
  if (progress) {
    if (isTRUE(language.en)) {
      message("Building interactive tree...")
    } else {
      message("Construyendo \u00e1rbol interactivo...")
    }
  }

  tree <- collapsibleTree::collapsibleTree(
    df_tree,
    hierarchy = c(area_col, dim_col, sub_col, grp_col, name_col, id_col),
    root = "CEPALSTAT",
    collapsed = TRUE,
    zoomable = TRUE
  )

  # ---- Add hover tooltips to leaf nodes (final indicator id nodes) ----
  # Tooltip appears on hover using the browser-native title attribute.
  tree <- htmlwidgets::onRender(
    tree,
    "
    function(el, x, data) {
      var tooltipMap = {};
      data.forEach(function(d) {
        tooltipMap[String(d.id)] = d.tooltip;
      });

      function nodeName(d) {
        if (d && d.data && d.data.name !== undefined) return String(d.data.name);
        if (d && d.name !== undefined) return String(d.name);
        return null;
      }

      function isLeaf(d) {
        return !(d.children && d.children.length) && !(d._children && d._children.length);
      }

      d3.select(el).selectAll('g.node').each(function(d) {
        var nm = nodeName(d);
        if (nm !== null && isLeaf(d) && tooltipMap[nm]) {
          d3.select(this).attr('title', tooltipMap[nm]);
        }
      });
    }
    ",
    data = tooltip_df
  )

  # ---- Footer aligned with viewer + extra legend ----
  footer_block <- htmltools::tags$div(
    style = paste0(
      "padding:10px 22px 16px 22px;",
      "font-size:11.5px;",
      "color:#6B7280;",
      "background:#FFFFFF;"
    ),
    if (isTRUE(language.en)) {
      htmltools::tagList(
        htmltools::tags$div("Generated with CepalStatR"),
        htmltools::tags$div(footer_extra)
      )
    } else {
      htmltools::tagList(
        htmltools::tags$div("Generado con CepalStatR"),
        htmltools::tags$div(footer_extra)
      )
    }
  )

  # ---- Container ----
  container <- htmltools::tags$div(
    style = paste0(
      "max-width:100%;",
      "margin:0 auto;",
      "background:#FFFFFF;",
      "border:1px solid #E5E7EB;",
      "border-radius:14px;",
      "overflow:hidden;",
      "box-shadow:0 6px 20px rgba(12,74,97,0.08);"
    ),
    responsive_style,
    header_block,
    htmltools::tags$div(
      style = "padding:14px 18px 10px 18px; background:#FFFFFF;",
      tree
    ),
    footer_block
  )

  out <- htmltools::browsable(container)

  if (isTRUE(open.browser)) {
    tmp <- tempfile(fileext = ".html")
    htmltools::save_html(out, file = tmp)
    utils::browseURL(tmp)
  } else {
    if (interactive()) {
      message(
        if (isTRUE(language.en)) {
          "Rendering in Viewer pane. Use open.browser = TRUE to open in browser."
        } else {
          "Mostrando en el Viewer. Usa open.browser = TRUE para abrir en el navegador."
        }
      )
    }
  }

  return(out)
}
