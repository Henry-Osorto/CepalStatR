#' Viewer for CEPALSTAT indicators
#'
#' @description
#' Displays an interactive HTML table for browsing the hierarchical structure
#' of CEPALSTAT indicators using `call.indicators()` as backend.
#'
#' @param language.en Logical. If `TRUE` (default), the viewer uses English labels.
#' If `FALSE`, Spanish labels are used.
#' @param progress Logical. If `TRUE`, progress messages are shown.
#' @param show_search Logical. If `TRUE`, enables global search in the table.
#' @param striped Logical. If `TRUE`, striped rows are shown.
#' @param bordered Logical. If `TRUE`, table borders are shown.
#' @param compact Logical. If `TRUE`, reduces row padding.
#' @param highlight Logical. If `TRUE`, highlights rows on hover.
#' @param full_width Logical. If `TRUE`, table uses full available width.
#' @param page_size Integer. Number of rows to show per page.
#' @param output Character. One of `"viewer"` or `"html"`.
#'
#' @return A browsable HTML object containing the header and interactive table.
#' @export
#'
#' @examples
#' \dontrun{
#' viewer.indicators()
#' viewer.indicators(language.en = FALSE)
#' }
viewer.indicators <- function(language.en = TRUE,
                              progress = TRUE,
                              show_search = TRUE,
                              striped = TRUE,
                              bordered = FALSE,
                              compact = FALSE,
                              highlight = TRUE,
                              full_width = TRUE,
                              page_size = 15,
                              open.browser = TRUE,
                              output = c("viewer", "html")) {

  output <- match.arg(output)

  if (!is.logical(language.en) || length(language.en) != 1 || is.na(language.en)) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("progress must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(show_search) || length(show_search) != 1 || is.na(show_search)) {
    stop("show_search must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(striped) || length(striped) != 1 || is.na(striped)) {
    stop("striped must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(bordered) || length(bordered) != 1 || is.na(bordered)) {
    stop("bordered must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(compact) || length(compact) != 1 || is.na(compact)) {
    stop("compact must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(highlight) || length(highlight) != 1 || is.na(highlight)) {
    stop("highlight must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(full_width) || length(full_width) != 1 || is.na(full_width)) {
    stop("full_width must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(page_size) || length(page_size) != 1 || is.na(page_size) || page_size <= 0) {
    stop("page_size must be a positive number.", call. = FALSE)
  }

  if (!is.logical(open.browser) || length(open.browser) != 1 || is.na(open.browser)) {
    stop("open.browser must be TRUE or FALSE.", call. = FALSE)
  }

  img_to_data_uri <- function(path) {
    if (!nzchar(path) || !file.exists(path)) {
      return(NULL)
    }

    ext <- tolower(tools::file_ext(path))
    mime <- switch(
      ext,
      png = "image/png",
      jpg = "image/jpeg",
      jpeg = "image/jpeg",
      svg = "image/svg+xml",
      "application/octet-stream"
    )

    base64enc::dataURI(file = path, mime = mime)
  }

  if (progress) {
    if (isTRUE(language.en)) {
      message("Preparing indicator viewer...")
    } else {
      message("Preparando visor de indicadores...")
    }
  }

  df <- call.indicators(language.en = language.en, progress = progress)

  if (!is.data.frame(df) || nrow(df) == 0) {
    stop("No indicators were returned by call.indicators().", call. = FALSE)
  }

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

  subtitle <- if (isTRUE(language.en)) {
    "Statistical Data Portal and Publications"
  } else {
    "Portal de Datos y Publicaciones Estadísticas"
  }

  package_desc <- if (isTRUE(language.en)) {
    "R interface for accessing, exploring and visualizing CEPALSTAT indicators"
  } else {
    "Interfaz en R para acceder, explorar y visualizar indicadores de CEPALSTAT"
  }

  search_label <- if (isTRUE(language.en)) "Search" else "Buscar"
  no_data_label <- if (isTRUE(language.en)) "No rows found" else "No se encontraron registros"
  page_info_text <- if (isTRUE(language.en)) {
    "{rowStart}–{rowEnd} of {rows} rows"
  } else {
    "{rowStart}–{rowEnd} de {rows} registros"
  }
  page_numbers_text <- if (isTRUE(language.en)) {
    "{page} of {pages}"
  } else {
    "{page} de {pages}"
  }
  page_size_text <- if (isTRUE(language.en)) {
    "Show {rows}"
  } else {
    "Mostrar {rows}"
  }

  available_label <- if (isTRUE(language.en)) {
    "Available indicators"
  } else {
    "Indicadores disponibles"
  }

  col_names <- names(df)

  fixed_cols <- col_names[col_names %in% c(
    "Area", "Dimension", "Subdimension",
    "Área", "Dimensión", "Subdimensión"
  )]

  id_col <- col_names[col_names %in% c("Indicator ID", "Id del Indicador", "ID Indicador")]
  name_col <- col_names[col_names %in% c("Indicator Name", "Nombre Indicador")]

  n_indicators <- if (length(id_col) == 1) {
    length(unique(stats::na.omit(df[[id_col]])))
  } else {
    nrow(df)
  }

  if (length(id_col) == 1) {
    df[[id_col]] <- suppressWarnings(as.character(df[[id_col]]))
  }

  default_coldef <- reactable::colDef(
    minWidth = 140,
    align = "left",
    sortable = TRUE,
    headerStyle = list(
      background = "#0C4A61",
      color = "white",
      fontWeight = "600",
      fontSize = "13px",
      borderColor = "#0C4A61"
    ),
    style = list(
      fontSize = "12.5px",
      whiteSpace = "normal"
    )
  )

  columns_list <- stats::setNames(
    lapply(col_names, function(x) default_coldef),
    col_names
  )

  if (length(fixed_cols) > 0) {
    for (nm in fixed_cols) {
      columns_list[[nm]] <- reactable::colDef(
        minWidth = 150,
        sticky = "left",
        rowHeader = identical(nm, fixed_cols[1]),
        headerStyle = list(
          background = "#0C4A61",
          color = "white",
          fontWeight = "700",
          fontSize = "13px",
          borderColor = "#0C4A61",
          zIndex = 2
        ),
        style = list(
          background = "#FFFFFF",
          fontWeight = "500",
          fontSize = "12.5px"
        )
      )
    }
  }

  if (length(id_col) == 1) {
    columns_list[[id_col]] <- reactable::colDef(
      minWidth = 120,
      align = "center",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      headerStyle = list(
        background = "#0C4A61",
        color = "white",
        fontWeight = "600",
        fontSize = "13px",
        borderColor = "#0C4A61"
      ),
      style = list(
        fontSize = "12.5px",
        fontWeight = "500"
      )
    )
  }

  if (length(name_col) == 1) {
    columns_list[[name_col]] <- reactable::colDef(
      minWidth = 280,
      headerStyle = list(
        background = "#0C4A61",
        color = "white",
        fontWeight = "700",
        fontSize = "13px",
        borderColor = "#0C4A61"
      ),
      style = list(
        fontSize = "12.5px",
        fontWeight = "500"
      )
    )
  }

  responsive_style <- htmltools::tags$style(
    paste(
      ".cepal-header {",
      "  display: flex;",
      "  justify-content: space-between;",
      "  align-items: flex-start;",
      "  gap: 28px;",
      "  padding: 18px 24px 16px 24px;",
      "  border-top: 4px solid #0C4A61;",
      "  border-bottom: 3px solid #34B0AA;",
      "  background: #FFFFFF;",
      "}",

      ".cepal-left,",
      ".cepal-right {",
      "  display: flex;",
      "  flex-direction: column;",
      "}",

      ".cepal-left {",
      "  min-width: 320px;",
      "  max-width: 40%;",
      "  align-items: flex-start;",
      "}",

      ".cepal-right {",
      "  min-width: 500px;",
      "  max-width: 52%;",
      "  align-items: flex-end;",
      "  text-align: right;",
      "}",

      ".cepal-right-top {",
      "  display: flex;",
      "  align-items: center;",
      "  justify-content: flex-end;",
      "  gap: 18px;",
      "}",

      ".cepal-pkg-logo {",
      "  max-height: 100px;",
      "  width: auto;",
      "  display: block;",
      "}",

      ".cepal-inst-logo {",
      "  max-height: 120px;",
      "  width: auto;",
      "  display: block;",
      "}",

      ".cepal-pkg-count {",
      "  margin-top: 8px;",
      "  font-size: 15px;",
      "  color: #6B7280;",
      "  font-weight: 500;",
      "  text-align: left;",
      "}",

      ".cepal-pkg-text {",
      "  margin-top: 8px;",
      "  font-family: Inter, 'Segoe UI', Arial, sans-serif;",
      "  font-size: 14px;",
      "  font-weight: 500;",
      "  color: #0C4A61;",
      "  line-height: 1.35;",
      "  text-align: left;",
      "}",

      ".cepal-main-title {",
      "  font-family: 'Arial Narrow', 'Helvetica Neue', Arial, sans-serif;",
      "  font-size: 60px;",
      "  font-weight: 700;",
      "  color: #0C4A61;",
      "  line-height: 1.0;",
      "  text-align: left;",
      "}",

      ".cepal-subtitle {",
      "  margin-top: 12px;",
      "  font-size: 20px;",
      "  font-weight: 400;",
      "  color: #34B0AA;",
      "  line-height: 1.2;",
      "  text-align: left;",
      "}",

      ".cepal-meta {",
      "  margin-top: 2px;",
      "  font-size: 11.5px;",
      "  color: #6B7280;",
      "  line-height: 1.4;",
      "  text-align: right;",
      "}",

      ".cepal-meta a {",
      "  color: #0C4A61;",
      "  text-decoration: none;",
      "}",

      ".cepal-meta a:hover {",
      "  text-decoration: underline;",
      "}",

      "@media (max-width: 1100px) {",
      "  .cepal-header {",
      "    flex-direction: column;",
      "    align-items: center;",
      "    gap: 18px;",
      "  }",

      "  .cepal-left,",
      "  .cepal-right {",
      "    min-width: 100%;",
      "    max-width: 100%;",
      "    align-items: center;",
      "    text-align: center;",
      "  }",

      "  .cepal-right-top {",
      "    justify-content: center;",
      "  }",

      "  .cepal-main-title,",
      "  .cepal-subtitle,",
      "  .cepal-pkg-text,",
      "  .cepal-meta,",
      "  .cepal-pkg-count {",
      "    text-align: center;",
      "  }",

      "  .cepal-pkg-logo {",
      "    max-height: 90px;",
      "  }",

      "  .cepal-inst-logo {",
      "    max-height: 86px;",
      "  }",

      "  .cepal-main-title {",
      "    font-size: 34px;",
      "  }",

      "  .cepal-subtitle {",
      "    font-size: 15px;",
      "  }",
      "}",
      sep = "\n"
    )
  )

  header_block <- htmltools::tags$div(
    class = "cepal-header",

    # Left block: package logo + available indicators
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

    # Center block: package description
    #htmltools::tags$div(
    #  style = paste0(
    #    "flex:1;",
    #    "text-align:center;",
    #   "padding:0 16px;",
    #   "align-self:center;"
    # ),
    # htmltools::tags$div(
    #   package_desc,
    #   class = "cepal-pkg-text"
    # )
    # ),

    # Right block: eclac logo + CEPALSTAT + subtitle + url
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
            'Para más información de CEPALSTAT accede a <a href="https://statistics.cepal.org/portal/cepalstat/" target="_blank">https://statistics.cepal.org/portal/cepalstat/</a>'
          )
        },
        class = "cepal-meta"
      )
    )
  )

  table_block <- reactable::reactable(
    df,
    searchable = show_search,
    filterable = FALSE,
    striped = striped,
    bordered = bordered,
    highlight = highlight,
    compact = compact,
    fullWidth = full_width,
    pagination = TRUE,
    defaultPageSize = as.integer(page_size),
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 15, 25, 50, 100),
    resizable = TRUE,
    columns = columns_list,
    defaultColDef = reactable::colDef(sortable = TRUE),
    outlined = FALSE,
    wrap = TRUE,
    showSortIcon = TRUE,
    showSortable = TRUE,
    selection = NULL,
    paginationType = "jump",
    theme = reactable::reactableTheme(
      color = "#1F2937",
      backgroundColor = "#FFFFFF",
      borderColor = "#E5E7EB",
      stripedColor = "#F8FAFC",
      highlightColor = "#EEF7F8",
      cellPadding = if (compact) "6px 8px" else "9px 10px",
      style = list(
        fontFamily = "Inter, Segoe UI, Arial, sans-serif",
        fontSize = "12.5px"
      ),
      headerStyle = list(
        fontWeight = "600"
      ),
      searchInputStyle = list(
        border = "1px solid #CBD5E1",
        borderRadius = "8px",
        padding = "8px 10px",
        fontSize = "12.5px",
        width = "320px"
      ),
      pageButtonStyle = list(
        borderRadius = "8px",
        border = "1px solid #CBD5E1",
        padding = "6px 10px",
        background = "white"
      ),
      pageButtonHoverStyle = list(
        background = "#F1F5F9"
      ),
      pageButtonActiveStyle = list(
        background = "#0C4A61",
        color = "white",
        border = "1px solid #0C4A61"
      ),
      pageButtonCurrentStyle = list(
        background = "#0C4A61",
        color = "white",
        border = "1px solid #0C4A61"
      )
    ),
    language = reactable::reactableLang(
      searchPlaceholder = paste0(search_label, "..."),
      searchLabel = search_label,
      noData = no_data_label,
      pageNext = if (isTRUE(language.en)) "Next" else "Siguiente",
      pagePrevious = if (isTRUE(language.en)) "Previous" else "Anterior",
      pageNumbers = page_numbers_text,
      pageInfo = page_info_text,
      pageSizeOptions = page_size_text,
      pageNextLabel = if (isTRUE(language.en)) "Next page" else "Página siguiente",
      pagePreviousLabel = if (isTRUE(language.en)) "Previous page" else "Página anterior",
      pageJumpLabel = if (isTRUE(language.en)) "Go to page" else "Ir a la página",
      pageSizeOptionsLabel = if (isTRUE(language.en)) "Rows per page" else "Filas por página"
    )
  )

  footer_block <- htmltools::tags$div(
    style = paste0(
      "padding:10px 22px 16px 22px;",
      "font-size:11.5px;",
      "color:#6B7280;",
      "background:#FFFFFF;"
    ),
    if (isTRUE(language.en)) {
      "Generated with CepalStatR"
    } else {
      "Generado con CepalStatR"
    }
  )

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
      table_block
    ),
    footer_block
  )

  out <- htmltools::browsable(container)

  if (isTRUE(open.browser)) {
    tmp <- tempfile(fileext = ".html")
    htmltools::save_html(out, file = tmp)
    utils::browseURL(tmp)
  }

  if (output == "viewer") {
    return(out)
  }

  out
}
