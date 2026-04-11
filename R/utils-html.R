#' Convert an image file to a data URI
#' @param path Path to the image file.
#' @return A character string containing a data URI, or NULL if the file does not exist.
#' @keywords internal
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



#' CSS for viewer.indicators header layout
#' @return An htmltools style tag.
#' @keywords internal
viewer_indicators_style <- function() {
  htmltools::tags$style(
    paste(
      ".cepal-header {",
      "  display: flex;",
      "  justify-content: space-between;",
      "  align-items: flex-start;",
      "  gap: 24px;",
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
      "  min-width: 280px;",
      "  max-width: 40%;",
      "  align-items: flex-start;",
      "}",

      ".cepal-right {",
      "  min-width: 360px;",
      "  max-width: 52%;",
      "  align-items: flex-end;",
      "  text-align: right;",
      "}",

      ".cepal-right-top {",
      "  display: flex;",
      "  align-items: center;",
      "  justify-content: flex-end;",
      "  gap: 16px;",
      "}",

      ".cepal-pkg-logo {",
      "  max-height: 92px;",
      "  width: auto;",
      "  display: block;",
      "}",

      ".cepal-inst-logo {",
      "  max-height: 96px;",
      "  width: auto;",
      "  display: block;",
      "}",

      ".cepal-pkg-count {",
      "  margin-top: 8px;",
      "  font-size: 13px;",
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
      "  font-size: 46px;",
      "  font-weight: 700;",
      "  color: #0C4A61;",
      "  line-height: 1.0;",
      "  text-align: left;",
      "}",

      ".cepal-subtitle {",
      "  margin-top: 8px;",
      "  font-size: 17px;",
      "  font-weight: 400;",
      "  color: #34B0AA;",
      "  line-height: 1.2;",
      "  text-align: left;",
      "}",

      ".cepal-meta {",
      "  margin-top: 8px;",
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

      "@media (max-width: 1200px) {",
      "  .cepal-main-title { font-size: 38px; }",
      "  .cepal-subtitle { font-size: 15px; }",
      "  .cepal-pkg-logo { max-height: 84px; }",
      "  .cepal-inst-logo { max-height: 84px; }",
      "  .cepal-left { min-width: 240px; }",
      "  .cepal-right { min-width: 300px; }",
      "}",

      "@media (max-width: 900px) {",
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

      "  .cepal-pkg-logo { max-height: 78px; }",
      "  .cepal-inst-logo { max-height: 78px; }",
      "  .cepal-main-title { font-size: 32px; }",
      "  .cepal-subtitle { font-size: 14px; }",
      "}",

      sep = "\n"
    )
  )
}
