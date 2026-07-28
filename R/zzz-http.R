#' Internal helper



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
