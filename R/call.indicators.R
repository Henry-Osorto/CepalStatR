#' Call indicators from CEPALSTAT
#'
#' @description
#' Retrieves the CEPALSTAT thematic tree and returns a data frame with the
#' hierarchical structure of areas, dimensions, subdimensions, indicators,
#' and final indicator IDs.
#'
#' @param language.en Logical. If `TRUE` (default), results are returned with
#' English labels. If `FALSE`, results are returned with Spanish labels.
#' @param progress Logical. If `TRUE`, status messages are displayed during
#' download and processing.
#'
#' @return A data frame containing the hierarchical thematic structure of
#' CEPALSTAT indicators and their final indicator IDs.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#'
#' @examples
#' data.indicators <- call.indicators()
#' data.indicators <- call.indicators(language.en = FALSE)
#'
call.indicators <- function(language.en = TRUE, progress = TRUE) {

  # 1. Validate arguments ----
  if (!is.logical(language.en) || length(language.en) != 1 || is.na(language.en)) {
    stop("language.en must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("progress must be TRUE or FALSE.", call. = FALSE)
  }

  # 2. Internal helpers ----

  # Robust CSV downloader
  get_thematic_tree <- function(url) {
    req <- httr2::request(url) |>
      httr2::req_user_agent(
        "CepalStatR (https://github.com/Henry-Osorto/CepalStatR)"
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(
        max_tries = 5,
        retry_on_failure = TRUE,
        is_transient = function(resp) {
          httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
        },
        backoff = function(tries) min(60, 2^tries)
      )

    resp <- try(httr2::req_perform(req), silent = TRUE)

    if (inherits(resp, "try-error")) {
      stop("Could not connect to the CEPALSTAT API.", call. = FALSE)
    }

    status <- httr2::resp_status(resp)

    if (status >= 400) {
      stop(
        paste0("The CEPALSTAT API returned HTTP error ", status, "."),
        call. = FALSE
      )
    }

    out <- try(
      utils::read.csv(text = httr2::resp_body_string(resp), stringsAsFactors = FALSE),
      silent = TRUE
    )

    if (inherits(out, "try-error") || !is.data.frame(out)) {
      stop("The thematic tree could not be parsed as a CSV file.", call. = FALSE)
    }

    out
  }

  # Build hierarchy preserving original logic
  build_indicator_table <- function(data, language.en = TRUE) {

    required_cols <- c("indentation", "id", "name", "parent", "area_id")
    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
      stop(
        paste0(
          "The CEPALSTAT response does not contain the expected structure. Missing columns: ",
          paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (nrow(data) == 0) {
      stop("No indicators were returned by the CEPALSTAT API.", call. = FALSE)
    }

    # optional structural warning
    if (!all(1:6 %in% unique(data$indentation))) {
      warning("The thematic tree structure may have changed.", call. = FALSE)
    }

    # 3. Create subsets by hierarchy level ----
    n.1 <- data %>% dplyr::filter(indentation == 6)
    n.2 <- data %>% dplyr::filter(indentation == 5)
    n.3 <- data %>% dplyr::filter(indentation == 4)
    n.4 <- data %>% dplyr::filter(indentation == 3)
    n.5 <- data %>% dplyr::filter(indentation == 2)
    n.6 <- data %>% dplyr::filter(indentation == 1)

    # 4. Join subsets ----
    n.1 <- n.1 %>%
      dplyr::select(id, name, parent)

    n.2 <- n.2 %>%
      dplyr::left_join(n.1, by = c("area_id" = "parent")) %>%
      dplyr::select(id.x, name.x, parent, id.y, name.y)

    n.3 <- n.3 %>%
      dplyr::left_join(n.2, by = c("area_id" = "parent")) %>%
      dplyr::select(id, name, parent, id.x, name.x, id.y, name.y)

    n.4 <- n.4 %>%
      dplyr::left_join(n.3, by = c("area_id" = "parent")) %>%
      dplyr::select(id.x.x, name.x.x, parent, id.y, name.y, id.x, name.x, id.y.y, name.y.y)

    n.5 <- n.5 %>%
      dplyr::left_join(n.4, by = c("area_id" = "parent")) %>%
      dplyr::select(id, name, parent, id.x.x, name.x.x, id.y, name.y, id.x, name.x, id.y.y, name.y.y)

    n.6 <- n.6 %>%
      dplyr::left_join(n.5, by = c("area_id" = "parent"))

    # 5. Final names according to language ----
    if (isTRUE(language.en)) {
      area_nm         <- "Area"
      dimension_nm    <- "Dimension"
      subdimension_nm <- "Subdimension"
      ind1_nm         <- "Indicator.1"
      ind2_nm         <- "Indicator.2"
      ind3_nm         <- "Indicator.3"
      id_nm           <- "Indicator ID"
    } else {
      area_nm         <- "Área"
      dimension_nm    <- "Dimensión"
      subdimension_nm <- "Subdimensión"
      ind1_nm         <- "Indicador.1"
      ind2_nm         <- "Indicador.2"
      ind3_nm         <- "Indicador.3"
      id_nm           <- "Id del Indicador"
    }

    out <- n.6 %>%
      dplyr::select(
        !!area_nm         := name.x.x.x,
        !!dimension_nm    := name.y,
        !!subdimension_nm := name.x.x,
        !!ind1_nm         := name.y.y,
        !!ind2_nm         := name.x,
        !!ind3_nm         := name.y.y.y,
        id.y.y,
        id.x,
        id.y.y.y
      ) %>%
      dplyr::mutate(
        !!id_nm := ifelse(
          is.na(id.y.y.y) & is.na(id.x),
          id.y.y,
          ifelse(is.na(id.y.y.y), id.x, id.y.y.y)
        )
      ) %>%
      dplyr::select(-id.y.y.y, -id.y.y, -id.x) %>%
      dplyr::mutate(
        dplyr::across(
          c(all_of(ind1_nm), all_of(ind2_nm), all_of(ind3_nm)),
          ~ ifelse(is.na(.), "", trimws(as.character(.)))
        )
      )

    # 6. Explicit type conversion of final ID ----
    out[[id_nm]] <- suppressWarnings(as.numeric(out[[id_nm]]))

    # Trim remaining character columns
    out <- out %>%
      dplyr::mutate(
        dplyr::across(where(is.character), trimws)
      )

    out
  }

  # 3. Define language and URL ----
  lang <- if (isTRUE(language.en)) "en" else "es"

  url.indicators <- paste0(
    "https://api-cepalstat.cepal.org/cepalstat/api/v1/thematic-tree?lang=",
    lang,
    "&format=csv"
  )

  if (progress) {
    if (isTRUE(language.en)) {
      message("Downloading thematic tree from CEPALSTAT...")
    } else {
      message("Descargando árbol temático desde CEPALSTAT...")
    }
  }

  # 4. Download data ----
  data <- get_thematic_tree(url.indicators)

  if (progress) {
    if (isTRUE(language.en)) {
      message("Building indicator hierarchy...")
    } else {
      message("Construyendo jerarquía de indicadores...")
    }
  }

  # 5. Build final table ----
  data <- build_indicator_table(data = data, language.en = language.en)

  if (progress) {
    if (isTRUE(language.en)) {
      message("Finished downloading indicators.")
    } else {
      message("Descarga de indicadores finalizada.")
    }
  }

  return(data)
}
