# Viewer for CEPALSTAT indicators

Displays an interactive HTML table for browsing the hierarchical
structure of CEPALSTAT indicators using
[`call.indicators()`](https://henry-osorto.github.io/CepalStatR/reference/call.indicators.md)
as backend.

## Usage

``` r
viewer.indicators(
  language.en = TRUE,
  progress = TRUE,
  show_search = TRUE,
  striped = TRUE,
  bordered = FALSE,
  compact = FALSE,
  highlight = TRUE,
  full_width = TRUE,
  page_size = 15,
  open.browser = FALSE
)
```

## Arguments

- language.en:

  Logical. If `TRUE` (default), the viewer uses English labels. If
  `FALSE`, Spanish labels are used.

- progress:

  Logical. If `TRUE`, progress messages are shown.

- show_search:

  Logical. If `TRUE`, enables global search in the table.

- striped:

  Logical. If `TRUE`, striped rows are shown.

- bordered:

  Logical. If `TRUE`, table borders are shown.

- compact:

  Logical. If `TRUE`, reduces row padding.

- highlight:

  Logical. If `TRUE`, highlights rows on hover.

- full_width:

  Logical. If `TRUE`, table uses full available width.

- page_size:

  Integer. Number of rows to show per page.

- open.browser:

  Logical. If `TRUE`, the generated HTML is saved to a temporary file
  and opened in the default web browser. Defaults to `FALSE`.

## Value

A browsable HTML object containing the header and interactive table.

## Examples

``` r
if (FALSE) { # \dontrun{
viewer.indicators()
viewer.indicators(language.en = FALSE)
viewer.indicators(open.browser = TRUE)
} # }
```
