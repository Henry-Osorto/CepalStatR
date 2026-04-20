# Sustainable Development Goals ranking

Creates a ranking chart for a CEPALSTAT indicator belonging to the
Sustainable Development Goals (SDG/ODS) dimension, using the latest
available data by country.

## Usage

``` r
ranking.sdg(
  id.indicator,
  language.en = TRUE,
  save = FALSE,
  file = NULL,
  format = c("png", "pdf", "svg"),
  height = 5,
  width = 9,
  size.title = 10,
  title = TRUE,
  caption = TRUE,
  color = "#0C4A61",
  color.gc = "#34B0AA",
  progress = TRUE
)
```

## Arguments

- id.indicator:

  Numeric or character. Indicator ID.

- language.en:

  Logical. If `TRUE` (default), English labels are used. If `FALSE`,
  Spanish labels are used.

- save:

  Logical. If `TRUE`, saves the resulting figure.

- file:

  Character. Output filename when `save = TRUE`. If `NULL`, a default
  filename is used.

- format:

  Character. Output format when `save = TRUE`. One of `"png"` (default),
  `"pdf"` or `"svg"`.

- height:

  Numeric. Height of saved figure.

- width:

  Numeric. Width of saved figure.

- size.title:

  Numeric. Title size.

- title:

  Logical. If `TRUE`, displays the chart title.

- caption:

  Logical. If `TRUE`, displays a note indicating the latest available
  year by country.

- color:

  Character. Color for countries in the ranking.

- color.gc:

  Character. Color for regional aggregates if present.

- progress:

  Logical. If `TRUE`, shows progress messages during execution.

## Value

Invisibly returns a `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
ranking.sdg(id.indicator = 3682)
ranking.sdg(id.indicator = 3682, language.en = FALSE)

ranking.sdg(
  id.indicator = 3682,
  save = TRUE
)

ranking.sdg(
  id.indicator = 3682,
  save = TRUE,
  format = "pdf",
  file = "ranking_sdg.pdf"
)

ranking.sdg(
  id.indicator = 3682,
  save = TRUE,
  format = "svg",
  file = "ranking_sdg.svg"
)
} # }
```
