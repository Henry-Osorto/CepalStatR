# CEPALSTAT thematic map of indicators

Creates an interactive thematic tree of CEPALSTAT indicators using
`collapsibleTree`, with the same visual header and footer style as
[`viewer.indicators()`](https://henry-osorto.github.io/CepalStatR/reference/viewer.indicators.md).

## Usage

``` r
topic_map(language.en = TRUE, progress = TRUE, open.browser = FALSE)
```

## Arguments

- language.en:

  Logical. If `TRUE` (default), English labels are used. If `FALSE`,
  Spanish labels are used.

- progress:

  Logical. If `TRUE`, progress messages are shown.

- open.browser:

  Logical. If `TRUE`, the generated HTML is saved to a temporary file
  and opened in the default web browser. Defaults to `FALSE`.

## Value

A browsable HTML widget.

## Examples

``` r
if (FALSE) { # \dontrun{
cepal_topic_map()
cepal_topic_map(language.en = FALSE)
cepal_topic_map(open.browser = TRUE)
} # }
```
