# Call Data Indicators CEPALSTAT

Retrieves indicator data from the CEPALSTAT API in JSON format and
returns a data frame with values, dimensions, metadata, and optionally
footnotes.

## Usage

``` r
call.data(id.indicator, language.en = TRUE, notes = FALSE,
                 progress = TRUE, add.indicator.name = TRUE)
```

## Arguments

- id.indicator:

  A single CEPALSTAT indicator ID.

- language.en:

  Logical. If `TRUE` (default), the output uses English labels. If
  `FALSE`, Spanish labels are used when applicable.

- notes:

  Logical. If `TRUE`, footnotes are joined when available.

- progress:

  Logical. If `TRUE`, progress messages are shown.

- add.indicator.name:

  Logical. If `TRUE`, adds indicator ID and indicator name.

## Value

A data frame with indicator values, dimension labels, and metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- call.data(id.indicator = 4788)
data <- call.data(id.indicator = 4788, language.en = FALSE)
data <- call.data(id.indicator = 4788, notes = TRUE)
} # }
```
