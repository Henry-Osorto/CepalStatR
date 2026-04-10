# Call indicators from CEPALSTAT

Retrieves the thematic tree from the CEPALSTAT API in JSON format and
returns a data frame with the hierarchical structure of indicators.

## Usage

``` r
call.indicators(language.en = TRUE, progress = TRUE)
```

## Arguments

- language.en:

  Logical. If `TRUE` (default), labels are returned in English. If
  `FALSE`, labels are returned in Spanish.

- progress:

  Logical. If `TRUE` (default), progress messages are displayed.

## Value

A data frame with the hierarchical thematic structure of CEPALSTAT
indicators and the corresponding indicator ID.

## Examples

``` r
if (FALSE) { # \dontrun{
data.indicators <- call.indicators()
data.indicators <- call.indicators(language.en = FALSE)
} # }
```
