# Call indicators from CEPALSTAT

Retrieves the CEPALSTAT thematic tree and returns a data frame with the
hierarchical structure of areas, dimensions, subdimensions, indicators,
and final indicator IDs.

## Usage

``` r
call.indicators(language.en = TRUE, progress = TRUE)
```

## Arguments

- language.en:

  Logical. If `TRUE` (default), results are returned with English
  labels. If `FALSE`, results are returned with Spanish labels.

- progress:

  Logical. If `TRUE`, status messages are displayed during download and
  processing.

## Value

A data frame containing the hierarchical thematic structure of CEPALSTAT
indicators and their final indicator IDs.

## Examples

``` r
data.indicators <- call.indicators()
#> Downloading thematic tree from CEPALSTAT...
#> Parsing indicator hierarchy...
#> Finished downloading indicators.
data.indicators <- call.indicators(language.en = FALSE)
#> Descargando árbol temático desde CEPALSTAT...
#> Procesando jerarquía de indicadores...
#> Descarga de indicadores finalizada.
```
