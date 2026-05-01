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
# \donttest{
data <- call.data(id.indicator = 4788)
#> Downloading indicator data from CEPALSTAT...
#> Indicator: Total population, by sex (ID: 4788)
#> Assigning labels to dimension values...
#> Assigning variable names...
#> Finished download: Total population, by sex
data <- call.data(id.indicator = 4788, language.en = FALSE)
#> Descargando datos del indicador desde CEPALSTAT...
#> Indicador: Población total, según sexo (ID: 4788)
#> Asignando etiquetas a los valores de las dimensiones...
#> Asignando nombres de variables...
#> Descarga finalizada: Población total, según sexo
data <- call.data(id.indicator = 4788, notes = TRUE)
#> Downloading indicator data from CEPALSTAT...
#> Indicator: Total population, by sex (ID: 4788)
#> Assigning labels to dimension values...
#> Assigning variable names...
#> Adding footnotes...
#> Finished download: Total population, by sex
# }
```
