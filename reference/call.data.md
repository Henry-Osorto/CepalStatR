# Call Data Indicators CEPALSTAT

call.data allows querying the CEPALSTAT API to obtain a data frame with
indicator values, disaggregations, years, countries, and other
dimensions available for the selected indicator.

## Usage

``` r
call.data(id.indicator, language.en = TRUE, notes = FALSE)
```

## Arguments

- id.indicator:

  Indicator ID to retrieve.

- language.en:

  Logical. TRUE for English, FALSE for Spanish.

- notes:

  Logical. If TRUE, methodological notes are added when available.

## Value

A data frame with indicator values and labels.
