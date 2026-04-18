# List of countries available in CEPALSTAT

Retrieves the list of countries available in CEPALSTAT using the
dimensions endpoint of a reference indicator. This approach avoids
downloading full datasets and ensures consistency with the API
structure.

## Usage

``` r
countries(language.en = TRUE)
```

## Arguments

- language.en:

  Logical. If TRUE (default), returns country names in English. If
  FALSE, returns names in Spanish.

## Value

A data frame containing the list of countries.

## Details

The function extracts the "Country" (or "País") dimension from the
CEPALSTAT API using a JSON-based request. This method is more efficient
than retrieving full indicator datasets.

## Examples

``` r
countries()
#> Error in x$name: $ operator is invalid for atomic vectors
countries(language.en = FALSE)
#> Error in x$name: $ operator is invalid for atomic vectors
```
