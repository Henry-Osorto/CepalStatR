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

The function extracts the "Country" (or "Pas") dimension from the
CEPALSTAT API using a JSON-based request. This method is more efficient
than retrieving full indicator datasets.

## Examples

``` r
countries()
#>                             Countries
#> 1                            Anguilla
#> 2                 Antigua and Barbuda
#> 3                           Argentina
#> 4                               Aruba
#> 5                             Bahamas
#> 6                            Barbados
#> 7                              Belize
#> 8    Bolivia (Plurinational State of)
#> 9                              Brazil
#> 10             British Virgin Islands
#> 11                          Caribbean
#> 12              Caribbean Netherlands
#> 13                     Cayman Islands
#> 14                              Chile
#> 15                           Colombia
#> 16                         Costa Rica
#> 17                               Cuba
#> 18                            Curaçao
#> 19                           Dominica
#> 20                 Dominican Republic
#> 21                            Ecuador
#> 22                        El Salvador
#> 23                            Grenada
#> 24                         Guadeloupe
#> 25                          Guatemala
#> 26                             Guyana
#> 27                              Haiti
#> 28                           Honduras
#> 29                            Jamaica
#> 30                      Latin America
#> 31    Latin America and the Caribbean
#> 32                         Martinique
#> 33                             Mexico
#> 34                         Montserrat
#> 35                          Nicaragua
#> 36                             Panama
#> 37                           Paraguay
#> 38                               Peru
#> 39                        Puerto Rico
#> 40              Saint Kitts and Nevis
#> 41                        Saint Lucia
#> 42   Saint Vincent and the Grenadines
#> 43          Sint Maarten (Dutch part)
#> 44                           Suriname
#> 45                Trinidad and Tobago
#> 46           Turks and Caicos Islands
#> 47       United States Virgin Islands
#> 48                            Uruguay
#> 49 Venezuela (Bolivarian Republic of)
countries(language.en = FALSE)
#> Error: No country members found.
```
