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
#>                                    Paises
#> 1                          América Latina
#> 2              América Latina y el Caribe
#> 3                                 Anguila
#> 4                       Antigua y Barbuda
#> 5                               Argentina
#> 6                                   Aruba
#> 7                                 Bahamas
#> 8                                Barbados
#> 9                                  Belice
#> 10      Bolivia (Estado Plurinacional de)
#> 11                                 Brasil
#> 12                      Caribe neerlandés
#> 13                                  Chile
#> 14                               Colombia
#> 15                             Costa Rica
#> 16                                   Cuba
#> 17                                Curaçao
#> 18                               Dominica
#> 19                                Ecuador
#> 20                              El Caribe
#> 21                            El Salvador
#> 22                                Granada
#> 23                              Guadalupe
#> 24                              Guatemala
#> 25                                 Guyana
#> 26                                  Haití
#> 27                               Honduras
#> 28                           Islas Caimán
#> 29                  Islas Turcas y Caicos
#> 30              Islas Vírgenes Británicas
#> 31   Islas Vírgenes de los Estados Unidos
#> 32                                Jamaica
#> 33                              Martinica
#> 34                             Montserrat
#> 35                                 México
#> 36                              Nicaragua
#> 37                                 Panamá
#> 38                               Paraguay
#> 39                                   Perú
#> 40                            Puerto Rico
#> 41                   República Dominicana
#> 42                    Saint Kitts y Nevis
#> 43 San Martín (parte de los Países Bajos)
#> 44           San Vicente y las Granadinas
#> 45                            Santa Lucía
#> 46                               Suriname
#> 47                      Trinidad y Tabago
#> 48                                Uruguay
#> 49   Venezuela (República Bolivariana de)
```
