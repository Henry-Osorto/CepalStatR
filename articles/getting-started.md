# Getting Started with CepalStatR

## Overview

`CepalStatR` provides functions to access, explore, and visualize data
from the statistical data portal of the Economic Commission for Latin
America and the Caribbean. The package is designed to support
reproducible workflows in R by retrieving metadata and indicator data
through a public statistical API.

## Loading the package

``` r

library(CepalStatR)
```

## Exploring available indicators

The function
[`call.indicators()`](https://henry-osorto.github.io/CepalStatR/reference/call.indicators.md)
retrieves the hierarchical structure of available indicators.

``` r

indicators <- call.indicators(progress = FALSE)
head(indicators)
#>                     Area   Dimension Subdimension
#> 1 Demographic and social Demographic   Population
#> 2 Demographic and social Demographic   Population
#> 3 Demographic and social Demographic   Population
#> 4 Demographic and social Demographic   Population
#> 5 Demographic and social Demographic   Population
#> 6 Demographic and social Demographic    Fertility
#>                                                       Group Sub Group Level 1
#> 1                                  Total population, by sex              <NA>
#> 2                          Population, by age group, by sex              <NA>
#> 3 Demographic dependency ratio, by dependent groups and sex              <NA>
#> 4    Structure of the total population by sex and age group              <NA>
#> 5  Annual growth rate of the total population, by age group              <NA>
#> 6                                          Crude birth rate              <NA>
#>   Sub Group Level 2                                            Indicator Name
#> 1              <NA>                                  Total population, by sex
#> 2              <NA>                          Population, by age group, by sex
#> 3              <NA> Demographic dependency ratio, by dependent groups and sex
#> 4              <NA>    Structure of the total population by sex and age group
#> 5              <NA>  Annual growth rate of the total population, by age group
#> 6              <NA>                                          Crude birth rate
#>   Indicator ID
#> 1         4788
#> 2         4789
#> 3         4792
#> 4         4793
#> 5         4795
#> 6         4787
```

The returned data frame includes information about thematic areas,
dimensions, subdimensions, groups, indicator names, and indicator
identifiers.

## Listing available countries

The function
[`countries()`](https://henry-osorto.github.io/CepalStatR/reference/countries.md)
returns the countries available in the statistical dimensions used by
the API.

``` r

head(countries(language.en = TRUE))
#>             Countries
#> 1            Anguilla
#> 2 Antigua and Barbuda
#> 3           Argentina
#> 4               Aruba
#> 5             Bahamas
#> 6            Barbados
```

## Retrieving indicator data

The main data retrieval function is
[`call.data()`](https://henry-osorto.github.io/CepalStatR/reference/call.data.md).
Users provide an indicator identifier and obtain an analysis-ready data
frame.

``` r

population <- call.data(id.indicator = 1, progress = FALSE)
head(population)
#> # A tibble: 6 × 15
#>    Value Sex    Country Years indicator_meta_name unit  definition data_features
#>    <dbl> <chr>  <chr>   <chr> <chr>               <chr> <chr>      <chr>        
#> 1 31216. Both … Argent… 1987  Total population, … Thou… "<p align… Annual estim…
#> 2 33568. Both … Argent… 1992  Total population, … Thou… "<p align… Annual estim…
#> 3 34489. Both … Argent… 1994  Total population, … Thou… "<p align… Annual estim…
#> 4 37480. Both … Argent… 2001  Total population, … Thou… "<p align… Annual estim…
#> 5 38278. Both … Argent… 2003  Total population, … Thou… "<p align… Annual estim…
#> 6 40684. Both … Argent… 2009  Total population, … Thou… "<p align… Annual estim…
#> # ℹ 7 more variables: calculation_methodology <chr>, comments <chr>,
#> #   theme <chr>, area_meta <chr>, last_update <chr>, indicator_id <dbl>,
#> #   indicator_name <chr>
```

## Interactive tools

`CepalStatR` also includes interactive tools to explore the indicator
catalogue.

``` r

viewer.indicators()
topic_map()
```

These functions return HTML-based outputs that are useful for exploring
metadata and thematic hierarchies.

## Visualization examples

The package includes functions for common visual outputs, such as
population pyramids and rankings for indicators related to sustainable
development goals.

``` r

pyramids(country = "Honduras", years = c(1, 5, 10, 15), progress = FALSE)

ranking.sdg(id.indicator = 3682, progress = FALSE)
```

When saving output files, use
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) in examples and
reproducible scripts.

``` r

ranking.sdg(id.indicator = 3682, save = TRUE, file = file.path(tempdir(), "ranking_sdg.png"), progress = FALSE)
```

## Reproducibility notes

The package retrieves data from a public API. Therefore, results may
depend on internet connectivity and the availability of the external
service at runtime. Functions include progress messages and basic error
handling to support reproducible workflows.
