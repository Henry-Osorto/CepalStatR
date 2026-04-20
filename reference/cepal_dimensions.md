# Retrieve CEPALSTAT dimensions for an indicator

Retrieves all dimensions associated with a CEPALSTAT indicator,
including their members and metadata.

## Usage

``` r
cepal_dimensions(id.indicator, language.en = TRUE)
```

## Arguments

- id.indicator:

  Numeric or character. Indicator ID.

- language.en:

  Logical. If TRUE (default), English labels are used. If FALSE, Spanish
  labels are returned.

## Value

A list of dimensions. Each element contains:

- name: Dimension name

- id: Dimension ID

- members: Data frame of dimension members

## Examples

``` r
dims <- cepal_dimensions(1)
names(dims)
#> [1] ""
```
