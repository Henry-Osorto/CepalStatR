# Internal helper to retrieve CEPALSTAT resources

Performs an HTTP GET request to a CEPALSTAT endpoint and parses the
response according to the requested format.

## Usage

``` r
cepal_get(
  url,
  format = c("json", "csv"),
  timeout_sec = 60,
  simplify_vector = FALSE
)
```

## Arguments

- url:

  A non-empty character string containing the complete request URL.

- format:

  Response format. One of `"json"` or `"csv"`.

- timeout_sec:

  Positive numeric value indicating the request timeout in seconds.

- simplify_vector:

  Logical. If `TRUE`, JSON arrays are simplified when possible. If
  `FALSE`, the original nested list structure is preserved.

## Value

The parsed response returned by CEPALSTAT.
