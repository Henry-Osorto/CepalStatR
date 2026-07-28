# Internal CEPALSTAT GET request helper

Performs HTTP GET requests to CEPALSTAT endpoints and parses responses
according to the requested format.

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

  Character string with the request URL.

- format:

  Character string indicating the response format. One of `"json"` or
  `"csv"`.

- timeout_sec:

  Numeric value indicating the request timeout in seconds. Default is
  `60`.

- simplify_vector:

  Logical. If `TRUE`, JSON responses are simplified into vectors,
  matrices, or data frames when possible. If `FALSE`, the parsed JSON
  structure is returned without vector simplification. Default is
  `FALSE`.

## Value

Parsed CEPALSTAT response.
