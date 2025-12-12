# Checks for Empty Data Input

Internal helper to ensure the `data` argument is not `NULL` when
required. Suggests loading all countries if input is missing.

## Usage

``` r
check_empty_data(data, lws = FALSE)
```

## Arguments

- data:

  A character vector.

- lws:

  Logical. If `TRUE`, suggests loading all LWS countries; otherwise,
  suggests LIS countries.

## Value

Stops with an informative error message if `data` is `NULL`.
