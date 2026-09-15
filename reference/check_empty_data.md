# Checks for Empty Data Input

Internal helper to ensure the `data` argument is not `NULL` when
required. Suggests loading all countries if input is missing.

## Usage

``` r
check_empty_data(data, database = "lis")
```

## Arguments

- data:

  A character vector.

- database:

  A character value. One of "lis" (default), "lws", or "lcs".

## Value

Stops with an informative error message if `data` is `NULL`.
