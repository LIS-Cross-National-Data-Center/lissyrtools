# Checks for Invalid iso2 codes.

Internal helper to ensure the iso2 codes in argument `data` correspond
to valid iso2 codes.

## Usage

``` r
check_iso2(data, database = "lis")
```

## Arguments

- data:

  A character vector.

- database:

  A character value. One of "lis" (default), "lws", or "lcs".

## Value

Stops if all iso2 codes in `data` are invalid, and it warns if only some
are invalid.
