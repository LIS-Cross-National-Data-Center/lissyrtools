# Checks for Invalid ccyy.

Internal helper to ensure the ccyy pairs in argument `data` are valid.

## Usage

``` r
invalid_ccyy_pairs(data, database = "lis")
```

## Arguments

- data:

  A character vector.

- database:

  A character value. One of "lis" (default), "lws", or "lcs".

## Value

Stops if all ccyy codes in `data` are invalid, and it warns if only some
are invalid.
