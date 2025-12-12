# Checks for Invalid ccyy.

Internal helper to ensure the ccyy pairs in argument `data` are valid.

## Usage

``` r
invalid_ccyy_pairs(data, lws = FALSE)
```

## Arguments

- data:

  A character vector.

- lws:

  Logical.

## Value

Stops if all ccyy codes in `data` are invalid, and it warns if only some
are invalid.
