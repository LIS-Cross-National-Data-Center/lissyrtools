# Checks for Invalid Vars

Internal helper to ensure the `vars` argument has no invalid variable
names.

## Usage

``` r
check_invalid_vars(vars, database = "lis")
```

## Arguments

- vars:

  A character vector.

- database:

  A character value. One of "lis" (default), "lws", or "lcs".

## Value

Stops if all characters in `vars` are invalid, and it warns if only some
are invalid.
