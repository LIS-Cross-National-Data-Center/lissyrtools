# Checks for Invalid Vars

Internal helper to ensure the `vars` argument has no invalid variable
names.

## Usage

``` r
check_invalid_vars(vars, lws = FALSE)
```

## Arguments

- vars:

  A character vector.

- lws:

  Logical.

## Value

Stops if all characters in `vars` are invalid, and it warns if only some
are invalid.
