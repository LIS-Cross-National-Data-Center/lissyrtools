# Wrap over 'purrr::imap()' for lissy files

**\[deprecated\]** A wrap over 'purrr::imap()' that keeps the lissy
attributes on the output list.

## Usage

``` r
lissy_map(lissy_files, .f)
```

## Arguments

- lissy_files:

  A list of LIS or LWS p-level files.

- .f:

  A function, formula, or vector (not necessarily atomic) as in the
  argument from purrr::map with the same name.

## Value

A list like lissy_files
