# Import lisppp dataset

**\[deprecated\]** Retrieves the lisppp deflators dataset.

## Usage

``` r
import_lisppp_data(path_to_ppp_file = "lissy", version_ppp = version_ppp)
```

## Arguments

- path_to_ppp_file:

  A character string indicating where the deflator values can be found.
  If the value is 'lissyrtools', it will import the data from
  'lissyrtools'. Specifying 'lissy' (default) will read them from within
  the LISSY directory. Any other value requires the full path
  specification to the deflators file.

## Value

A tibble with the lisppp deflators.
