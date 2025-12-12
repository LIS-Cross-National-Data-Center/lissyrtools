# Applies the deflator adjustment to an aggregate

**\[superseded\]**

## Usage

``` r
implement_adjust_by_lisppp(
  file,
  file_name,
  database,
  variable,
  income_variable = NULL,
  ppp_data = "lissyrtools"
)
```

## Arguments

- file:

  A list of LIS or LWS file

- file_name:

  The name of the LIS or LWS file.

- variable:

  A string with the name of the variable to which the adjustment should
  be applied.

- income_variable:

  Defaults to NULL. Is only relevant for LWS files. If the file is LWS
  and 'income_variable = TRUE', the function will retrieve the deflator
  for the year in which the income data was collected. This might not be
  the same as the year when wealth variables were collected.

- ppp_data:

  An optional file with the deflators. Should be in the same format as
  the tibble in 'lissyrtools::deflators'. If "lissyrtools" (default) the
  deflators are imported from the package internal data.

## Details

To be used inside transform_adjust_by_lisppp().
