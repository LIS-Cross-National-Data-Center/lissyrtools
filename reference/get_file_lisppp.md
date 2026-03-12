# Retreive the 'lisppp' deflator for a given file and variable.

**\[deprecated\]**

## Usage

``` r
get_file_lisppp(
  file_name,
  database,
  variable = NULL,
  income_variable = NULL,
  ppp_data = "lissyrtools",
  version_ppp = version_ppp
)
```

## Arguments

- file_name:

  A sting with the file name with format 'ccyy'.

- database:

  'lis' or 'lws'.

- variable:

  A string with the variable name. Defaults to NULL as it is optional
  for 'lis' files. It is required if 'database' = 'lws”.

- income_variable:

  Defaults to NULL. Is only relevant for LWS files. If the file is LWS
  and 'income_variable = TRUE', the function will retrieve the deflator
  for the year in which the income data was collected. This might not be
  the same as the year when wealth variables were collected.

- ppp_data:

  An optional file with the deflators. Should be in the same format as
  the tibble in 'lissyrtools::deflators'. If "lissyrtools" (default) the
  deflators are imported from the package internal data.

## Value

A numeric value with the'lisppp' deflator for the file

## Details

Retrieves the 'lisppp' deflator for a file. It takes into account that
the reference year of income variables for LWS files might differ from
the year of the file.
