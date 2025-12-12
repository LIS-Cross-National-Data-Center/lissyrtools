# Apply top or bottom coding with log IQR to a single file

**\[superseded\]** Applies an upper or lower limit on variable values
using the Interquartile Range (IQR) of the variable transformed with the
natural logarithm and a scale factor ('times'). For 'household level'
variables, the IQR of the log transformed variable is computed using
only household heads. For 'person level' variables, all individuals in
the file are used.

## Usage

``` r
implement_top_code_with_iqr(
  file,
  file_name,
  variable,
  times,
  file_level,
  variable_level = NULL,
  weight = NULL
)

implement_bottom_code_with_iqr(
  file,
  file_name,
  variable,
  times,
  file_level,
  variable_level = NULL,
  weight = NULL
)
```

## Arguments

- file:

  A LIS or LWS file.

- file_name:

  A string with the name of the LIS or LWS file.

- variable:

  A string with the variable to which top coding should be applied.

- times:

  A numeric indicating the scale factor for IQR. Defaults to 3.

- file_level:

  A string indicating the level of the file. Valid inputs are:
  'household', 'h', 'person' or 'p'.

- variable_level:

  A string with the level of the variable. Should be either 'household'
  or 'person'. If NULL (default), the function will try to guess the
  level of the variable. This is done by comparing the value in
  'variable' with pre-set lists of variables.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights.

## Value

A tibble containing the file with the recoded variable.

## Details

To be used inside transform_top_code_with_iqr() and
transform_bottom_code_with_iqr() .
