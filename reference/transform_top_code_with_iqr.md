# Apply top or bottom coding with log IQR

**\[superseded\]** Applies an upper or lower limit on variable values
using the Interquartile Range (IQR) of the variable transformed with the
natural logarithm and a scale factor ('times').

If the lissy files passed are at person-level and the variable is
household-level, only household heads are used to compute the IQR of the
log transformed variable. For person-level variables, all individuals in
the file are used.

## Usage

``` r
transform_top_code_with_iqr(
  lissy_files,
  variable,
  times = 3,
  files_level = NULL,
  variable_level = NULL,
  weight = NULL
)

transform_bottom_code_with_iqr(
  lissy_files,
  variable,
  times = 3,
  files_level = NULL,
  variable_level = NULL,
  weight = NULL
)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- variable:

  A character string with the name of the variable that should be
  transformed.

- times:

  A numeric indicating the scale factor for IQR. Defaults to 3.

- files_level:

  A string indicating the level of the file. Valid inputs are:
  'household', 'h', 'person' or 'p'. If NULL (default), the file level
  will be retrived from the 'lissy_files' attributes.

- variable_level:

  Level of the variable. Should be either 'household', 'h', 'person' or
  'p'. If NULL (default), the function will try to guess the level of
  the variable. This is done by comparing the value in 'variable' with
  pre-set lists of variables.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights.

## Value

A list of tibbles with the recoded variable.
