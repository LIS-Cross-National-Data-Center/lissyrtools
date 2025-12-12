# Determine the name of the weight variable

**\[deprecated\]** Computes the name of the weight variable based on the
information from the 'lissy_files' object, 'files_level' and
'variable_level'

## Usage

``` r
determine_weight(lissy_files, variable, files_level, variable_level)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- variable:

  A character string indicating the aggregate for which the indicator
  needs to be computed.

- files_level:

  A string indicating the level of the file. Valid inputs are:
  'household', 'h', 'person' or 'p'. If NULL (default), the file level
  will be retrived from the 'lissy_files' attributes.

- variable_level:

  Level of the variable. Should be either 'household', 'h', 'person' or
  'p'. If NULL (default), the function will try to guess the level of
  the variable. This is done by comparing the value in 'variable' with
  pre-set lists of variables.

## Value

A character vector with the name of the weight variable.
