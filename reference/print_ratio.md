# Print the Ratio Index

**\[superseded\]** Computes and displays the ratio index for a given
variable across multiple files.

## Usage

``` r
print_ratio(
  lissy_files,
  variable,
  ratio,
  weight = NULL,
  na.rm = FALSE,
  files_level = NULL,
  variable_level = NULL
)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- variable:

  A character string indicating the variable for which the ratio index
  needs to be computed.

- ratio:

  A vector of two numeric values between 0 and 1. Defines the
  percentiles in the numerator and denominator respectively. E.g. (0.9,
  0.1) computes the 90/10 ratio.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights. If NULL (default), the function tries to guess the
  needed weight to compute the ratio index. This guess is made based on
  the information from files_level and variable_level.

- na.rm:

  A boolean. Indicates if NAs should be ignored. Defaults to FALSE.

- files_level:

  A string indicating the level of the file. Valid inputs are:
  'household', 'h', 'person' or 'p'. If NULL (default), the file level
  will be retrieved from the 'lissy_files' attributes.

- variable_level:

  Level of the variable. Should be either 'household', 'h', 'person' or
  'p'. If NULL (default), the function will try to guess the level of
  the variable. This is done by comparing the value in 'variable' with
  pre-set lists of variables.

## Value

A numeric vector containing the ratio index for each file.

## Examples

``` r
if (FALSE) { # \dontrun{
lissy_files <- read_lissy_files(c("file1", "file2"))
print_ratio(lissy_files = lissy_files, variable = "income", ratio = c(0.9, 0.1))
} # }
```
