# Print the Gini Coefficient

**\[superseded\]** Computes and displays the Gini coefficient for a
given variable across multiple files.

## Usage

``` r
print_gini(
  lissy_files,
  variable,
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

  A character string indicating the variable for which the Gini
  coefficient needs to be computed.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights. If NULL (default), the function tries to guess the
  needed weight to compute the Gini coefficient. This guess is made
  based on the information from files_level and variable_level.

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

A numeric vector containing the Gini coefficient for each file.

## Examples

``` r
if (FALSE) { # \dontrun{
lissy_files <- read_lissy_files(c("file1", "file2"))
print_gini(lissy_files = lissy_files, variable = "income")
} # }
```
