# Print the Atkinson Index

**\[superseded\]**

Computes and displays the Atkinson index for a given variable across
multiple files.

## Usage

``` r
print_atkinson(
  lissy_files,
  variable,
  epsilon,
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

  A character string indicating the variable for which the Atkinson
  index needs to be computed.

- epsilon:

  A numeric vector of length one. The inequality aversion parameter.
  Needs to be epsilon \> 0.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights. If NULL (default), the function tries to guess the
  needed weight to compute the Atkinson index. This guess is made based
  on the information from files_level and variable_level.

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

A numeric vector containing the Atkinson index for each file.

## Examples

``` r
if (FALSE) { # \dontrun{
lissy_files <- read_lissy_files(c("file1", "file2"))
print_atkinson(lissy_files = lissy_files, variable = "income", epsilon = 0.5)
} # }
```
