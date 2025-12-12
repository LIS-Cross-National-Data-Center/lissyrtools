# Missing values in variable warning

Triggers a warning if there are missing values in 'variable' and the
warning has not been triggered before.

## Usage

``` r
missing_values_in_variable_warning(file, file_name, variable)
```

## Arguments

- file:

  A LIS or LWS file.

- file_name:

  The name of the LIS or LWS file.

- variable:

  A string with the variable name.

## Details

Uses an option to store the names of the files and variables with
missing values that have already triggered a warning before. The option
is stored as: 'file_name*warning_NAs*variable'.
