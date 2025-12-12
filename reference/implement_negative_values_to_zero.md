# Apply recoding negative values to zero

**\[superseded\]** Applies the recoding of negative values into zeroes
in a variable of a single file.

## Usage

``` r
implement_negative_values_to_zero(file, file_name, variable)
```

## Arguments

- file:

  A LIS or LWS file.

- file_name:

  The name of the LIS or LWS file

- variable:

  A string with the variable to which recoding should be applied.

## Value

A file with the recoded variable.

## Details

To be used inside transform_negative_values_to_zero().
