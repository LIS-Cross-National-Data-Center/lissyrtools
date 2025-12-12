# Apply recoding of zeros into missing values

**\[superseded\]** Applies the recoding of zeroes into missing values in
a file for a single variable.

## Usage

``` r
implement_zeros_to_na(file, file_name, variable)
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

To be used inside transform_zeros_to_na().
