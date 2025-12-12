# Apply the recoding of zeros into missing values if all values are zero

Lower-level function used within 'transform_false_zeros_to_na()' .

## Usage

``` r
implement_false_zeros_to_na(file, file_name, variable)
```

## Arguments

- file:

  A tibble or data.frame with a LIS or LWS file.

- file_name:

  A string with the name of the LIS or LWS file.

- variable:

  A string with the variable to which top coding should be applied.

## Value

A tibble containing the file with the recoded variable.
