# Apply 'transform_restrict_to_household_heads()' to a single file

**\[deprecated\]** Applies 'transform_restrict_to_household_heads()' to
a single LIS/LWS file.

## Usage

``` r
implement_restrict_to_household_heads(file, file_name, variable)
```

## Arguments

- file:

  A LIS or LWS file.

- file_name:

  The name of the LIS or LWS file.

- variable:

  A character vector of length one with the variable that will be
  recoded to NA when relation is missing or not equal to 1000 (household
  head).

## Value

A a file with the filtered age variable.
