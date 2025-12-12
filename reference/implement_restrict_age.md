# Apply 'transform_restrict_age()' to a single file

**\[deprecated\]** Applies 'transform_restrict_age()' to a single
LIS/LWS file.

## Usage

``` r
implement_restrict_age(file, file_name, variable, from, to)
```

## Arguments

- file:

  A LIS or LWS file.

- file_name:

  The name of the LIS or LWS file.

- variable:

  A character vector of length one with the variable that will be
  recoded to NA when age is outside the boundaries (both included).

- from:

  An integer with the lower boundary value (included in the sample).

- to:

  An integer with the higher boundary value (included in the sample).

## Value

A a file with the filtered age variable.
