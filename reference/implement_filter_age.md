# Apply 'transform_filter_age()' to a single file

**\[deprecated\]** Applies 'transform_filter_age()' to a single LIS/LWS
file.

To be used inside 'transform_filter_age()' and
'transform_restrict_age()'.

## Usage

``` r
implement_filter_age(file, file_name, from, to)
```

## Arguments

- file:

  A LIS or LWS file.

- file_name:

  The name of the LIS or LWS file.

- from:

  An integer with the lower boundary value (included in the sample).

- to:

  An integer with the higher boundary value (included in the sample).

## Value

A a file with the filtered age variable.
