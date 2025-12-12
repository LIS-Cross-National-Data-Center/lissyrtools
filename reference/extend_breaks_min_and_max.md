# Extend the minimum and maximum of a vector used for breaks

The 'cut' function can sometimes fail to include the minimum and maximum
values into intervals due to 'the floating point trap'. This function
modifies the breaks by extending the lower and uppermost values so it
always includes the minimum and maximum.

## Usage

``` r
extend_breaks_min_and_max(x, breaks, extension = 1e-04)
```

## Arguments

- x:

  Numeric vector with all values that intervals should include.

- breaks:

  Numeric vector with the breaks that might need to be extended.

- extension:

  A real number by which the maximum and minimum values should be
  extended if the originals don't include all values in 'x'.
