# Filter a sample by age

**\[deprecated\]** 'transform_filter_age()' filter the rows in a LIS/LWS
file so all cases have ages between two values (both included),
returning a list with the files containing only rows where the age is
between those values. I.e. a subset of rows.

## Usage

``` r
transform_filter_age(lissy_files, from, to)
```

## Arguments

- lissy_files:

  A list of LIS or LWS p-level files.

- from:

  An integer with the lower boundary value (included in the sample).

- to:

  An integer with the higher boundary value (included in the sample).

## Value

A list of tibbles with the subset of rows.
