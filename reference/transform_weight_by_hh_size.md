# Multiply by household size

**\[superseded\]** Multiplies the household weights ('hwgt') by the
number of individuals in the household ('nhhmem'). It is used for
computations at household level.

## Usage

``` r
transform_weight_by_hh_size(lissy_files)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

## Value

A list of tibbles with the modified 'hwgt' variable.
