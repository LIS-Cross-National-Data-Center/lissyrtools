# Subset Datasets Based on Expression

Internal function that subsets the given datasets based on a filtering
expression. Supports both LWS and LIS datasets.

## Usage

``` r
subset_datasets(intermediate_data_to_filter, lws = FALSE, subset_expr)
```

## Arguments

- intermediate_data_to_filter:

  List of data frames to subset.

- lws:

  Logical.

- subset_expr:

  An expression used to filter the datasets.

## Value

List. A subsetted version of the input list based on the provided
expression.
