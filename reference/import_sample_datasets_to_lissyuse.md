# Import Sample Datasets to lissyuse

Internal helper to import sample datasets into the `lissyuse`
environment.

## Usage

``` r
import_sample_datasets_to_lissyuse(data = NULL, database = "lis")
```

## Arguments

- data:

  Optional. A named list of datasets to import. If `NULL`, all the
  default set is used.

- database:

  A character value. One of "lis" (default), "lws", or "lcs".

- from:

  A numeric value representing the year (inclusive) after which the
  LIS/LWS datasets should be loaded.

- to:

  A numeric value representing the year (inclusive) up to which the
  LIS/LWS datasets should be loaded.

## Value

A list.
