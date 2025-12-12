# Load the datasets for lissyuse, and automatically merge h and p level files based on the variables.

Internal function to load datasets from specified files. Depending on
the variable criteria, it loads household-level, person-level, or both
datasets. If both are loaded, it merges them; otherwise, it returns the
single dataset. The merge stage may be skipped if only one dataset type
is required.

## Usage

``` r
variable_selection_for_lissyuse(
  data_to_load,
  path_to_files,
  vars = NULL,
  lws = FALSE
)
```

## Arguments

- data_to_load:

  Character vector of dataset names to load.

- path_to_files:

  File path where datasets are stored.

- vars:

  Optional character vector of variable names to select. If `NULL`, all
  variables are considered.

- lws:

  Logical.

## Value

A list containing the data frames selected.
