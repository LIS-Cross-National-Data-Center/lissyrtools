# Merge household and person-level files

**\[deprecated\]**

Merges the household and person-level files of datasets.

The level of the files in 'lissy_hfiles' and 'lissy_pfiles' is
determined by the 'level' attribute of the files.

## Usage

``` r
merge_dataset_levels(lissy_hfiles, lissy_pfiles)
```

## Arguments

- lissy_hfiles:

  A list with household-level files.

- lissy_pfiles:

  A list with person-level files.

## Value

A list with the merged files.
