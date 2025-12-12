# Sort a list of lissy files

To be used only within 'merge_dataset_levels()'

## Usage

``` r
sort_lissy_list(list_to_sort, according_to)
```

## Arguments

- list_to_sort:

  A list with pfiles or hfiles. Names of these files should be in
  'ccyyyydl' format.

- according_to:

  A character vector with names on which the list should be sorted.
  These can be in any format that can be transformed to 'ccyyyyd'.

## Details

Sorts a list of lissy household or person-level files according to the
names passed in argument 'according_to'.
