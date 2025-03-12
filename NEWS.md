# lissyrtools (beta version)

# lissyrtools 0.1.10

## Introduction of lissyuse()

-   A new and more efficient method for loading data is now available through `lissyuse()`. The previous approach, which utilized `read_lissy_files()` and `merge_dataset_levels()`, will be deprecated moving forward.

## Inclusion of sample files

-   Inclusion of built-in data frames containing sample files from LIS to assist users in developing LISSY code scripts in local environments.

## New built-in objects

-   These include the `datasets` data frame, as well as vector objects containing variables categorized into specific groups (e.g., `key_vars_household_lis`, `lis_weight_variables`, `lws_wealth_categorical_variables`, `lis_country_specific_variables`).

## New "show\_" functions

-   Four new functions have been added to print the ISO2 codes for each country in LIS and LWS, as well as the available years for a given country in either LIS or LWS.

# lissyrtools 0.1.5

## Bug fixes

-   `transform_equivalise_oecd()` and respectively `implement_equivalise_oecd()` were now corrected for the way they classify children in the argument `value_children`. Previously this argument was dependent on LIS variable `nhhmem17`, now it uses `nhhmem13`.
