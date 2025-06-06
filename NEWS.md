# lissyrtools

# lissyrtools 0.2.0 (2025-06-04)

## New functions

🔧 Adjustment & Equivalisation

-   `apply_iqr_top_bottom_coding()`: Applies interquartile range-based top and bottom coding.
-   `apply_oecd_equivalisation()`: Applies the OECD equivalence scale.
-   `apply_sqrt_equivalisation()`: Applies square-root equivalisation.
-   `apply_ppp_adjustment()`: Adjust monetary variables for inflation and PPP

📊 Weighted Statistics

-   `run_weighted_count()`: Computes weighted counts in absolute and percentage terms.
-   `run_weighted_mean()`: Computes weighted means.
-   `run_weighted_percentiles()`: Computes weighted percentiles or shares using type 2 or type 4 definitions.
-   `run_weighted_ratios()`: Computes percentile ratios (e.g. P90/P10).
-   `run_weighted_gini()`: Calculates the Gini coefficient.
-   `run_weighted_atkinson()`: Computes the Atkinson inequality index.
-   `run_weighted_relative_poverty()`: Measures relative poverty based on a median threshold.
-   `run_weighted_poverty_shortfall()`: Computes absolute and relative poverty shortfalls (poverty gap).
-   `run_weighted_poverty_gap_index()`: Calculates the poverty gap index.

📈 Plotting

-   `structure_to_plot()`: Transforms output into tidy data for plotting with `ggplot2`.

🌍 Country, Year, and Survey Access

-   `get_countries_lis()`: Prints available countries in LIS.
-   `get_countries_lws()`: Prints available countries in LWS.
-   `get_years_lis()`: Prints available years for each country in LIS.
-   `get_years_lws()`: Prints available years for each country in LWS.
-   `get_surveys_lis()`: Prints surveys used for each country in LIS.
-   `get_surveys_lws()`: Prints surveys used for each country in LWS.

🔍 Variable Information

-   `variable_labels()`: Retrieves variable labels.
-   `variable_country_specific_categories()`: Gets country-specific categories for such variables.
-   `variable_exists()`: Checks the years that a variable exists for a given country series.
-   `variable_has_note()`: Checks whether a variable has an associated note.

## Major changes

-   **Expanded Local Sample Data**: Increased the number of sample datasets available for local use, supporting more flexible and practical development and testing workflows.
-   **New Variable and Data Availability Utilities**: Introduced a set of functions for local use, to retrieve country/year/survey availability and variable-level metadata, centralizing access to crucial documentation and reducing cognitive fatigue by minimizing tab/window switching.
-   **Unified Percentile Computation**: A new function, `run_weighted_percentiles()`, now centralizes the computation of weighted percentiles, supporting both Type 2 and Type 4 estimation methods as described in Hyndman & Fan (1996). This ensures consistency across all functions that depend on percentile-based logic, such as `run_weighted_ratios()`, `apply_iqr_top_bottom_coding()`, and the one that compute relative poverty figures: `run_weighted_relative_poverty()`, `run_weighted_poverty_shortfall()` and `run_weighted_poverty_gap_index()`.
-   **Group-Level Analysis via `by` Argument**: `run_weighted_percentiles()`, `run_weighted_mean()` and `run_weighted_count()` now accept a `by` argument, allowing the disaggregation of results by categorical variables (e.g. region, gender) including `inum` variables in LWS datasets.
-   **Improved Output and Visualization Readiness**:
    -   **Cleaner Output Structure**: Results are now printed in a more compact and intuitive format, grouped by country and sorted by year, in attempt to improve readability.

    -   **Tidy Data for Plotting**: The new `structure_to_plot()` function standardizes lists to be printed in the console into tidy data frames, making it easy to feed directly into `ggplot2` for visualization.

## Minor changes

-   `lis_variables` and `lws_variables` are now external and documented reference objects in the form of character vectors.

-   It is no longer possible to adjust scale parameters in the `apply_oecd_equivalisation()` function.

## Lifecycle

Deprecated functions from the early stages of the lissyrtools package. Maintained for reference and backward compatibility.

-   `bind_lissy_files()`

-   `compute_*()`

-   `determine_*()`

-   `lissy_map()`

-   `merge_dataset_levels()`

-   `plot_indicator()`

-   `print_*()`

-   `read_*()`

-   `transform_*()`

# lissyrtools 0.1.11

## Change in lissyuse()

-   The output of `lissyuse()`now requires explicit assignment to a variable, whereas previously, the function automatically created and assigned a pre-named list (`lis_datasets` or `lws_datasets`) to the global environment.

# lissyrtools 0.1.10

## Introduction of lissyuse()

-   A new and more efficient method for loading data is now available through `lissyuse()`. The previous approach, which utilized `read_lissy_files()` and `merge_dataset_levels()`, will be deprecated moving forward.

## Inclusion of sample files

-   Inclusion of built-in data frames containing sample files from LIS to assist users in developing LISSY code scripts in local environments.

## New built-in objects

-   These include the `datasets` data frame, as well as vector objects containing variables categorized into specific groups (e.g., `key_vars_household_lis`, `lis_weight_variables`, `lws_wealth_categorical_variables`, `lis_country_specific_variables`).

## New "show_countries\_" and "get_years\_" functions

-   Four new functions have been added to print the ISO2 codes for each country in LIS and LWS, as well as the available years for a given country in either LIS or LWS.

# lissyrtools 0.1.5

## Bug fixes

-   `transform_equivalise_oecd()` and respectively `implement_equivalise_oecd()` were now corrected for the way they classify children in the argument `value_children`. Previously this argument was dependent on LIS variable `nhhmem17`, now it uses `nhhmem13`.
