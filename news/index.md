# Changelog

## lissyrtools 0.2.2 (2025-12-15)

### New functions

### Major changes

### Minor changes

- Added a new numeric argument `base_year_ppp` to
  [`apply_ppp_adjustment()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_ppp_adjustment.md).
  This argument specifies the PPP base year and must correspond to one
  of the unique values in the new `version_year` column of the
  `deflators` data frame.
- Added a new logical argument `print_columns` to
  [`structure_to_plot()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/structure_to_plot.md).

## lissyrtools 0.2.1 (2025-09-15)

### New functions

- [`run_weighted_absolute_poverty()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_absolute_poverty.md):
  Measures absolute poverty based on a fixed monetary threshold.
- [`check_github_version()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/check_github_version.md):
  Retrieves the current version of `lissyrtools` in GitHub.

### Major changes

- Added logical argument `average` to
  [`run_weighted_percentiles()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_percentiles.md).
  When set to `TRUE` it computes the weighted mean of a variable within
  each defined percentile group.

### Minor changes

- The `by` argument in
  [`run_weighted_count()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_count.md),
  [`run_weighted_mean()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_mean.md)
  and
  [`run_weighted_percentiles()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_percentiles.md)
  is now less restrictive. It accepts additional named variables beyond
  those defined in `lis_categorical_variables`,
  `lws_wealth_categorical_variables`, or `inum`.
- Applied corrections in
  [`structure_to_plot()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/structure_to_plot.md),
  standardized `dname` column values across structures, and renamed a
  column to `distribution_group` in the third structure.
- Deleted columns `lisppp`, `cpi`, and `ppp` after each use of
  [`apply_ppp_adjustment()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_ppp_adjustment.md).
  This allows the function to be used immediately afterwards, with
  another variable.  
- Added the argument `daily_poverty_line` on the following functions:
  [`run_weighted_poverty_shortfall()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_poverty_shortfall.md)
  and
  [`run_weighted_poverty_shortfall()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_poverty_shortfall.md).

## lissyrtools 0.2.0 (2025-06-04)

### New functions

🔧 Adjustment & Equivalisation

- [`apply_iqr_top_bottom_coding()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_iqr_top_bottom_coding.md):
  Applies interquartile range-based top and bottom coding.
- [`apply_oecd_equivalisation()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_oecd_equivalisation.md):
  Applies the OECD equivalence scale.
- [`apply_sqrt_equivalisation()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_sqrt_equivalisation.md):
  Applies square-root equivalisation.
- [`apply_ppp_adjustment()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_ppp_adjustment.md):
  Adjust monetary variables for inflation and PPP

📊 Weighted Statistics

- [`run_weighted_count()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_count.md):
  Computes weighted counts in absolute and percentage terms.
- [`run_weighted_mean()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_mean.md):
  Computes weighted means.
- [`run_weighted_percentiles()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_percentiles.md):
  Computes weighted percentiles or shares using type 2 or type 4
  definitions.
- [`run_weighted_ratios()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_ratios.md):
  Computes percentile ratios (e.g. P90/P10).
- [`run_weighted_gini()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_gini.md):
  Calculates the Gini coefficient.
- [`run_weighted_atkinson()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_atkinson.md):
  Computes the Atkinson inequality index.
- [`run_weighted_relative_poverty()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_relative_poverty.md):
  Measures relative poverty based on a median threshold.
- [`run_weighted_poverty_shortfall()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_poverty_shortfall.md):
  Computes absolute and relative poverty shortfalls (poverty gap).
- [`run_weighted_poverty_gap_index()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_poverty_gap_index.md):
  Calculates the poverty gap index.

📈 Plotting

- [`structure_to_plot()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/structure_to_plot.md):
  Transforms output into tidy data for plotting with `ggplot2`.

🌍 Country, Year, and Survey Access

- [`get_countries_lis()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_countries_lis.md):
  Prints available countries in LIS.
- [`get_countries_lws()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_countries_lws.md):
  Prints available countries in LWS.
- [`get_years_lis()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_years_lis.md):
  Prints available years for each country in LIS.
- [`get_years_lws()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_years_lws.md):
  Prints available years for each country in LWS.
- [`get_surveys_lis()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_surveys_lis.md):
  Prints surveys used for each country in LIS.
- [`get_surveys_lws()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_surveys_lws.md):
  Prints surveys used for each country in LWS.

🔍 Variable Information

- [`variable_labels()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/variable_labels.md):
  Retrieves variable labels.
- [`variable_country_specific_categories()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/variable_country_specific_categories.md):
  Gets country-specific categories for such variables.
- [`variable_exists()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/variable_exists.md):
  Checks the years that a variable exists for a given country series.
- [`variable_has_note()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/variable_has_note.md):
  Checks whether a variable has an associated note.

### Major changes

- **Expanded Local Sample Data**: Increased the number of sample
  datasets available for local use, supporting more flexible and
  practical development and testing workflows.
- **New Variable and Data Availability Utilities**: Introduced a set of
  functions for local use, to retrieve country/year/survey availability
  and variable-level metadata, centralizing access to crucial
  documentation and reducing cognitive fatigue by minimizing tab/window
  switching.
- **Unified Percentile Computation**: A new function,
  [`run_weighted_percentiles()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_percentiles.md),
  now centralizes the computation of weighted percentiles, supporting
  both Type 2 and Type 4 estimation methods as described in Hyndman &
  Fan (1996). This ensures consistency across all functions that depend
  on percentile-based logic, such as
  [`run_weighted_ratios()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_ratios.md),
  [`apply_iqr_top_bottom_coding()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_iqr_top_bottom_coding.md),
  and the one that compute relative poverty figures:
  [`run_weighted_relative_poverty()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_relative_poverty.md),
  [`run_weighted_poverty_shortfall()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_poverty_shortfall.md)
  and
  [`run_weighted_poverty_gap_index()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_poverty_gap_index.md).
- **Group-Level Analysis via `by` Argument**:
  [`run_weighted_percentiles()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_percentiles.md),
  [`run_weighted_mean()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_mean.md)
  and
  [`run_weighted_count()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_count.md)
  now accept a `by` argument, allowing the disaggregation of results by
  categorical variables (e.g. region, gender) including `inum` variables
  in LWS datasets.
- **Improved Output and Visualization Readiness**:
  - **Cleaner Output Structure**: Results are now printed in a more
    compact and intuitive format, grouped by country and sorted by year,
    in attempt to improve readability.

  - **Tidy Data for Plotting**: The new
    [`structure_to_plot()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/structure_to_plot.md)
    function standardizes lists to be printed in the console into tidy
    data frames, making it easy to feed directly into `ggplot2` for
    visualization.

### Minor changes

- `lis_variables` and `lws_variables` are now external and documented
  reference objects in the form of character vectors.

- It is no longer possible to adjust scale parameters in the
  [`apply_oecd_equivalisation()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_oecd_equivalisation.md)
  function.

### Lifecycle

Deprecated functions from the early stages of the lissyrtools package.
Maintained for reference and backward compatibility.

- [`bind_lissy_files()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/bind_lissy_files.md)

- `compute_*()`

- `determine_*()`

- [`lissy_map()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/lissy_map.md)

- [`merge_dataset_levels()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/merge_dataset_levels.md)

- [`plot_indicator()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/plot_indicator.md)

- `print_*()`

- `read_*()`

- `transform_*()`

## lissyrtools 0.1.11

### Change in lissyuse()

- The output of
  [`lissyuse()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/lissyuse.md)now
  requires explicit assignment to a variable, whereas previously, the
  function automatically created and assigned a pre-named list
  (`lis_datasets` or `lws_datasets`) to the global environment.

## lissyrtools 0.1.10

### Introduction of lissyuse()

- A new and more efficient method for loading data is now available
  through
  [`lissyuse()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/lissyuse.md).
  The previous approach, which utilized
  [`read_lissy_files()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/read_lissy_files.md)
  and
  [`merge_dataset_levels()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/merge_dataset_levels.md),
  will be deprecated moving forward.

### Inclusion of sample files

- Inclusion of built-in data frames containing sample files from LIS to
  assist users in developing LISSY code scripts in local environments.

### New built-in objects

- These include the `datasets` data frame, as well as vector objects
  containing variables categorized into specific groups (e.g.,
  `key_vars_household_lis`, `lis_weight_variables`,
  `lws_wealth_categorical_variables`, `lis_country_specific_variables`).

### New “show_countries\_” and “get_years\_” functions

- Four new functions have been added to print the ISO2 codes for each
  country in LIS and LWS, as well as the available years for a given
  country in either LIS or LWS.

## lissyrtools 0.1.5

### Bug fixes

- [`transform_equivalise_oecd()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_equivalise_oecd.md)
  and respectively
  [`implement_equivalise_oecd()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/implement_equivalise_oecd.md)
  were now corrected for the way they classify children in the argument
  `value_children`. Previously this argument was dependent on LIS
  variable `nhhmem17`, now it uses `nhhmem13`.
