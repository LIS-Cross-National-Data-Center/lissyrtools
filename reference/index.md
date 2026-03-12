# Package index

## Import Data

This variable-driven function automatically merges household- and
person-level files from the same dataset. It also supports loading data
across multiple countries and a selected range of years.

- [`lissyuse()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/lissyuse.md)
  : Load data easily and efficiently with lissyuse

## Adjustment & Equivalisation

Applies a transformation to the variable under analysis across datasets.

- [`apply_iqr_top_bottom_coding()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_iqr_top_bottom_coding.md)
  : Apply IQR-Based Top and Bottom Coding to LIS/LWS Variables
- [`apply_oecd_equivalisation()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_oecd_equivalisation.md)
  : Apply OECD Equivalence Scale to a Variable
- [`apply_sqrt_equivalisation()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_sqrt_equivalisation.md)
  : Apply Square-Root Equivalisation to a Variable
- [`apply_ppp_adjustment()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/apply_ppp_adjustment.md)
  : Adjust Monetary Variables for Inflation and PPP

## Weighted Statistics

Computes aggregate indicators across datasets using a list structure.
These functions also allow the inclusion of survey weights in the
calculations.

- [`run_weighted_count()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_count.md)
  : Compute (weighted) counts or percentages from a list of data frames
- [`run_weighted_mean()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_mean.md)
  : Compute Weighted Mean Across a List of Data Frames (with optional
  grouping)
- [`run_weighted_percentiles()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_percentiles.md)
  : Compute Weighted Percentiles cross a List of Data Frames (with
  optional grouping)
- [`run_weighted_ratios()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_ratios.md)
  : Compute Percentile Ratios
- [`run_weighted_gini()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_gini.md)
  : Compute Weighted Gini Index Across a List of Data Frames
- [`run_weighted_atkinson()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_atkinson.md)
  : Compute Weighted Atkinson Across a List of Data Frames
- [`run_weighted_absolute_poverty()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_absolute_poverty.md)
  : Compute Absolute Poverty Rate
- [`run_weighted_relative_poverty()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_relative_poverty.md)
  : Compute Relative Poverty Rate
- [`run_weighted_poverty_shortfall()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_poverty_shortfall.md)
  : Compute the Weighted Poverty Gap (Shortfall)
- [`run_weighted_poverty_gap_index()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_poverty_gap_index.md)
  : Compute the Poverty Gap Index

## Plotting

Function designed to restructure data from a list to be printed in the
console to a tabular data frame allowing to plot the values

- [`structure_to_plot()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/structure_to_plot.md)
  : Transform Structured Data Lists into a Tidy Data Frame for Plotting

## Data Availability

Provide a quick overview of available countries and years in LIS and
LWS, including the surveys behind each dataset.

- [`get_countries_lis()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_countries_lis.md)
  : Print all the country code in LIS
- [`get_countries_lws()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_countries_lws.md)
  : Print all the country code in LWS
- [`get_years_lis()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_years_lis.md)
  : Print all the existing years in LIS for a given country.
- [`get_years_lws()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_years_lws.md)
  : Print all the existing years in LWS for a given country.
- [`get_surveys_lis()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_surveys_lis.md)
  : Print the survey used to construct the LIS datasets for a given
  country.
- [`get_surveys_lws()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_surveys_lws.md)
  : Print the survey used to construct the LWS datasets for a given
  country.

## Variable Information

Helps users check if a variable exists in a dataset and reveals extra
information such as notes, labels, and categories—especially important
for variables that vary by country.

- [`variable_labels()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/variable_labels.md)
  : Inspect the Labels of LIS and LWS variables
- [`variable_country_specific_categories()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/variable_country_specific_categories.md)
  : Retrieve the categories of a country-specific variable in LIS/LWS
  for a given country.
- [`variable_exists()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/variable_exists.md)
  : Inspect whether a given variable contains more than just non-missing
  or non-zero values for a selected group of countries in the LIS or LWS
  databases.
- [`variable_has_note()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/variable_has_note.md)
  : Verify if a given variable has a note, for a selected group of
  countries, in LIS or LWS databases.

## Built in Datasets

### Sample Datasets

Simulated datasets that mirror the structure of real LIS datasets. These
allow users to test and debug code locally, with the benefit of
autocompletion and the ability to inspect data frames interactively.

- [`it14_h_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/it14_h_lis.md)
  : Sample Household-Level LIS Dataset - Italy 2014
- [`it14_p_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/it14_p_lis.md)
  : Sample Individual-Level LIS Dataset - Italy 2014
- [`it16_h_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/it16_h_lis.md)
  : Sample Household-Level LIS Dataset - Italy 2016
- [`it16_p_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/it16_p_lis.md)
  : Sample Individual-Level LIS Dataset - Italy 2016
- [`it20_h_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/it20_h_lis.md)
  : Sample Household-Level LIS Dataset - Italy 2020
- [`it20_p_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/it20_p_lis.md)
  : Sample Individual-Level LIS Dataset - Italy 2020
- [`mx14_h_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/mx14_h_lis.md)
  : Sample Household-Level LIS Dataset - Mexico 2014
- [`mx14_p_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/mx14_p_lis.md)
  : Sample Individual-Level LIS Dataset - Mexico 2014
- [`mx16_h_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/mx16_h_lis.md)
  : Sample Household-Level LIS Dataset - Mexico 2016
- [`mx16_p_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/mx16_p_lis.md)
  : Sample Individual-Level LIS Dataset - Mexico 2016
- [`mx18_h_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/mx18_h_lis.md)
  : Sample Household-Level LIS Dataset - Mexico 2018
- [`mx18_p_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/mx18_p_lis.md)
  : Sample Individual-Level LIS Dataset - Mexico 2018
- [`us14_h_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/us14_h_lis.md)
  : Sample Household-Level LIS Dataset - US 2014
- [`us14_p_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/us14_p_lis.md)
  : Sample Individual-Level LIS Dataset - US 2014
- [`us16_h_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/us16_h_lis.md)
  : Sample Household-Level LIS Dataset - US 2016
- [`us16_p_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/us16_p_lis.md)
  : Sample Individual-Level LIS Dataset - US 2016
- [`us18_h_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/us18_h_lis.md)
  : Sample Household-Level LIS Dataset - US 2018
- [`us18_p_lis`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/us18_p_lis.md)
  : Sample Individual-Level LIS Dataset - US 2018
- [`it14_h_lws`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/it14_h_lws.md)
  : Sample Household-Level LWS Dataset - Italy 2014
- [`it14_p_lws`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/it14_p_lws.md)
  : Sample Individual-Level LWS Dataset - Italy 2014
- [`it16_h_lws`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/it16_h_lws.md)
  : Sample Household-Level LWS Dataset - Italy 2016
- [`it16_p_lws`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/it16_p_lws.md)
  : Sample Individual-Level LWS Dataset - Italy 2016
- [`us16_h_lws`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/us16_h_lws.md)
  : Sample Household-Level LWS Dataset - US 2016
- [`us16_p_lws`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/us16_p_lws.md)
  : Sample Individual-Level LWS Dataset - US 2016
- [`us19_h_lws`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/us19_h_lws.md)
  : Sample Household-Level LWS Dataset - US 2019
- [`us19_p_lws`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/us19_p_lws.md)
  : Sample Individual-Level LWS Dataset - US 2019

### Reference Objects

Character vectors containing the names of the variables available in LIS
and LWS.

- [`lis_variables`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/lis_variables.md)
  : LIS Variables
- [`lws_variables`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/lws_variables.md)
  : LWS Variables

### Complementary Tables

- [`data_inc_ref_year`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/data_inc_ref_year.md)
  : LWS income reference years
- [`data_vars_labels`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/data_vars_labels.md)
  : Variable labels
- [`data_with_warnings`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/data_with_warnings.md)
  : Variable notes
- [`datasets`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/datasets.md)
  : LIS and LWS datasets
- [`deflators`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/deflators.md)
  : CPI and PPP deflators.
- [`missing_or_zero_vars_all`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/missing_or_zero_vars_all.md)
  : Variable Status Data Frame
- [`metis_countries_df`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/metis_countries_df.md)
  : Countries Names and ISO Codes.
- [`value_label_c_data`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/value_label_c_data.md)
  : Value Labels for Country-Specific Variables in LIS and LWS Datasets

## Check Version

- [`check_github_version()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/check_github_version.md)
  : Retrieve the current version of lissyrtools from the GitHub
  repository

## Superseded

Deprecated functions from the early stages of the lissyrtools package.
Maintained for reference and backward compatibility.

- [`bind_lissy_files()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/bind_lissy_files.md)
  **\[deprecated\]** : Bind multiple LIS or LWS datasets
- [`compute_atkinson()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/compute_atkinson.md)
  **\[superseded\]** : Compute Atkinson index.
- [`compute_gini()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/compute_gini.md)
  **\[superseded\]** : Compute gini index.
- [`compute_mean()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/compute_mean.md)
  [`compute_median()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/compute_mean.md)
  [`compute_ratio()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/compute_mean.md)
  **\[superseded\]** : Compute standard indicators.
- [`compute_percentiles()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/compute_percentiles.md)
  **\[superseded\]** : Compute percentiles.
- [`compute_poverty_rate()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/compute_poverty_rate.md)
  **\[superseded\]** : Compute poverty rate.
- [`determine_file_level()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/determine_file_level.md)
  **\[deprecated\]** : Determine the level of the file
- [`determine_weight()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/determine_weight.md)
  **\[deprecated\]** : Determine the name of the weight variable
- [`lissy_map()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/lissy_map.md)
  **\[deprecated\]** : Wrap over 'purrr::imap()' for lissy files
- [`merge_dataset_levels()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/merge_dataset_levels.md)
  **\[deprecated\]** : Merge household and person-level files
- [`plot_indicator()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/plot_indicator.md)
  **\[deprecated\]** : Plot an Indicator.
- [`print_all_lissy_files()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/print_all_lissy_files.md)
  **\[superseded\]** : Print all available files
- [`print_atkinson()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/print_atkinson.md)
  **\[superseded\]** : Print the Atkinson Index
- [`print_gini()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/print_gini.md)
  **\[superseded\]** : Print the Gini Coefficient
- [`print_indicator()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/print_indicator.md)
  **\[superseded\]** : Print an indicator
- [`print_percentiles()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/print_percentiles.md)
  **\[superseded\]** : Print percentiles.
- [`print_ratio()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/print_ratio.md)
  **\[superseded\]** : Print the Ratio Index
- [`read_lissy_files()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/read_lissy_files.md)
  **\[superseded\]** : Read files in LISSY
- [`read_lissy_files_locally()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/read_lissy_files_locally.md)
  **\[superseded\]** : Read files locally
- [`read_rename_files()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/read_rename_files.md)
  **\[superseded\]** : Rename read files
- [`transform_adjust_by_lisppp()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_adjust_by_lisppp.md)
  **\[superseded\]** : Adjust aggregates by LIS PPPs
- [`transform_equivalise()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_equivalise.md)
  **\[superseded\]** : Equivalise by number of household members.
- [`transform_equivalise_oecd()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_equivalise_oecd.md)
  **\[superseded\]** : Equivalise with the OECD scale
- [`transform_zeros_to_na()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_zeros_to_na.md)
  **\[superseded\]** : Recodes zeros into missing values
- [`transform_false_zeros_to_na()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_false_zeros_to_na.md)
  **\[superseded\]** : Recode zeros into missing values if all values
  are zero
- [`transform_negative_values_to_zero()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_negative_values_to_zero.md)
  **\[superseded\]** : Recode negative values to zero
- [`transform_filter_age()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_filter_age.md)
  **\[deprecated\]** : Filter a sample by age
- [`transform_restrict_age()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_restrict_age.md)
  **\[deprecated\]** : Filter a variable by age
- [`transform_restrict_to_household_heads()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_restrict_to_household_heads.md)
  **\[deprecated\]** : Filter a variable for household heads
- [`transform_top_code_with_iqr()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_top_code_with_iqr.md)
  [`transform_bottom_code_with_iqr()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_top_code_with_iqr.md)
  **\[superseded\]** : Apply top or bottom coding with log IQR
- [`transform_weight_by_hh_size()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/transform_weight_by_hh_size.md)
  **\[superseded\]** : Multiply by household size
