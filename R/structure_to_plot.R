#' Transform Structured Data Lists into a Tidy Data Frame for Plotting
#' 
#' This function takes a nested list of data (with up to three possible structures)
#' and transforms it into a tidy `data.frame` suitable for visualization in LISSY.
#' It supports three structures, typically outputs by weighted summary functions in lissyrtools such as: 
#' `run_weighted_mean()`, `run_weighted_percentiles()`, and `run_weighted_count()`.
#' 
#' @param data_list A named list containing the data to be transformed. The structure and naming
#'   conventions of this list determine how the data is processed.
#'   
#'   - **1st structure:** List with country names as keys and named vectors of year-values.
#'     Expected when `names(data_list)` are country codes matching `get_countries_lis()` or `get_countries_lws()`.
#'     
#'   - **2nd structure:** List with `ccyy` abbreviations as keys, and named numeric/integer vectors, where names represent categories.
#'     Typically output from functions grouped by a categorical variable, e.g. `"educ"`.
#'     
#'   - **3rd structure:** List with `ccyy` abbreviations as keys, each containing sublists of named vectors.
#'     Represents more complex summaries with multiple grouping variables, percentiles, or shares.
#' 
#' @return A tidy `data.frame` with the following columns (depending on input structure):
#'   - `cname`: Country name.
#'   - `year`: Year.
#'   - `dname`: Dataset country-year code in the `ccyy` form.
#'   - `category`: (2nd and 3rd structure) Grouping variable categories.
#'   - `share` / `percentile` / `by_var`: (3rd structure) Variable describing the vector names.
#'   - `value`: Numeric values from the input list.
#' 
#' @details
#' Depending on the summary statistics computed before, the function renames one of the columns in the 3rd structure based on pattern matching:
#'   - If the values contains "%" and "-", the column is renamed to `share`.
#'   - If the values contains "%", renamed to `percentile`.
#'   - Otherwise, renamed to `by_var`.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' library(ggplot2)
#' library(lissyrtools)
#' library(RColorBrewer)
#' library(ggthemes)
#' library(purrr)
#' library(forcats)
#' 
#' data <- lissyrtools::lissyuse(data = c("es", "de"), vars = c("dhi", "educ", "pi11", "rural"), from = 2016)
#' 
#' # Example usage for 1st structure
#' weighted_means <- run_weighted_mean(data, "pi11")
#' df1 <- structure_to_plot(weighted_means)
#' 
#' # Example usage for 2nd structure
#' weighted_means_educ <- run_weighted_mean(data, "pi11", by = "educ")
#' df2 <- structure_to_plot(weighted_means_educ)
#' 
#' # Example usage for 3rd structure
#' weighted_percentiles <- run_weighted_percentiles(data, "pi11", by = "educ")
#' df3 <- structure_to_plot(weighted_percentiles)
#' 
#' # Example usage for 3rd structure but with shares
#' weighted_percentiles <- run_weighted_percentiles(data, "pi11", by = "educ", share = TRUE)
#' df3 <- structure_to_plot(weighted_percentiles)
#' 
#' 
#' 
#' # Chart example: Plotting education group shares over years by country
#'  
#' 
#' run_weighted_count(data, "educ", percent = TRUE, na.rm = TRUE) %>%
#'   structure_to_plot() %>%
#'   ggplot(aes(x = year, y = value, color = cname, group = interaction(cname, category))) +
#'   geom_line(linewidth = 1.2) +
#'   geom_point(size = 0.6) +
#'   scale_color_stata() +
#'   labs(
#'     x = "Year",
#'     y = "Share of Education Group (%)",
#'     color = "Country"
#'   ) + 
#'   expand_limits(y = 0) + 
#'   facet_grid(~fct_relevel(category, 'low', 'medium', 'high')) +
#'   theme_bw() +
#'   theme(axis.text.x = element_text(angle = 25, hjust = 1))
#'   
#' 
#' # Another example: plotting weighted mean of 'dhi' over years by country
#' data %>%
#'   purrr::map(~ .x %>% filter(relation == 1000) %>% mutate(new_wgt = nhhmem * hwgt)) %>%
#'   apply_ppp_adjustment("dhi", "lis", "lisppp") %>%
#'   run_weighted_mean("dhi", "new_wgt") %>%
#'   structure_to_plot() %>%
#'   ggplot(aes(x = year, y = value, color = cname, group = cname)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(
#'     title = "dhi trend",
#'     caption = "Source: Luxembourg Income Study"
#'   ) +
#'   scale_color_stata() +
#'   scale_y_continuous(labels = scales::comma) +
#'   theme_minimal() +
#'   theme(axis.text.x = element_text(angle = 25, hjust = 1))
#' }
structure_to_plot <- function(data_list) {
  
  # data <- lissyrtools::lissyuse(data = c("es", "de"), vars = c("dhi", "educ", "pi11", "rural"),from = 2016)
  #
  # 1st structure: 
  #    - run_weighted_mean(data, "pi11") 
  #    
  #    - run_weighted_percentiles(data, "dhi" , probs = 0.5)
  # 
  # 
  # 2nd structure: 
  #    - run_weighted_mean(data, "pi11", by = "educ") 
  #    
  #    - run_weighted_percentiles(data, "dhi" , probs = 0.5, share = TRUE)
  #    - run_weighted_percentiles(data, "dhi" , probs = seq(0.1, 0.9, 0.1))
  #   
  #    - run_weighted_count(data, "educ", na.rm = TRUE)
  #    - run_weighted_count(data, "educ", na.rm = TRUE, percent = TRUE)
  #    
  #    
  # 3rd structure: 
  #    - run_weighted_percentiles(data, "pi11", by = "educ")
  #    - run_weighted_percentiles(data, "pi11", by = "educ", share = TRUE)
  #     
  #    - run_weighted_count(data, "educ", by = "rural", na.rm = TRUE)
  #    - run_weighted_count(data, "educ", by = "rural", na.rm = TRUE, percent = TRUE)

  
  # 1st structure
  if (all(names(data_list) %in% c(names(get_countries_lis()), names(get_countries_lws())))) {
    
    result_df <- list_rbind(purrr::imap(
      data_list ,
      ~ tibble::enframe(.x, name = "year", value = "value") %>% mutate(cname = .y)
    )) %>%
      mutate(cc = get_countries_lis()[cname],
             yy = stringr::str_sub(year, 3, 4),
             dname = paste0(cc, yy),
             year = as.integer(year)) %>% 
      select(cname, year, dname, value)
    
  # 2nd structure  
  } else if (
    all(length(names(data_list) == 4)) && 
    all(purrr::map_chr(data_list, ~ class(.x)[1]) %in% c("numeric", "integer"))
    ) {
    
    result_df <- list_rbind(purrr::imap(
      data_list ,
      ~ tibble::enframe(.x, name = "category", value = "value") %>% mutate(dname = .y)
    )) %>%
      mutate(cname = ccyy_to_cname(dname), year =  ccyy_to_yyyy(dname),
             category = stringr::str_remove(category, "^\\[\\d+\\]"),
             year = as.integer(year)) %>% 
      select(cname, year, dname, category, value)
   
  # 3rd structure     
  } else if (all(length(names(data_list) == 4) && all(purrr::map_chr(data_list, ~ class(.x)[1]) == "list"))) {
    
    result_df <- list_rbind(purrr::imap(data_list, ~ {
      outer_name <- .y
      list_rbind(purrr::imap(.x, function(sublist, subgroup) { # subgroup would be the categorical variable in run_weighted_mean or run_weighted_percentiles 
        tibble::enframe(sublist, name = "vector_names", value = "value") %>% # vector_names: could be percentiles, shares, or the by var in run_weighted_count
          mutate(dname = outer_name,
                 category = stringr::str_remove(subgroup, "^\\[\\d+\\]"),
                 name = stringr::str_remove(vector_names, "^\\[\\d+\\]"))
      }))
    })) %>%
      mutate(cname = ccyy_to_cname(dname), year =  as.integer(ccyy_to_yyyy(dname))) %>% 
      select(cname, year, dname, category, name, value)
    
    
    # Rename the `name` column accordingly, specifically in the 3rd structure
    first_value <- result_df$name[1] 
    
    if (stringr::str_detect(first_value, "%") &  stringr::str_detect(first_value, "-")) {
      names(result_df)[names(result_df) == "name"] <- "share"
    } else if (stringr::str_detect(first_value, "%")) {
      names(result_df)[names(result_df) == "name"] <- "percentile" 
    } else {
      names(result_df)[names(result_df) == "name"] <- "by_var" 
    }
    
  }
  
  cat("The resulting data frame's column names are:\n")
  print(names(result_df))
  return(result_df)

}









