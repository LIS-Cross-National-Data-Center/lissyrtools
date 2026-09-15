
#' Get database-specific configuration for lissyuse internals
#' 
#' @param database A character value. One of "lis" (default), "lws", or "lcs".
#' 
#' @return A list.
#' 
#' @keywords internal
get_database_config <- function(database) {
  
  assertthat::assert_that(database %in% c("lis", "lws", "lcs"),
                          msg = glue::glue("'database' must be one of 'lis', 'lws', or 'lcs'. Got '{database}' instead."))
  
  switch(database,
         lis = list(
           letter              = "i",
           household_variables = lissyrtools::lis_household_variables,
           person_variables    = lissyrtools::lis_person_variables,
           both_hp_variables   = lissyrtools::lis_both_hp_variables,
           all_variables       = lissyrtools::lis_variables,
           key_vars_household  = lissyrtools::key_vars_household_lis,
           key_vars_person     = lissyrtools::key_vars_person_lis,
           join_by             = c("hid")
         ),
         lws = list(
           letter              = "w",
           household_variables = lissyrtools::lws_household_variables,
           person_variables    = lissyrtools::lws_person_variables,
           both_hp_variables   = lissyrtools::lws_both_hp_variables,
           all_variables       = lissyrtools::lws_variables,
           key_vars_household  = lissyrtools::key_vars_household_lws,
           key_vars_person     = lissyrtools::key_vars_person_lws,
           join_by             = c("hid", "inum")
         ),
         lcs = list(
           letter              = "c",
           household_variables = lissyrtools::lcs_household_variables,
           person_variables    = lissyrtools::lcs_person_variables,
           both_hp_variables   = lissyrtools::lcs_both_hp_variables,
           all_variables       = lissyrtools::lcs_variables,
           key_vars_household  = lissyrtools::key_vars_household_lcs,
           key_vars_person     = lissyrtools::key_vars_person_lcs,
           join_by             = c("hid")
         )
  )
}