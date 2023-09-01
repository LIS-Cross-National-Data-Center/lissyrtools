# test-print.R

library(testthat)
library(lissyrtools)

.run_local_tests <- (Sys.info()[["effective_user"]] == "josep" && Sys.info()[["nodename"]] == "DEVV-CT01")


# print_indicator ---------------------------------------------------------

# copy tests from 'test-transformations' for
# test when variable has only NAS

# try also with single file list

# ** gini -----------------------------------------------------------------

# test_that("print_indicator uses the right weight when computing gini with a p-level file", {
#
#
#
#
# })
#
#
# test_that("print_indicator uses the right weight when computing gini with a h-level file", {
#
#
#
#
# })

# ** atkinson -------------------------------------------------------------

# test_that("print_indicator uses the right weight when computing atkinson with a p-level file", {
#
#
#
#
# })
#
#
# test_that("print_indicator uses the right weight when computing atkinson with a h-level file", {
#
#
#
#
# })


# ** percentiles ----------------------------------------------------------

# test_that("print_indicator uses the right weight when computing percentiles with a p-level file", {
#
#
#
#
# })
#
#
# test_that("print_indicator uses the right weight when computing percentiles with a h-level file", {
#
#
#
#
# })

# ** average --------------------------------------------------------------

# test_that("print_indicator uses the right weight when computing mean with a p-level file", {
#
#
#
#
# })
#
#
# test_that("print_indicator uses the right weight when computing mean with a h-level file", {
#
#
#
#
# })


# ** median ---------------------------------------------------------------


# test_that("print_indicator uses the right weight when computing median with a p-level file", {
#
#
#
#
# })
#
#
# test_that("print_indicator uses the right weight when computing median with a h-level file", {
#
#
#
#
# })




# ** relative poverty -----------------------------------------------------






# ** using p-level files -------------------------------------------------

# **** mean ---------------------------------------------------------------

# prints a warning if ratio or epsilon arguments are passed

# **** median -------------------------------------------------------------
# prints a warning if ratio or epsilon arguments are passed


# **** ratio --------------------------------------------------------------

# prints a warning if epsilon argument is passed


# **** gini ---------------------------------------------------------------

# prints a warning if ratio or epsilon arguments are passed

# **** atkinson -----------------------------------------------------------

# prints a warning if ratio argument is passed



# ** using hh-level files -------------------------------------------------


