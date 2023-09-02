library(lissyrtools)
context("Functions from 'utils.R'")


# intervals_with_single_value ---------------------------------------------

test_that("intervals_with_single_value 1", {

  x_1 <- "(26.6000003814697,26.6000003814697]"
  x_2 <- "(39.9000015258789,39.9000015258789]"
  x_3 <- "(26.6000003814697,39.9000015258789]"

  expect_equal(intervals_with_single_value(x = x_1),
               TRUE)

  expect_equal(intervals_with_single_value(x = x_2),
               TRUE)

  expect_equal(intervals_with_single_value(x = x_3),
               FALSE)


})


test_that("intervals_with_single_value incorrect input", {

  x_4 <- "(26.6000003814697,26.6000003814697, 66.5]"
  x_5 <- "(26.6000003814697,26.6000003814697], (39.9000015258789, 66.5]"

  expect_error(intervals_with_single_value(x = x_4))

  expect_error(intervals_with_single_value(x = x_5))

})


# is.household.dataset ----------------------------------------------------

test_that("is.household.dataset returns expected outputs", {

  expect_equal(is.household.dataset("ca2009h"), TRUE)
  expect_equal(is.household.dataset("ca09h"), TRUE)
  expect_equal(is.household.dataset("ca2009p"), FALSE)
  expect_equal(is.household.dataset("ca09p"), FALSE)
  expect_equal(is.household.dataset("ca2009"), FALSE)
  expect_equal(is.household.dataset("ca09"), FALSE)
  expect_equal(is.household.dataset("ca09ih"), TRUE)
  expect_equal(is.household.dataset("ca09wh"), TRUE)

})


# define_option -----------------------------------------------------------

test_that("define_option defines an option", {

  define_option("my_option", TRUE)

  expect_equal(getOption("my_option"), TRUE)


})

test_that("define_option works well with glue::glue()", {

  country <- "ca12"
  define_option(glue::glue("my_option_{country}"), TRUE)

  expect_equal(getOption("my_option_ca12"), TRUE)


})




# change_file_name_format -------------------------------------------------

test_that("change_file_name_format 1", {

  expect_equal(change_file_name_format("ca2016ih", "ccyydl"), "ca16ih")

  expect_equal(change_file_name_format("ca2016ih", "ccyyl"), "ca16h")

  expect_equal(change_file_name_format("ca2016ih", "ccyyd"), "ca16i")

  expect_equal(change_file_name_format("ca2016ih", "ccyy"), "ca16")

  expect_equal(change_file_name_format("fr1984i", "ccyyyy"), "fr1984")

  expect_equal(change_file_name_format("fr1984h", "ccyyyy"), "fr1984")

  expect_equal(change_file_name_format("fr1984h", "ccyyl"), "fr84h")

  expect_equal(change_file_name_format("fr1984", "ccyy"), "fr84")

  expect_equal(change_file_name_format("fr84", "ccyyyy"), "fr1984")

  expect_equal(change_file_name_format("fr84ih", "ccyyyydl"), "fr1984ih")

})



test_that("change_file_name_format returns the same output if the inputed file name is already in the requested format", {

  expect_equal(change_file_name_format("ca2016ih", "ccyyyydl"), "ca2016ih")

})



test_that("change_file_name_format throws an error if change is not possible", {

  expect_error(change_file_name_format("ca2016", "ccyydl"))

  expect_error(change_file_name_format("ca2016", "ccyyd"))

  expect_error(change_file_name_format("ca2016", "ccyyl"))


})

test_that("change_file_name_format throws an error if the requested format is not correct", {

  expect_error(change_file_name_format("ca2016", "ccyyyyydl"))

  expect_error(change_file_name_format("ca2016", "cccyyd"))

  expect_error(change_file_name_format("ca2016", "ccyyll"))


})



test_that("change_file_name_format works with vectors of multiple file names - input format is the same", {

  expect_equal(change_file_name_format(c("ca2016ih", "au2010ih"), "ccyydl"),
               c("ca16ih", "au10ih"))

  expect_equal(change_file_name_format(c("ca2016ih", "au2010ih"), "ccyyl"),
               c("ca16h", "au10h"))

  expect_equal(change_file_name_format(c("ca2016ih", "au2010ih"), "ccyyd"),
               c("ca16i", "au10i"))

  expect_equal(change_file_name_format(c("ca2016ih", "au2010ih"), "ccyy"),
               c("ca16", "au10"))

  expect_equal(change_file_name_format(c("fr1984i", "ca2016ih"), "ccyyyy"),
               c("fr1984", "ca2016"))

  expect_equal(change_file_name_format(c("fr1984h", "ca2016h"), "ccyyyy"),
               c("fr1984", "ca2016"))

  expect_equal(change_file_name_format(c("fr1984h", "ca2016h"), "ccyyl"),
               c("fr84h", "ca16h"))

  expect_equal(change_file_name_format(c("fr1984", "ca2016"), "ccyy"),
               c("fr84", "ca16"))

  expect_equal(change_file_name_format(c("fr84", "ca16"), "ccyyyy"),
              c("fr1984", "ca2016"))

  expect_equal(change_file_name_format(c("fr84ih", "ca16ih"), "ccyyyydl"),
               c("fr1984ih", "ca2016ih"))


})


test_that("change_file_name_format works with vectors of multiple file names - input format diverges", {

  expect_equal(change_file_name_format(c("ca2016ih", "au10ih"), "ccyydl"),
               c("ca16ih", "au10ih"))

  expect_equal(change_file_name_format(c("ca2016ih", "au10h"), "ccyyl"),
               c("ca16h", "au10h"))

  expect_equal(change_file_name_format(c("ca2016ih", "au10i"), "ccyyd"),
               c("ca16i", "au10i"))

  expect_equal(change_file_name_format(c("ca2016ih", "au10"), "ccyy"),
               c("ca16", "au10"))

})



# read_file_name_format ---------------------------------------------------

test_that("read_file_name_format reads the formats correctly", {

  expect_equal(read_file_name_format("ca2016ih"),
               "ccyyyydl")

  expect_equal(read_file_name_format("ca2016ip"),
               "ccyyyydl")

  expect_equal(read_file_name_format("ca2016wh"),
               "ccyyyydl")

  expect_equal(read_file_name_format("ca2016wp"),
               "ccyyyydl")



  expect_equal(read_file_name_format("ca16ih"),
               "ccyydl")

  expect_equal(read_file_name_format("ca16ip"),
               "ccyydl")

  expect_equal(read_file_name_format("ca16wh"),
               "ccyydl")

  expect_equal(read_file_name_format("ca16wp"),
               "ccyydl")



  expect_equal(read_file_name_format("ca2016h"),
               "ccyyyyl")

  expect_equal(read_file_name_format("ca2016p"),
               "ccyyyyl")



  expect_equal(read_file_name_format("ca16h"),
               "ccyyl")

  expect_equal(read_file_name_format("ca16p"),
               "ccyyl")



  expect_equal(read_file_name_format("ca2016"),
               "ccyyyy")

  expect_equal(read_file_name_format("ca2016"),
               "ccyyyy")



  expect_equal(read_file_name_format("ca16"),
               "ccyy")

  expect_equal(read_file_name_format("ca16"),
               "ccyy")

})


test_that("read_file_name_format fails if format is incorrect", {

  expect_error(read_file_name_format("ca6"),
               "'ca6' is not a valid file name format.",
               fixed = TRUE)

  expect_error(read_file_name_format("ca20016"),
               "'ca20016' is not a valid file name format.",
               fixed = TRUE)

})




# is_change_possible ------------------------------------------------------

test_that("is_change_possible 1", {

  expect_equal(is_change_possible("ccyyyydl", "ccyyyyd"),
               TRUE)

  expect_equal(is_change_possible("ccyydl", "ccyyyyd"),
               TRUE)

  expect_equal(is_change_possible("ccyyyyl", "ccyyyyl"),
               TRUE)

  expect_equal(is_change_possible("ccyyyyl", "ccyyyydl"),
               FALSE)

  expect_equal(is_change_possible("ccyyyyd", "ccyyl"),
               FALSE)

  expect_equal(is_change_possible("ccyyyy", "ccyyl"),
               FALSE)


})



# extract_information_from_file_name --------------------------------------

test_that("extract_information_from_file_name 1", {


  # 4 year file name
  expect_equal(extract_information_from_file_name("ca2016ih", 'ccyyyydl')[["country_"]],
               "ca")

  expect_equal(extract_information_from_file_name("ca2016ih", 'ccyyyydl')[["year_"]],
               "2016")

  expect_equal(extract_information_from_file_name("ca2016ih", 'ccyyyydl')[["database_"]],
               "i")

  expect_equal(extract_information_from_file_name("ca2016ih", 'ccyyyydl')[["level_"]],
               "h")


  # 2 year digit file name
  expect_equal(extract_information_from_file_name("ca16ih", 'ccyydl')[["country_"]],
               "ca")

  expect_equal(extract_information_from_file_name("ca16ih", 'ccyydl')[["database_"]],
               "i")

  expect_equal(extract_information_from_file_name("ca2016ih", 'ccyyyydl')[["level_"]],
               "h")


  # missing database
  expect_equal(extract_information_from_file_name("ca2016h", 'ccyyyyl')[["country_"]],
               "ca")

  expect_equal(extract_information_from_file_name("ca2016h", 'ccyyyyl')[["year_"]],
               "2016")

  expect_equal(extract_information_from_file_name("ca2016h", 'ccyyyyl')[["database_"]],
               "")

  expect_equal(extract_information_from_file_name("ca2016h", 'ccyyyyl')[["level_"]],
               "h")


  # missing level
  expect_equal(extract_information_from_file_name("ca2016w", 'ccyyyyd')[["country_"]],
               "ca")

  expect_equal(extract_information_from_file_name("ca2016w", 'ccyyyyd')[["year_"]],
               "2016")

  expect_equal(extract_information_from_file_name("ca2016w", 'ccyyyyd')[["database_"]],
               "w")

  expect_equal(extract_information_from_file_name("ca2016w", 'ccyyyyd')[["level_"]],
               "")

})


test_that("extract_information_from_file_name returns 4 digit output with two digit year input", {

  expect_equal(extract_information_from_file_name("ca16ih", 'ccyydl')[["country_"]],
               "ca")

  expect_equal(extract_information_from_file_name("ca16ih", 'ccyydl')[["year_"]],
               "2016")

})


test_that("extract_information_from_file_name throws an error if 'current_format' is incorrect", {

  # 'y' * 5
  expect_error(extract_information_from_file_name("ca16ih", 'ccyyyyydl'),
               "Argument 'current_format' needs to be one of the following: 'ccyyyydl', 'ccyydl', 'ccyyyyd', 'ccyyd', 'ccyyyyl', 'ccyyl', 'ccyyyy', 'ccyy'",
               fixed = TRUE)

  # missing 'cc'
  expect_error(extract_information_from_file_name("ca16ih", 'yyyydl'),
               "Argument 'current_format' needs to be one of the following: 'ccyyyydl', 'ccyydl', 'ccyyyyd', 'ccyyd', 'ccyyyyl', 'ccyyl', 'ccyyyy', 'ccyy'",
               fixed = TRUE)

  #  'cc' * 3
  expect_error(extract_information_from_file_name("ca16ih", 'cccyydl'),
               "Argument 'current_format' needs to be one of the following: 'ccyyyydl', 'ccyydl', 'ccyyyyd', 'ccyyd', 'ccyyyyl', 'ccyyl', 'ccyyyy', 'ccyy'",
               fixed = TRUE)

})


test_that("extract_information_from_file_name throws an error if any of the returned elements is NA", {

  expect_error(extract_information_from_file_name("ca2016w", 'ccyyyydl'),
               "Some extracted elements are NA. Please make sure that 'ca2016w' has format 'ccyyyydl'.")


  expect_error(extract_information_from_file_name("ca2016h", 'ccyyyydl'),
               "Some extracted elements are NA. Please make sure that 'ca2016h' has format 'ccyyyydl'.")

})






# return_desired_format ---------------------------------------------------


test_that("return_desired_format 1", {

  expect_equal(return_desired_format(country = "ca",
                                     year = "2016",
                                     database = "i",
                                     level = "h",
                                     to_format = "ccyyyydl"), "ca2016ih")

  expect_equal(return_desired_format(country = "ca",
                                     year = "2016",
                                     database = "i",
                                     level = "h",
                                     to_format = "ccyyyy"), "ca2016")


  expect_equal(return_desired_format(country = "ca",
                                     year = "2016",
                                     database = "",
                                     level = "",
                                     to_format = "ccyyyy"), "ca2016")


  expect_equal(return_desired_format(country = "ca",
                                     year = "2016",
                                     database = "",
                                     level = "",
                                     to_format = "ccyy"), "ca16")


})



# is.unique ---------------------------------------------------------------




# is.all.same.value -------------------------------------------------------

test_that("is.all.same.value works as expected", {

  expect_equal(is.all.same.value(c(1,1,1,1,1)), TRUE)

  expect_equal(is.all.same.value(rep("a", 5)), TRUE)

  expect_equal(is.all.same.value(c(1,1,1,1,2)), FALSE)

})


test_that("is.all.same.value fails if x is null or has length 0", {

  expect_error(is.all.same.value(NULL))

  expect_error(is.all.same.value(vector(length = 0)))

})


test_that("is.all.same.value does not ignore NAs if na.rm = FALSE", {

  expect_error(is.all.same.value(c(1,1,1,1,1, NA)),
               "'x' can not have NAs if 'na.rm = FALSE'")

  expect_equal(is.all.same.value(c(1,1,1,1,1, NA), na.rm = TRUE), TRUE)

})


test_that("is.all.same.value deals correctly with floating point error", {

  expect_equal(is.all.same.value(c(sqrt(2)^2, 2)), TRUE)

})


test_that("is.all.same.value uses tests of previous function: is.unique 1", {

  expect_equal(is.all.same.value(1),
               TRUE)

  expect_equal(is.all.same.value(c(1, 1, 1)),
               TRUE)

  expect_equal(is.all.same.value(c(1, 1, c(1, 1) )),
               TRUE)

  expect_equal(is.all.same.value(c(0, 1, 1)),
               FALSE)


})

test_that("is.all.same.value uses tests of previous function: is.unique throws error if argument is not a vector", {

  expect_error(is.all.same.value(list(a = c(1, 1, 1))))

  expect_error(is.all.same.value(data.frame(a = c(1, 1, 1))))


})


test_that("is.all.same.value works well with factors",{

  expect_equal(
    is.all.same.value(structure(c(2L, 2L, 2L),
                                .Label = c("[0]Historical Wave",
                                           "[1]Wave I", "[2]Wave II"),
                                class = "factor")),
    TRUE
  )

  expect_equal(
    is.all.same.value(structure(c(1L, 2L, 2L),
                                .Label = c("[0]Historical Wave",
                                           "[1]Wave I", "[2]Wave II"),
                                class = "factor")),
    FALSE
  )

})




# four_digit_year ---------------------------------------------------------

test_that("four_digit_year works as expected", {

  expect_equal(four_digit_year("10"), "2010")

  expect_equal(four_digit_year("70"), "1970")

  expect_equal(four_digit_year(70), "1970")

  expect_error(four_digit_year("lu2010ih"),
               "Argument 'two_digit_year' should be two digits (00 to 99).",
               fixed = TRUE)

  expect_error(four_digit_year("lu"),
               "Argument 'two_digit_year' should be two digits (00 to 99).",
               fixed = TRUE)


})



# database_character_to_name ----------------------------------------------

test_that("database_character_to_name works as expected", {

  expect_equal(database_character_to_name("i"),
               "LIS")

  expect_equal(database_character_to_name("w"),
               "LWS")

  expect_equal(database_character_to_name("e"),
               "ERFLIS")

})




# get_database ------------------------------------------------------------

test_that("get_database works as expected", {

  list_1 <- list(aa11ih = tibble::tibble(),
                 bb22ih = tibble::tibble())

  list_2 <- list_1

  list_3 <- list_1

  list_4 <- list_1

  attr(list_1, "database") <- "i"

  attr(list_2, "database") <- "b"

  attr(list_4, "database") <- c("i", "i")

  expect_equal(get_database(list_1), "i")

  expect_error(get_database(list_2),
               "Only 'lis', 'lws', 'erflis', 'i', 'w' and 'e' are valid values for databases. Got 'b'")

  expect_error(get_database(list_3),
               "Attribute 'database' is NULL")

  expect_error(get_database(list_4),
               "Attribute 'database' must have length 1.",
               fixed = TRUE)


})



# get_index_hh_heads ------------------------------------------------------
#
# test_that("get_index_hh_heads works as expected on data from readstata13", {
#
#   zz55ip <- structure(list(hid = c(1L, 2L, 3L, 3L, 4L),
#                  pid = c(1L, 1L, 1L, 2L, 1L),
#                  relation = structure(c(1L, 1L, 1L, 3L, 1L),
#                                       .Label = c("[1000]head",
#                                                  "[2000]spouse/partner",
#                                                  "[2100]spouse",
#                                                  "[2200]cohabiting partner",
#                                                  "[3000]child",
#                                                  "[3100]own child (incl adopted)"),
#                                       class = "factor")),
#             row.names = c(NA, 5L),
#             class = "data.frame")
#
#   expect_equal(get_index_hh_heads(zz55ip), c(TRUE, TRUE, TRUE, FALSE, TRUE))
#
# })
#
#
# test_that("get_index_hh_heads works as expected on data from haven", {
#
#   zz55ip <- structure(list(hid = structure(c(1, 2, 3, 3, 4),
#                                  label = "household identifier",
#                                  format.stata = "%8.0g"),
#                  pid = structure(c(1, 1, 1, 2, 1),
#                                  label = "person identifier",
#                                  format.stata = "%8.0g"),
#                  relation = structure(c(1000, 1000, 1000, 2100, 1000),
#                                       label = "relationship to household head",
#                                       format.stata = "%50.0g",
#                                       labels = c(`[1000]head` = 1000, `[2000]spouse/partner` = 2000,
#                                                  `[2100]spouse` = 2100, `[2200]cohabiting partner` = 2200,
#                                                  `[3000]child` = 3000, `[3100]own child (incl adopted)` = 3100),
#                                       class = c("haven_labelled","vctrs_vctr", "double"))),
#             row.names = c(NA, -5L),
#             class = c("tbl_df", "tbl", "data.frame"))
#
#   expect_equal(get_index_hh_heads(zz55ip), c(TRUE, TRUE, TRUE, FALSE, TRUE))
#
# })
